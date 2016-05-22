library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(haven)
library(lubridate)
library(countrycode)
library(feather)
library(rvest)
library(stringr)
library(WDI)
library(GGally)

# -----------------------------------------------------------------------------
# NB: See `data/external_data.yaml` for metadata about each of these datasets
# -----------------------------------------------------------------------------

# ------------------------------------
# Christensen and Weinstein NGO laws
# ------------------------------------
# Get and load DCJW data
dcjw.url <- "https://darinchristensen.github.io/Data/DCJW_NGO_Laws.xlsx"
dcjw.name <- basename(dcjw.url)
dcjw.path <- file.path(PROJHOME, "data_raw", dcjw.name)

if (!file.exists(dcjw.path)) {
  download.file(url=dcjw.url, dcjw.path, "auto")
}

# Tidy DCJW data
dcjw <- read_excel(dcjw.path)[,1:50] %>%
  select(-c(contains("source"), contains("burden"), contains("subset"), Coder, Date)) %>%
  gather(key, value, -Country) %>%
  separate(key, c("question", "var.name"), 4) %>%
  filter(!is.na(Country)) %>%
  mutate(var.name = ifelse(var.name == "", "value", gsub("_", "", var.name))) %>%
  spread(var.name, value) %>%
  mutate(year.actual = ymd(paste0(year, "-01-01"), quiet=TRUE),
         country.name = countrycode(Country, "country.name", "country.name"))

write_feather(dcjw, file.path(PROJHOME, "data", "dcjw.feather"))


# ----------------------------
# ECOSOC consultative status
# ----------------------------
# The UN site by default checks for a login cookie that is set when first
# searching in the database. This can be overridden with the
# `sessionCheck=false` parameter. The `show=x` parameter controls how many
# entries are shown. Set this really, really high to get every possible
# organization in one page.

if (!file.exists(file.path(PROJHOME, "data", "ecosoc.feather"))) {
  ecosoc.url <- "https://esango.un.org/civilsociety/getByAllHavingStatus.do?method=getByAllHavingStatus&searchType=csSearch&show=10000&sessionCheck=false"
  
  # Scrape HTML table from site
  ecosoc.raw <- read_html(ecosoc.url) %>%
    html_nodes(xpath='//*[@id="pagedResults1"]/form/table') %>%
    html_table(header=TRUE, fill=TRUE) %>% 
    data.frame(stringsAsFactors=FALSE) %>%
    select(org.name = 1, status = 2)
  
  # Each status column follows a similar pattern: Status Year, Extra stuff
  # Extract these parts into separate columns
  ecosoc.clean <- ecosoc.raw %>%
    mutate(status = gsub("\\n|\\t", " ", status)) %>%
    extract(status, into=c("status.clean", "year", "extra"), 
            regex="(General|Special|Roster) +(\\d+)(.*)", remove=FALSE)
  
  # extract() chokes when the pattern does not include a year, so separate the
  # entries with a date from those without and parse the ones using a different
  # regex pattern
  ecosoc.with.date <- ecosoc.clean %>%
    filter(!is.na(status.clean)) %>%
    select(-status) %>%
    mutate(extra = ifelse(extra == "", NA_character_,
                          gsub(" +", ", ", str_trim(gsub(",", "", extra), side="both"))))
  
  ecosoc.no.date <- ecosoc.clean %>%
    filter(is.na(status.clean), !is.na(status), status != "Cease to exist") %>%
    select(org.name, status) %>%
    separate(status, into=c("status.clean", "extra"),
             extra="merge", fill="right") %>%
    mutate(year = NA)
  
  # Combine the dated and dateless rows into one master dataframe
  ecosoc.all <- bind_rows(ecosoc.with.date, ecosoc.no.date) %>%
    mutate(year.actual = ymd(paste0(year, "-01-01"), quiet=TRUE))

  # Save this puppy
  write_feather(ecosoc.all, file.path(PROJHOME, "data", "ecosoc.feather"))
}


# ----------------------------
# Bush:2015 replication data
# ----------------------------
# This Stata file is not currently available online. I received it via e-mail
# from Sarah Bush.
bush <- read_stata(file.path(PROJHOME, "data_raw", "Ch_5_Replication_File.dta")) %>%
  mutate(year.actual = ymd(paste0(year, "-01-01"), quiet=TRUE))

write_feather(bush, file.path(PROJHOME, "data", "bush.feather"))


# -----------
# WDI stats
# -----------
wdi.indicators <- c("SH.H2O.SAFE.ZS",  # Access to water supply
                    "SE.ADT.LITR.ZS",  # Adult literacy rate
                    "SH.STA.ACSN",  # Improved sanitation access
                    "SH.STA.MMRT",  # Maternal mortality rate
                    "SP.DYN.IMRT.IN",  # Infant mortality
                    "SI.POV.DDAY")  # Less than $2/day

wdi.raw <- WDI(country="all", wdi.indicators, extra=TRUE, start=1981, end=2016)
wdi.countries <- countrycode_data$iso2c

wdi.clean <- wdi.raw %>%
  filter(iso2c %in% wdi.countries) %>%
  select(one_of(wdi.indicators)) %>%
  rename(access.to.water = SH.H2O.SAFE.ZS,
         adult.literacy = SE.ADT.LITR.ZS,
         access.to.sanitation = SH.STA.ACSN,
         maternal.mortality = SH.STA.MMRT,
         infant.mortality = SP.DYN.IMRT.IN,
         less.than.2.dollars.day = SI.POV.DDAY)

ggplot2::theme_set(ggplot2::theme_light(9))
all.corr <- ggpairs(wdi.clean)
