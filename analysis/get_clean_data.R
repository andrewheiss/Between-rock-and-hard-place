library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(lubridate)
library(countrycode)
library(feather)

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
