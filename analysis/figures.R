library(dplyr)
library(feather)
library(ggplot2)
library(scales)

# Different types of civil society restrictions over time
dcjw <- read_feather(file.path(PROJHOME, "data", "dcjw.feather"))

restrictions <- dcjw %>%
  filter(question %in% c("q_2a", "q_3e", "q_4a"), !is.na(year)) %>%
  arrange(year.actual) %>%
  group_by(question, year.actual) %>%
  summarise(count = n()) %>%
  mutate(cumulative.count = cumsum(count)) %>%
  ungroup() %>%
  mutate(question = factor(question, levels=c("q_2a", "q_3e", "q_4a"),
                           labels=c("Registration requirements    ",
                                    "Foreign funding prohibition    ",
                                    "Advocacy restrictions")))

ggplot(restrictions, aes(x=year.actual, y=cumulative.count,
                         colour=question, linetype=question)) + 
  geom_line(size=1) + 
  coord_cartesian(xlim=ymd("1950-01-01", "2011-01-01")) +
  labs(x=NULL, y="Cumulative number of countries with law") + 
  scale_color_manual(values=c("black", "grey50", "grey30"), name=NULL) +
  scale_linetype_manual(values=c("solid", "solid", "32"), name=NULL) +
  theme_light(8) + theme(legend.position="bottom")


# Number of NGOs with ECOSOC status over time
ecosoc <- read_feather(file.path(PROJHOME, "data", "ecosoc.feather"))

ecosoc.plot.data <- ecosoc %>%
  group_by(status.clean, year.actual) %>%
  summarise(count = n()) %>%
  mutate(cumulative.count = cumsum(count)) %>%
  ungroup() %>%
  mutate(status.clean = factor(status.clean,
                               levels=c("General", "Roster", "Special"),
                               labels=c("General    ", "Roster    ", "Special")))

ggplot(ecosoc.plot.data, aes(x=year.actual, y=cumulative.count,
                             colour=status.clean, linetype=status.clean)) +
  geom_line(size=1) +
  coord_cartesian(xlim=ymd("1950-01-01", "2016-01-01")) +
  labs(x=NULL, y="Cumulative number of NGOs with ECOSOC status") +
  scale_y_continuous(labels=comma) + 
  scale_color_manual(values=c("black", "grey50", "grey30"), name=NULL) +
  scale_linetype_manual(values=c("solid", "solid", "32"), name=NULL) +
  theme_light(8) + theme(legend.position="bottom")
