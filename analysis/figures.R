library(dplyr)
library(feather)
library(ggplot2)

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
  scale_color_manual(values=c("black", "grey50", "grey30"), name=NULL) +
  scale_linetype_manual(values=c("solid", "solid", "32"), name=NULL) +
  theme_light(8) + theme(legend.position="bottom")
