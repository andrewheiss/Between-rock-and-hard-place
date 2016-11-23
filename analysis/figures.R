library(tidyverse)
library(feather)
library(gtable)
library(Cairo)
library(scales)
library(lubridate)

# Theme
theme_ingos <- function(base_size=9, base_family="Open Sans Light") {
  update_geom_defaults("bar", list(fill = "grey30"))
  update_geom_defaults("line", list(colour = "grey30"))
  ret <- theme_bw(base_size, base_family) +
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          title=element_text(size=rel(1.1), vjust=1.2, family="Open Sans Semibold"),
          plot.subtitle=element_text(size=rel(0.8), family="Open Sans Light"),
          plot.caption=element_text(margin=margin(t=1), size=rel(0.6),
                                    family="Open Sans Light"),
          panel.border = element_blank(), 
          panel.spacing = unit(1, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size=0.25, colour="grey90"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size=rel(0.8), family="Open Sans Semibold"),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          legend.position = "bottom",
          legend.title = element_text(size=rel(0.8)),
          legend.key.size=unit(.7, "line"),
          legend.key = element_blank(),
          legend.spacing = unit(0.1, "lines"),
          strip.text = element_text(size=rel(1), family="Open Sans Semibold"),
          strip.background = element_rect(fill="#ffffff", colour=NA))
  ret
}

# Save Cairo PDF and PNG at the same time
fig.save.cairo <- function(fig, filepath=file.path(PROJHOME, "figures"), 
                           filename, width, height, units="in", ...) {
  ggsave(fig, filename=file.path(filepath, paste0(filename, ".pdf")),
         width=width, height=height, units=units, device=cairo_pdf, ...)
  ggsave(fig, filename=file.path(filepath, paste0(filename, ".png")),
         width=width, height=height, units=units, type="cairo", dpi=300, ...)
}


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
                                    "Foreign funding prohibitions    ",
                                    "Advocacy restrictions")))

fig.restrictions <- ggplot(restrictions, 
                           aes(x=year.actual, y=cumulative.count,
                               colour=question, linetype=question)) + 
  geom_line(size=1) + 
  coord_cartesian(xlim=ymd("1950-01-01", "2011-01-01")) +
  labs(x=NULL, y="\nCumulative countries",
       title="Countries with NGO regulations",
       caption="Source: Christensen and Weinstein 2013") + 
  scale_color_manual(values=c("black", "grey70", "grey40"), name=NULL) +
  scale_linetype_manual(values=c("solid", "solid", "21"), name=NULL) +
  guides(colour=guide_legend(nrow=2),
         linetype=guide_legend(nrow=2)) +
  theme_ingos(7)
fig.restrictions

# fig.save.cairo(fig.restrictions, filename="fig-restrictions",
#                width=3.35, height=3)

restrictions.countries <- dcjw %>%
  filter(question %in% c("q_2a", "q_3e", "q_4a"), !is.na(year)) %>%
  arrange(year.actual) %>%
  group_by(country.name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


# Number of NGOs with ECOSOC status over time
ecosoc <- read_feather(file.path(PROJHOME, "data", "ecosoc.feather"))

ecosoc.plot.data <- ecosoc %>%
  group_by(status.clean, year.actual) %>%
  summarise(count = n()) %>%
  mutate(cumulative.count = cumsum(count)) %>%
  ungroup() %>%
  mutate(status.clean = factor(status.clean,
                               levels=c("General", "Special", "Roster"),
                               labels=c("General    ", "Special    ", "Roster")))

fig.ecosoc <- ggplot(ecosoc.plot.data, 
                     aes(x=year.actual, y=cumulative.count,
                         colour=status.clean, linetype=status.clean)) +
  geom_line(size=1) +
  coord_cartesian(xlim=as.POSIXct(ymd("1950-01-01", "2016-01-01"))) +
  labs(x=NULL, y="Cumulative NGOs",
       title="NGOs with ECOSOC status",
       subtitle=paste("General status represents highest level of participation;",
                      "roster status represents lowest level of participation",
                      sep="\n"),
       caption="Source: UN Economic and Social Council NGO database, 2016") +
  scale_y_continuous(labels=comma) + 
  scale_color_manual(values=c("black", "grey70", "grey40"), name=NULL) +
  scale_linetype_manual(values=c("solid", "solid", "21"), name=NULL) +
  guides(colour=guide_legend(nrow=2, ncol=3, byrow=TRUE),
         linetype=guide_legend(nrow=2, ncol=3, byrow=TRUE)) +
  theme_ingos(7)
fig.ecosoc

# fig.save.cairo(fig.ecosoc, filename="fig-ecosoc",
#                width=3.35, height=3)


# Percentage of NED programs that are measurable
bush.plot <- read_feather(file.path(PROJHOME, "data", "bush.feather")) %>%
  group_by(year.actual) %>%
  summarise(perc.measurable = mean(measurable), 
            perc.professional = mean(professional)) %>%
  gather(variable, perc, -year.actual) %>%
  mutate(variable = factor(variable,
                           levels=c("perc.professional", "perc.measurable"),
                           labels=c("Professional staff    ", "Measurable programs"),
                           ordered=TRUE))

fig.bush <- ggplot(bush.plot, aes(x=year.actual, y=perc, colour=variable)) +
  geom_line(size=1) +
  coord_cartesian(xlim=ymd("1985-01-01", "2010-01-01")) +
  labs(x=NULL, y="\nPercentage",
       title="NED characteristics",
       caption="Source: Bush 2015") +
  scale_y_continuous(labels=percent) + 
  scale_color_manual(values=c("black", "grey70"), name=NULL) +
  guides(colour=guide_legend(nrow=2, ncol=3, byrow=TRUE)) +
  theme_ingos(7)
fig.bush


# Combine plots with gtable::cbind instead of gridExtra because it aligns axes
# and titles across plots
plot.all <- cbind(ggplotGrob(fig.ecosoc),
                  ggplotGrob(fig.restrictions),
                  ggplotGrob(fig.bush),
                  size="first")  # Use the spacing from the first plot

grid::grid.draw(plot.all)

fig.save.cairo(plot.all, filename="fig-ingos-all",
               width=8, height=2.5)

# Just donor and state environments
fig.restrictions.plain <- fig.restrictions + 
  labs(title=NULL, subtitle=NULL, caption=NULL)

fig.bush.plain <- fig.bush +
  labs(title=NULL, subtitle=NULL, caption=NULL)

plot.env <- cbind(ggplotGrob(fig.restrictions.plain),
                  ggplotGrob(fig.bush.plain),
                  size="first")  # Use the spacing from the first plot

grid::grid.draw(plot.env)

fig.save.cairo(plot.env, filename="fig-ingos-env",
               width=7, height=2.5)
