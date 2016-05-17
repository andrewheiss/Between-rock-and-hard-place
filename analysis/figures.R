library(dplyr)
library(feather)
library(ggplot2)
library(gtable)
library(Cairo)
library(scales)

# Theme
theme_ingos <- function(base_size=9, base_family="Open Sans Light") {
  update_geom_defaults("bar", list(fill = "grey30"))
  update_geom_defaults("line", list(colour = "grey30"))
  ret <- theme_bw(base_size, base_family) +
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          title=element_text(size=rel(1.1), vjust=1.2, family="Open Sans Semibold"),
          plot.subtitle=element_text(size=rel(0.8), family="Open Sans Light"),
          plot.caption=element_text(margin=margin(t=5), size=rel(0.6),
                                    family="Open Sans Light"),
          panel.border = element_blank(), 
          panel.margin = unit(1, "lines"),
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
          legend.margin = unit(0.1, "lines"),
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
                                    "Foreign funding prohibition    ",
                                    "Advocacy restrictions")))

fig.restrictions <- ggplot(restrictions, 
                           aes(x=year.actual, y=cumulative.count,
                               colour=question, linetype=question)) + 
  geom_line(size=1) + 
  coord_cartesian(xlim=ymd("1950-01-01", "2011-01-01")) +
  labs(x=NULL, y="\nCumulative number of countries with law",
       title="Countries with NGO regulations",
       caption="Source: Christensen and Weinstein 2013") + 
  scale_color_manual(values=c("black", "grey50", "grey30"), name=NULL) +
  scale_linetype_manual(values=c("solid", "solid", "21"), name=NULL) +
  guides(colour=guide_legend(nrow=2),
         linetype=guide_legend(nrow=2)) +
  theme_ingos(8)
fig.restrictions

# fig.save.cairo(fig.restrictions, filename="fig-restrictions",
#                width=3.35, height=3)


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
  coord_cartesian(xlim=ymd("1950-01-01", "2016-01-01")) +
  labs(x=NULL, y="Cumulative number of NGOs with ECOSOC status",
       title="NGOs with ECOSOC status",
       subtitle=paste("General status represents highest level of participation;",
                      "roster status represents lowest level of participation",
                      sep="\n"),
       caption="Source: UN Economic and Social Council NGO database, 2016") +
  scale_y_continuous(labels=comma) + 
  scale_color_manual(values=c("black", "grey50", "grey30"), name=NULL) +
  scale_linetype_manual(values=c("solid", "solid", "21"), name=NULL) +
  guides(colour=guide_legend(nrow=2, ncol=3, byrow=TRUE),
         linetype=guide_legend(nrow=2, ncol=3, byrow=TRUE)) +
  theme_ingos(8)
fig.ecosoc

# fig.save.cairo(fig.ecosoc, filename="fig-ecosoc",
#                width=3.35, height=3)

# Combine plots with gtable::cbind instead of gridExtra because it aligns axes
# and titles across plots
plot.all <- cbind(ggplotGrob(fig.ecosoc),
                  ggplotGrob(fig.restrictions),
                  size="first")  # Use the spacing from the first plot

grid::grid.draw(plot.all)

fig.save.cairo(plot.all, filename="fig-ingos",
               width=7, height=3.5)
