# ----------------------------------------------------
#   GRAPHICS, with ggplot2
# ----------------------------------------------------

# ggplot2 is a graphics package.
# The {tidyverse} package imports ggplot2

library("here")
library("tidyverse")
library("tidylog") # gives feedback about data manipulation


# Grammar components:

# - data: the underlying data
# - geoms: geometric representations of data (points, lines, bars...)
# - aesthetic mapping: transformations from "data space" to "graphics space"
# - scales: modifications of aesthetic mappings
# - facets: dividing a plot into groups
# - coordinates: systems for representing data spaces
# - theme: other minutiae, fonts, etc.

# This script will explain each of these grammar components
#   and how to control them with ggplot2.




# ---- read and set up data -----------------------



# ---- import data -----------------------


# did you open Rstudio from your .Rproj file?
# how do you know?
# - Rstudio says project name in top right corner
# - working directory is already set

getwd()


# we will keep using the data on federal legislators

federal_raw <- 
  read_csv(here("data", "HSall_members.csv")) %>%
  print()



# ---- some more information about this data -----------------------

# The federal government has three "branches":
# - the Congress (legislature, writes laws, made up of House and Senate).
# - the executive (implements laws, led by the President).
# - the federal court system (interprets laws, including the Supreme Court).

# this dataset contains one row for every legislator and president 
#   who has ever served since the beginning of the U.S.

# The most important feature of this dataset is a set of IDEOLOGY SCORES
#   for every individual, which are estimated using a statistical model
#   with data from the YEA and NAY votes that Congress.
# Because presidents sign or veto bills that pass Congress,
#   you can estimate their ideology scores as well.

# Here is a rundown of some important data:
# congress: The two-year session of Congress. 
#           The first Congress began in 1789
# chamber: is the individual a House member, Senator, or President.
# icpsr: this is an identifier code for the individual.
# state_icpsr: identifier code for a state. 
# party_code: a numeric code for the many political parties that have existed.
# last_means: how did the member obtain office (election, appointment, etc)
# [skip some variables that aren't very important]

# about the ideology scores: the scores are called NOMINATE scores.
# NOMINATE stands for [nomina]l [t]wo-step [e]stimation (a horrible acronym).
# The scores have a first and second "dimension," which are low-dimensional
#   summaries of the things that structure voting.
# MOST OF THE TIME, it makes sense to interpret the first dimension as 
#   left-right (liberal-conservative) ideology. 
# The second dimension is hard to interpret, but many people think that it 
#   effectively captures dynamics of RACIAL POLITICS especially around the 
#   Civil War (1860s) and the Civil Rights Era (1960s).
# Nokken-Poole scores are another method of estimating these dimensions,
#   but they are less commonly used than NOMINATE.


# ---- let's keep only a subset of data -----------------------

# use the tools we learned last week:
# - keep only House and Senate cases (no president)
# - keep Democrat and Republican cases (100 and 200, nothing else)
# - recode party, create congress_year
# - keep only certain variables

# Note how the {tidylog} package gave us feedback on what we did.

congress <- federal_raw %>%
  filter(chamber == "House" | chamber == "Senate") %>%
  filter(party_code %in% c(100, 200)) %>%
  mutate(
    party = case_when(
      party_code == 100 ~ "Democrat",
      party_code == 200 ~ "Republican"
    ),
    congress_year = 1787 + (congress * 2)
  ) %>%
  select(
    starts_with("congress"), chamber, state_abbrev,
    party, bioname, 
    contains("_dim"), -contains("nokken")
  ) %>%
  print()


# same thing, but only the most recent session of Congress
this_congress <- congress %>%
  filter(congress == max(congress)) %>%
  print()


# average and sd of ideology scores over time
congress_means <- congress %>%
  group_by(congress, congress_year, chamber, party) %>%
  summarize(
    mean_nom1 = mean(nominate_dim1, na.rm = TRUE),
    sd_nom1 = sd(nominate_dim1, na.rm = TRUE),
    mean_nom2 = mean(nominate_dim2, na.rm = TRUE),
    sd_nom2 = sd(nominate_dim2, na.rm = TRUE)
  ) %>%
  arrange(congress) %>%
  ungroup() %>%
  print() 

house_means <- congress_means %>%
  filter(chamber == "House") %>%
  print()

senate_means <- congress_means %>%
  filter(chamber == "Senate") %>%
  print()


# ----------------------------------------------------
#   get started with plots
# ----------------------------------------------------

# ABSOLUTE ESSENTIALS to get started


# DATA: must have a dataset
ggplot(this_congress)


# AESTHETIC MAPPING: 
#   what things from the "data space" become part of "plot space"?
# Tell ggplot what the X and Y axes are using aes(x = ..., y = ...)
ggplot(this_congress) +
  aes(x = nominate_dim1, y = nominate_dim2)


# GEOMS: geometric representations of data.
# This example: show data (intersections of X and Y) as points
ggplot(this_congress) +
  aes(x = nominate_dim1, y = nominate_dim2) +
  geom_point()


# ALL PLOTS have some combination of those features:
#   [1] data, [2] declared axes, and [3] some geom_* to represent data.




# ---- Demonstration of "aesthetics" -----------------------

# other aesthetics typically include color, fill, shape, size...

# every geom has potentially different aesthetics that you can modify
# look at geom_point
help(geom_point)


# You can manipulate aesthetics in two major ways.
# 1. manual control
ggplot(this_congress) +
  aes(x = nominate_dim1, y = nominate_dim2) +
  geom_point(color = "maroon", 
             size = 3, 
             alpha = 0.5)

# 2. Aesthetic MAPPING: transforming from "data space" into "graphic space"
ggplot(this_congress) +
  aes(x = nominate_dim1, y = nominate_dim2) +
  geom_point(aes(color = party))

# Think of a "map" as a functional transformation. 
# E.g. transform party into color space.




# Here is a VERY common error that you will encounter."
ggplot(this_congress) +
  aes(x = nominate_dim1, y = nominate_dim2) +
  geom_point(color = party)

# what do you mean, "party is not found?" It's right here:
count(this_congress, party)

# The problem is that mapping from data ONLY works 
#   if you use the aes() function!!!!!



# ---- other geoms: smooths -----------------------

# what's the relationship between dim1 and dim2?
ggplot(this_congress) +
  aes(x = nominate_dim1, y = nominate_dim2) +
  geom_point(aes(color = party)) +
  geom_smooth()



# aesthetics tip:
# maps only apply to specified geoms!
ggplot(this_congress) +
  aes(x = nominate_dim1, y = nominate_dim2) +
  geom_point(aes(color = party)) +
  geom_smooth(aes(color = party))


# you can set global aesthetics (and override as needed)
ggplot(this_congress) +
  aes(x = nominate_dim1, y = nominate_dim2, color = party) +
  geom_point(color = "black") +
  geom_smooth()


# method = "lm" (linear model) 
# se = FALSE (no std error/confidence band)
ggplot(this_congress) +
  aes(x = nominate_dim1, y = nominate_dim2, color = party) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)



# default is "loess" or "gam" (depending on sample size)
# can do more methods! like "glm" etc etc







# average by congress x party
house_means


# lines
ggplot(house_means) +
  aes(x = congress_year, y = mean_nom1) +
  geom_line()

# what's missing?
ggplot(house_means) +
  aes(x = congress_year, y = mean_nom1, color = party) +
  geom_line()


# histograms and densities
# these use only the X aesthetic, because they calculate Y internally
ggplot(this_congress) +
  aes(x = nominate_dim1) +
  geom_histogram(
    aes(fill = party), 
    color = "white",
    binwidth = .05
  )

ggplot(this_congress) +
  aes(x = nominate_dim1) +
  geom_density(
    aes(fill = party), 
    color = "white"
  )

# note the difference between color and fill.
# for shapes with borders, color is the border, fill is interior.


# pointranges:
# combo of point and error bar.
# requires additional ymin and ymax aesthetics
ggplot(house_means) +
  aes(x = congress_year, y = mean_nom1, color = party) +
  geom_pointrange(
    aes(ymin = mean_nom1 - sd_nom1, 
        ymax = mean_nom1 + sd_nom1)
  )

# horizontal/vertical lines
ggplot(house_means) +
  aes(x = congress_year, y = mean_nom1, color = party) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = mean_nom1 - sd_nom1, 
        ymax = mean_nom1 + sd_nom1)
  )



# ---- scales: modify aesthetic mappings -----------------------

# all scale functions follow scale_[aesthetic]_[descriptor]() format

# modify the default color mapping
# use color palettes with scale_color_brewer()
ggplot(house_means) +
  aes(x = congress_year, y = mean_nom1, color = party) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = mean_nom1 - sd_nom1, ymax = mean_nom1 + sd_nom1)
  ) +
  scale_color_brewer(palette = "Set2")

# see below for packages containing more colors!


# supply colors manually with scale_color_manual()
ggplot(house_means) +
  aes(x = congress_year, y = mean_nom1, color = party) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = mean_nom1 - sd_nom1, ymax = mean_nom1 + sd_nom1)
  ) +
  scale_color_manual(
    values = c("Democrat" = "dodgerblue", "Republican" = "tomato")
  )



# axes are aesthestics too!
# specify breaks (tick locations), labels, transformations...
ggplot(house_means) +
  aes(x = congress_year, y = mean_nom1, color = party) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = mean_nom1 - sd_nom1, ymax = mean_nom1 + sd_nom1)
  ) +
  scale_color_manual(
    values = c("Democrat" = "dodgerblue", "Republican" = "tomato")
  ) +
  scale_x_continuous(breaks = seq(1788, 2018, 24)) +
  scale_y_continuous(
    breaks = seq(-.5, .5, .5),
    labels = c("More Liberal", "Moderate", "More Conservative")
  )


# mapped aesthetics get "names" that go either in legends or axes
# control them with labs().
# (also lets you do title, subtitle, etc..)
ggplot(house_means) +
  aes(x = congress_year, y = mean_nom1, color = party) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = mean_nom1 - sd_nom1, ymax = mean_nom1 + sd_nom1)
  ) +
  scale_color_manual(
    values = c("Democrat" = "dodgerblue", "Republican" = "tomato")
  ) +
  scale_x_continuous(breaks = seq(1788, 2018, 24)) +
  scale_y_continuous(
    breaks = seq(-.5, .5, .5),
    labels = c("More Liberal", "Moderate", "More Conservative")
  ) +
  labs(
    x = "Election Year", y = NULL, 
    color = NULL,
    title = "Ideology In Congress",
    subtitle = "Average DW-NOMINATE Scores, U.S. House",
    caption = "Points are averages.\nLines represent +/- 1 standard deviation."
  )



# ---- coordinates -----------------------

# most common coordinate system is "cartesian" a.k.a. XY plane
# but you can also use polar coordinates, coord_flip() (flip axes), and more

# control x and y limits with xlim and ylim arguments
ggplot(house_means) +
  aes(x = congress_year, y = mean_nom1, color = party) +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(ymin = mean_nom1 - sd_nom1, ymax = mean_nom1 + sd_nom1)
  ) +
  scale_color_manual(
    values = c("Democrat" = "dodgerblue", "Republican" = "tomato")
  ) +
  scale_x_continuous(breaks = seq(1788, 2018, 24)) +
  labs(
    x = "Election Year", y = NULL, 
    color = NULL,
    title = "Ideology In Congress",
    subtitle = "Average DW-NOMINATE Scores, U.S. House",
    caption = "Points are averages.\nLines represent +/- 1 standard deviation."
  ) +
  coord_cartesian(ylim = c(-1, 1))



# demo of coord_flip()
# (also geom_col)
this_congress %>%
  count(state_abbrev) %>%
  mutate(
    state_abbrev = fct_reorder(state_abbrev, n)
  ) %>%
  ggplot() +
  aes(x = state_abbrev, y = n) +
  geom_col() +
  coord_flip()


# note: you can pipe into ggplot
#       but sometimes it limits your capabilities to do things




# ---- facets -----------------------

# two main types of facets: wraps and grids


# facet_wrap(): can "wrap around" line breaks
# MUST USE TILDE ~
ggplot(this_congress) +
  aes(x = nominate_dim1, y = nominate_dim2) +
  geom_point() +
  facet_wrap(~ state_abbrev)
# can control number of rows and columns with nrow and ncol arguments




# facet_grid(): specify row and column
# MUST USE row_variable ~ col_variable
ggplot(this_congress) +
  aes(x = nominate_dim1, y = nominate_dim2) +
  geom_point() +
  facet_grid(party ~ chamber)




# ---- themes -----------------------


ggplot(this_congress) +
  aes(x = nominate_dim1, y = nominate_dim2) +
  geom_point() +
  facet_grid(party ~ chamber) +
  theme_bw()


# packages for themes: ggthemes, ggthemr, ...
# Modify existing themes and save as your own!


# ---- other awesome tools for ggplot2 -----------------------

# packages for enhancing ggplot2!
# ggdag: draw DAGs (causal graphs) with tidy/ggplot-friendly tools
# patchwork: combine ggplots into one plot
# ggforce: superpowered enhacements (cool!)
# ggpointdensity: color points according to estimated density
# ggridges: smush lots of histograms/densities together
# viridis: awesome colorblind-friendly color schemes
# scico: more colorblind-friendly color schemes
# wesanderson: movie-themed color palettes
# ggthemes: more themes!
# ggthemr: more themes!
# tidybayes: geoms for Bayesian MCMC samples
# GGally: helpful tools
# latex2exp: type LaTeX math into graphics
# and so much more!



