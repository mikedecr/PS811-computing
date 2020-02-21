# ----------------------------------------------------
#   Let's experiment with ggplot2
# ----------------------------------------------------


# ---- always load packages first -----------------------
library("here")
library("tidyverse")
library("tidylog")  # first time: install.packages("tidylog")


# ---- ggplot2 -----------------------

# gg stands for ~*~ grammar of graphics ~*~

# grammar components:

# - data: the underlying data
# - geoms: geometric representations of data (points, lines, bars...)
# - aesthetic mapping: transformations from "data space" to "graphics space"
# - scales: modifications of aesthetic mappings
# - facets: dividing a plot into groups
# - coordinates: systems for representing data spaces
# - theme: other minutiae, fonts, etc.


# grammar of graphics is a theory of graphical composition that says
#   "most (all?) graphics are made of the same building blocks."

# If you learn the grammar, you can make many different types of plots.
# Even if it's your first time making a certain type of plot,
#   you already know "how" to do it, because you know the grammar.


# ---- read data into R -----------------------

# federal_raw <- 






# note to self! explain data 






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


# This contains Republicans and Democrats from all congresses.
# Save only the current congress 

# try here --------- 


# ---------






# average and sd of ideology scores over time
# by (congress x chamber x party) groups
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


# save separate datasets for House and Senate
# ----



# ----









# ---- make a plot! -----------------------

# this_congress: scatter nominate_dim1 and nominate_dim2








# NOTES:
# - all plots have data, aes(), geoms,       
# - axes are aesthetics!!
# - geoms have different aesthetics
# - common aesthetic errors

# ADD:
# - smooths
# - scales
# - labs
# - coords
# - facets
# - themes






# ---- histograms and densities -----------------------



# color vs. fill

# ADD:
# - scales
# - labs
# - coords
# - facets
# - themes




# ---- lines -----------------------

# use house averages



# NOTES
# - linetype
# - points over lines


# ADD:
# - scales
# - labs
# - coords
# - facets
# - themes








# ---- next week: regression and model output -----------------

# Estimating regressions
# Interpreting output
# Regression tables
# Coefficient plots
# Generating and plotting predicted values

# including such ggplot features as...
# geom_pointrange()
# geom_ribbon()












