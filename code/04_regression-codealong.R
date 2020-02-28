# ----------------------------------------------------
#   PS 811: Statistical Computing for Poli Sci
#   Code-Along:  Regression Models, Output, Post-Estimation, etc.
# ----------------------------------------------------

# ---- packages -----------------------
library("here")
library("tidyverse")
library("broom")    # if error: install.packages("broom")


# ---- read data -----------------------

# data on U.S. House members and prior election
house <- 
  read_csv(here("data", "house-elections.csv")) %>%
  print()

# contains NOMINATE score (we've seen before)
# but also the district's prior Republican vote share (for House)
# and the district's prior Republican vote share for president

# quick view
house %>%
  select(nominate_dim1, contains("rep_share")) %>%
  summary()


# ---- simple regression -----------------------




# ---- use broom -----------------------



# ---- multiple regression -----------------------



# ---- coefficient plot -----------------------

# estimate a model, tidy it
bigger_tidy <- house %>%
  filter(election_year >= 2000) %>%
  lm(formula = nominate_dim1 ~ scale(rep_share_house) +
               scale(rep_share_pres) + republican +
               as.factor(election_year), 
     data = .) %>%
  tidy(conf.int = TRUE) %>%
  print()

# plot tidy estimates...



# ---- predictions with augment() -----------------------

# make newdata

# augment using newdata

# confidence intervals

# plot predictions




