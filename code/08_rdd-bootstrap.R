# ----------------------------------------------------
#   PS 811: Statistical Computing
#   Spring 2020
#   RDD and Bootstrapping
# ----------------------------------------------------

library("here")
library("tidyverse")
library("broom")
library("prediction") # install.packages("prediction")
library("rdrobust") # install.packages("rdrobust")

# ---- data -----------------------

# Each row is a district x party x year case.
# Every case has a primary election and a general election.
# Keep only cases where the ideological "distance" between 
#   primary candidates is greater than the median.
# Keep cases where the primary margin is less than 10%.
rd_data <- 
  haven::read_dta(here("data", "hall_house-primaries.dta")) %>%
  filter(absdist > median(absdist)) %>%
  filter(abs(margin) < .2) %>%
  rename(
    general_vote = dv,
    extremist_margin = rv,
    extremist_win = treat
  ) %>%
  print()

# notice the N is the same as the figure in the slides.


# ---- estimate RDD regression -----------------------

# we fit a line on each side of the cutoff (using dummy/interaction).
# the indicator for "treatment" is the treatment effect at the cutoff.
rd <- lm(
  general_vote ~ 1 + extremist_margin + 
                 extremist_win + extremist_win*extremist_margin,
  data = rd_data
)

# coefficients
tidy(rd)

# model results trick
tidy_rd_list <- rd %>%
  tidy(conf.int = TRUE) %>%
  split(.$term) %>%
  print()


# there is also an {rdrobust} package
#   for optimal bandwidth RD estimation
#   https://sites.google.com/site/rdpackages/rdrobust
# There are also "nonparametric" approaches to RD
#   which weight observations near the cutoff more highly
#   than observations far from the cutoff
#   see Calonico, Cattaneo, and Titiunik (2014)
#   https://d1e153b2-a-62cb3a1a-s-sites.googlegroups.com/site/rdpackages/rdrobust/Calonico-Cattaneo-Titiunik_2014_ECMA.pdf?attachauth=ANoY7cpcFmAh3MCJixnoXDiazukHmlQE3v9_IvlNemIL1Dw1w9DelMFPbmu14dOSqj7pBMxjQwJfgGc6TFJTEEOP4GAxk7q-B-Nmwo_PWhbEH9iiXrhmAvI6Vk-6n-ioB7Kq-rrLiI69CRNhsHE7mZOHiH3hCBEpJbBi3DtCszM3gyhzpUUUedJcrGpSceTloym4BiWZlTMQM2nBudmlN3rZQ2-ooXy3430hRF1cqfr2b68-6eC4KIh3qHL05rxh8wAtYuRrSvMx&attredirects=0




# ---- plot the RD fit -----------------------

# using the prediction package
# (makes confidence intervals easy, data frame output)
rd_predictions <- 
  prediction(rd, interval = "confidence") %>%
  as_tibble() %>%
  print()

# plot data and the fitted line
ggplot(rd_predictions) +
  aes(x = extremist_margin, y = general_vote, group = extremist_win) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_ribbon(
    aes(ymin = fitted.lwr, ymax = fitted.upr),
    color = NA, fill = "lightblue"
  ) +
  geom_point(color = "gray", size = 0.5) +
  geom_line(aes(y = fitted.fit)) +
  coord_cartesian(ylim = c(0.35, 0.8)) +
  theme_minimal() +
  labs(
    x = "Extremist Candidate Primary Election Winning Margin",
    y = "General Election Vote Share"
  )



# ---- bootstrap the model -----------------------

# we might do bootstrapping when our model is nonparametric
#   so analytical confidence intervals are harder to calculate.
# Example: nonparametric RD using "triangular" weight kernel

tri_rd <- 
  rdrobust::rdrobust(
    y = rd_data$general_vote,
    x = rd_data$extremist_margin,
    c = 0,
    h = 0.2,
    kernel = "triangular"
  )

tri_rd

attributes(tri_rd)

tri_rd$coef



# --- demonstration of "resampling the data" --- 

# original data:
rd_data

# sample N rows from the data with replacement
sample_n(rd_data, size = nrow(rd_data), replace = TRUE)





# --- implement many times ---

# A few ways to do this. 
# Many people might do a loop over some large number of iterations.
# We learned last week that loops in R are slow and hard to code.

# Using purrr:
# Make a nested data frame for 1000 bootstrapped samples.
# boot_data: resampled data
data_samples <- 
  tibble(
    boot = 1:2000,
    data = list(rd_data)
  ) %>%
  mutate(
    boot_data = map(
      .x = data, 
      .f = ~ sample_n(.x, size = nrow(.x), replace = TRUE)
    )
  ) %>%
  print()

# boot_model: model estimated from resampled data
# boot_effect: model coefficients from every model
boot_models <- data_samples %>%
  mutate(
    boot_model = map(
      .x = boot_data,
      .f = ~ {
        rdrobust::rdrobust(
          y = .x$general_vote, 
          x = .x$extremist_margin, 
          c = 0, 
          h = 0.2, 
          kernel = "triangular"
        )
      }
    ),
    boot_effect = map_dbl(
      .x = boot_model, 
      .f = ~ .x$coef["Robust", ]
    )
  ) %>%
  print()


# plot the bootstrap coefficients as histogram
# plus the original CI (added as ad-hoc point and line segment)
boot_models %>%
  ggplot() +
  aes(x = boot_effect) +
  geom_histogram() +
  annotate(
    geom = "point", 
    x = tidy_rd_list$extremist_win$estimate, 
    y = -5
  ) +
  annotate(
    geom = "segment",
    x = tidy_rd_list$extremist_win$conf.low, 
    xend = tidy_rd_list$extremist_win$conf.high, 
    y = -5, 
    yend = -5
  ) +
  labs(
    title = "Parametric vs. Nonparametric RD",
    subtitle = "Histogram of Bootstrapped Coefficients\nfrom Triangular Kernel RD",
    x = "Local Treatment Effect of Extremism\nat the Cutoff",
    y = "Count"
  )


# there are also packages for bootstrapping
# {bootstrap} or {boot}
# maybe even {tidymodels}? I'm not sure.
