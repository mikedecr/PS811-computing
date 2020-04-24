# ----------------------------------------------------
#   PS 811: Statistical Computing
#   Spring 2020
#   RDD and Bootstrapping
# ----------------------------------------------------

library("here")
library("tidyverse")
library("broom")
library("prediction") # install.packages("prediction")

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

rd_data

# demonstration of "resampling the data"
# sample N rows from the data with replacement
sample_n(rd_data, size = nrow(rd_data), replace = TRUE)

# make a nested data frame for 1000 bootstrapped samples
# boot_data: resampled data
data_samples <- 
  tibble(
    boot = 1:1000,
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
# boot_tidy: model coefficients from resampled data
boot_models <- data_samples %>%
  mutate(
    boot_model = map(
      .x = boot_data,
      .f = ~ lm(general_vote ~ 1 + extremist_margin + 
                  extremist_win + extremist_win*extremist_margin, 
                data = .x)
    ),
    boot_tidy = map(boot_model, tidy),
    boot_augment = map(boot_model, augment)
  ) %>%
  print()


# plot the original data,
# plus the resampled lines
boot_models %>%
  select(boot, boot_augment) %>%
  unnest(cols = boot_augment) %>%
  ggplot() +
  aes(x = extremist_margin, y = .fitted) +
  geom_line(
    aes(group = paste(boot, extremist_win)), 
    alpha = 0.5, color = "gray40"
  ) +
  geom_ribbon(
    data = rd_predictions,
    aes(y = fitted.fit, ymin = fitted.lwr, ymax = fitted.upr),
    color = NA, fill = "tomato",
    alpha = 0.5
  ) +
  geom_point(data = rd_data, aes(y = general_vote), size = 0.5) +
  theme_minimal() +
  coord_cartesian(ylim = c(0.35, 0.8))


# there are also packages for bootstrapping
# {bootstrap} or {boot}
# maybe even {tidymodels}? I'm not sure.
