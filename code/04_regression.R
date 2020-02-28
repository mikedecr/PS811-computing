# ----------------------------------------------------
#   PS 811: Statistical Computing for Poli Sci
#   
#   Regression Models, Output, Post-Estimation, etc.
# ----------------------------------------------------

# this lesson covers three big things
# 1. Estimating common regression models
# 2. Understanding, working with, visualizing model estimates
# 3. Generating post-estimation quantities

# Estimating models
# - linear models: lm(), 
# - general linear models: glm()
# - robust standard errors
# - other estimation approaches (lme4, plm, felm)

# Output
# - introduce broom as a replacement for summary()
# - tidy, broom



# ---- packages -----------------------

# introducing the {broom} package
library("here") 
library("tidyverse")
library("broom")      # if error: install.packages("broom")


# ---- read data -----------------------

# data on elections to U.S. House of Representatives
house <- 
  here("data", "house-elections.csv") %>%
  read_csv() %>%
  print()


# contains NOMINATE score (we've seen before)
# but also the district's prior Republican vote share (for House)
# and the district's prior Republican vote share for president

# quick view
house %>%
  select(nominate_dim1, contains("rep_share")) %>%
  summary()



# ---- regression of ideology on vote -----------------------

# Why don't all legislators vote "moderately"?
# If they're constantly under threat of electoral defeat, 
#   should they be more moderate? 
# On the other hand, if they represent a "safe" district,
#   maybe they are more free to vote ideologically


# lm() means "linear model"
# lm(y ~ x, data = data_name)
simple_reg <- lm(nominate_dim1 ~ rep_share_house, data = house)



# ---- looking at model results -----------------------

# the classic (old-fashioned) way
summary(simple_reg)


# the {broom} way: results are data frames

# broom::tidy() gives variable-level statistics
# (coefs, std.errors, test stats, p.values, interval bounds)
tidy(simple_reg)   
tidy(simple_reg, conf.int = TRUE)


# broom::glance() gives model-level statitsics
glance(simple_reg) 


# broom::augment() gives data-level statistics
augment(simple_reg)


# ---- plot residuals -----------------------

# straightforward: augment() gives you residuals (.resid)
ggplot(data = augment(simple_reg)) +
  aes(x = .fitted, y = .resid) +
  geom_point()


# what's the matter?

# plot raw data!
ggplot(house) +
  aes(x = rep_share_house, y = nominate_dim1) +
  geom_point() +
  geom_smooth(method = "lm")



# ---- multiple regression -----------------------

# control for things with 
# y ~ x1 + x2 + ...
# control for party (republican indicator variable)

# also note PIPE INTO NON-FIRST ARGUMENT
# use `.` to stand-in for data
multi_mod <- house %>%
  lm(nominate_dim1 ~ rep_share_house + republican, data = .) %>%
  print()

tidy(multi_mod)
glance(multi_mod)


# much better
ggplot(augment(multi_mod)) +
  aes(x = .fitted, y = .resid) +
  geom_point(alpha = 0.2) +
  geom_smooth(aes(group = republican))




# ---- quirks of estimation -----------------------

# multi-category variables behave like a series of dummy variables
# (one category is always excluded to prevent singularity)
house %>%
  lm(nominate_dim1 ~ rep_share_house + state_abbrev, data = .) %>%
  tidy()


# coerce numbers to categories using as.factor()
# example: "year fixed effects"
# if you don't as.factor(), it thinks "election_year" is continuous
house %>%
  lm(nominate_dim1 ~ rep_share_house + as.factor(election_year), 
     data = .) %>%
  tidy()

# suppress a model intercept with `0 +`
house %>%
  lm(nominate_dim1 ~ 0 + rep_share_house + as.factor(election_year), 
     data = .) %>%
  tidy()



# ---- estimating other types of models -----------------------

# general linear models (logit, probit, etc)
logit_example <- 
  glm(
    republican ~ rep_share_pres,
    family = binomial(link = "logit"),
    data = house
  ) 

glance(logit_example)
tidy(logit_example)


# predictions are on the LINK SCALE;
# need to back-out the appropriate parameter estimate
# plogis is logistic CDF: 1 / (1 + exp(-XB))
logit_predictions <- logit_example %>%
  augment() %>%
  mutate(estimated_probability = plogis(.fitted)) %>%
  print() 


ggplot(logit_predictions) +
  aes(x = rep_share_pres, y = republican) +
  geom_point() +
  geom_line(aes(y = estimated_probability))



# ---- robust standard errors -----------------------

# and clustered (cluster-robust) standard errors

# R gets a lot of wrap for this being "hard to do" 
# but I think what's actually going on is 
#   nobody understands what their models really are saying

# the "old way" involves the {sandwich} package
# you have to overwrite the model's variance-covariance matrix
# with a heteroskedasticity consistent VC matrix
# it's tedious but doable.


# things are faster now. 
# for linear models: install.packages("estimatr")
multi_robust <- 
  estimatr::lm_robust(
    nominate_dim1 ~ rep_share_house + party,
    data = house
  )

# compare
tidy(multi_mod)
tidy(multi_robust)

# can also do commarobust() on an existing lm() object
estimatr::commarobust(multi_mod)


# tidy() works for lm_robust objects, but augment() does not
# predict(model, newdata = ..., interval = "confidence") works,
#   but it returns a matrix, not a data frame.
#   do you need to coerce it back to data frame land.

# for more models: Alex Tahk's bucky::robustify()


# do not get robust/HC errors confused with "robust regression"
# latter of which usually means the ridge regression estimator
# which is a form of penalized regression 
 


# ---- displaying model output (graphics) -----------------------

# My preferred way:
# - 1: estimate model
# - 2: tidy model
# - 3: plot coefficients

# 1
big_model <- 
  lm(nominate_dim1 ~ rep_share_house + 
                     rep_share_pres + 
                     party +
                     as.factor(election_year),
     data = house)

# 2
big_tidy <- big_model %>%
  tidy(conf.int = TRUE) %>%
  print()

# 3
ggplot(big_tidy) +
  aes(x = term, y = estimate) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip()

# EASY.



# AND EASY TO COMBINE MODELS
# estimate separate models for each major party

# hint: save model formula ahead of time
mod_formula <- 
  nominate_dim1 ~ rep_share_house + rep_share_pres + as.factor(election_year)

rep_model <- house %>%
  filter(party == "Republican") %>%
  lm(formula = mod_formula, data = .) %>%
  print()

dem_model <- house %>%
  filter(party == "Democrat") %>%
  lm(formula = mod_formula, data = .) %>%
  print()

# combine data frames
stacked_models <- 
  bind_rows(
    "Republicans" = tidy(rep_model, conf.int = TRUE),
    "Democrats" = tidy(dem_model, conf.int = TRUE),
    .id = "party"
  ) %>%
  print()


# plot multiple models
ggplot(stacked_models) +
  aes(x = term, y = estimate, color = party) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.5)
  ) +
  coord_flip()



# ---- displaying model output (tables) -----------------------

# This is a challenging topic because it is complicated by
#   the use case of R Markdown.
# Which is to say, ideally we want a method that works well in R Markdown
#   regardless of output type (html, pdf...)
# But this is challenging.

# when it comes to regression models, 
# I have had good luck with texreg: install.packages("texreg")
texreg::screenreg(list(rep_model, dem_model))
texreg::texreg(list(rep_model, dem_model))
texreg::htmlreg(list(rep_model, dem_model))


# These f()s have lots of options.
# Some are nice (changing covariate labels).
# Others have horrendous defaults.
# Here are helpful defaults.
texreg::texreg(
  list(rep_model, dem_model),
  dcolumn = TRUE, use.packages = FALSE
)

texreg::htmlreg(
  list(rep_model, dem_model),
  doctype = FALSE
)

# You can save these to file
#   or have them appear as desired output in R Markdown
# Downside of this package:
#   doesn't have one single, output-flexible function

# Other functions you may investigate:
# - stargazer::stargazer() is just a worse version of texreg. 
#   don't use it. 
# - pander::pander() is known for being flexible in HTML/markdown, but complex
# - knitr::kable() is easy but a little limited in customization
# - {kableExtra} package enhances kable's appearance
# - xtable::xtable() makes nice tables for data frames, but LaTeX/PDF only

# we will come back to the issue of tables in RMarkdown later!!




# ---- working with "post-estimation" quantitites -----------------------

# we often want predicted values or other functions of model parameters


# augment() creates predictions, along with other things
tidy(multi_mod)
augment(multi_mod)

# usually you don't want to plot predictions for data you have
# but rather, for data you don't have
# such as: vary one variable, while fixing other variables

# You have to create this data though.
# Here are helpful functions

# tibble(): make a data frame, variable by variable
# (feels a lot like mutate)
tibble(
  rep_share_house = seq(0, 100, 10),
  republican = 1
)

# tribble(): not the greatest name for a function
#   makes a data frame, row by row
tribble(
  ~ rep_share_house, ~ republican,
                  0,            0,
                  0,            1,
                 50,            0,
                 50,            1,
                100,            0,
                100,            1
)


# more powerful stuff:
# crossing(): expand combinations of values
prediction_data <- 
  crossing(
    rep_share_house = seq(0, 100, 1),
    republican = c(0, 1)
  ) %>%
  print()


# use augment(model, newdata = ...)  to predict for these data
# MUST HAVE SAME VARIABLE NAMES as original model
augment(multi_mod, newdata = prediction_data)

# generates a fitted value and a s.e. for that value
# make a confidence interval as +/- the MOE
# which is itself a f() of s.e.
predictions <- 
  augment(multi_mod, newdata = prediction_data) %>%
  mutate(
    conf.low = .fitted - (1.96 * .se.fit),
    conf.high = .fitted + (1.96 * .se.fit)
  ) %>%
  print()


# plot predictions
# introducing geom_ribbon()
ggplot(predictions) +
  aes(
    x = rep_share_house, 
    y = .fitted, 
    color = as.factor(republican), 
    fill = as.factor(republican)
  ) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    color = NA,
    alpha = 0.25
  ) +
  geom_line() +
  coord_cartesian(ylim = c(-1, 1))



# ---- other types of predictions -----------------------

# you may want predicted DIFFERENCES
# such as the difference between Republicans and Democrats

# you may have luck with the {margins} package
# or with {sjPlot} or other tools by Strenge Jacke

# I sometimes prefer to simulate coefficients from the model
# and then generate a prediction for every simulation
# (matrix algebra comes in handy!!)

# install.packages("mvtnorm")

# suppose I had a matrix of coefficients,
# but instead of k x 1, it's k x m (for arbitrary m)
# this function samples the multivariate normal distribution
coef_matrix <- 
  mvtnorm::rmvnorm(
    n = 1000,
    mean = coef(multi_mod),
    sigma = vcov(multi_mod)
  ) %>%
  as_tibble() %>%
  print()

# I could use this to simulate lots of predictions
# this particular method brings us out of data frame world
matrix_data <- 
  tibble(
    const = 1,
    rep_share_house = 0:100
  ) %>%
  crossing(republican = c(0, 1)) %>%
  print()

prediction_matrix <- matrix_data %>%
  as.matrix() %>%
  (function(x) x %*% t(coef_matrix))

# we have 1000x predictions
dim(prediction_matrix)

simulation_frame <- prediction_matrix %>%
  as_tibble() %>%
  bind_cols(matrix_data, .) %>%
  gather(key = iteration, value = yhat, starts_with("V")) %>%
  print() 


# yooooooooo
ggplot(simulation_frame) +
  aes(x = rep_share_house, y = yhat) +
  geom_line(
    data = filter(simulation_frame, republican == 0),
    aes(group = iteration),
    alpha = 0.1, size = 0.1,
    color = "dodgerblue"
  ) +
  geom_line(
    data = filter(simulation_frame, republican == 1),
    aes(group = iteration),
    alpha = 0.1, size = 0.1,
    color = "tomato"
  )


# if you're ever seen papers talk about the Stata package CLARIFY
# this is the same thing

