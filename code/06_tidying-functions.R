# ----------------------------------------------------
#   PS 811: Lesson 6
#   Data shaping (a.k.a. reshaping, a.k.a. tidying)
#   and functions
# ----------------------------------------------------

# download datasets:
# - Supreme-Court-2000.dta
# - house-primaries-top-two.csv


# ---- packages -----------------------

library("here")
library("tidyverse")



# ---- new dataset -----------------------

# Supreme Court voting in the 2000 term
scotus_raw <- 
  here("data", "Supreme-Court-2000.dta") %>%
  haven::read_dta() %>%
  print()


# 43 cases
# 1 = "liberal" vote. 2 = "conservative" vote.

# why do we encounter data like this?
# imagine a model to predict how justice i votes on case j
# y_{ij} = f( x_{i}, z_{j} )




# ---- tidy data -----------------------

# what is it
# why is it useful
# https://vita.had.co.nz/papers/tidy-data.pdf

# using {tidyr} package (included in tidyverse)




# ---- "wide" and "long" data -----------------------

# "long" vs. "wide" data

# Pretend example w/ fake data
# a two-period, repeated-exposure experiment

n <- 10

# Wide data: repeated concepts across columns
wide <- tibble(
  id = 1:n,
  treatment_1 = sample(c(0, 1), n, replace = TRUE),
  treatment_2 = sample(c(0, 1), n, replace = TRUE),
  outcome_1 = sample(c(0, 1), n, replace = TRUE),
  outcome_2 = sample(c(0, 1), n, replace = TRUE)
) %>%
  print()


# Long data: repeated concepts across rows
long <- wide %>%
  pivot_longer(
    cols = -id,
    names_to = c(".value", "time"),
    names_sep = "_"
  ) %>%
  print()




# https://github.com/mkearney/tidy-animated-verbs/blob/master/images/tidyr-spread-gather.gif





# ---- tidying operations -----------------------

# making data longer :: gathering :: melting

# use pivot_longer()

# (supercedes gather(), melt(), reshape())
nrow(scotus_raw)

# tidyselect helpers
scotus_raw %>% select(Rehnquist:Scalia)
scotus_raw %>% select(starts_with("Sc"))

scotus_tidy <- scotus_raw %>% 
  mutate(case_id = row_number()) %>% 
  pivot_longer(
    cols = -case_id,
    names_to = "justice",
    values_to = "vote"
  ) %>% 
  print()







# making data wider :: spreading :: casting

# use pivot_wider()

# (supercedes spread(), cast(), reshape())

scotus_wide <- scotus_tidy %>% 
  pivot_wider(
    names_from = case_id,
    values_from = vote,
    names_prefix = "case_"
  ) %>% 
  print()


# ---- advanced case: multiple columns -----------------------

# Data on competitive House primary elections
primaries <- 
  here("data", "house-primaries-top-two.csv") %>%
  read_csv() %>%
  print()

# if we wanted to calculate:
# ideological "distance" between candidates
# who won the primary
# ... what would we want out of this data?
primaries %>% 
  select(-cand_id) %>% 
  pivot_wider(
    values_from = c(ideology_score, primary_vote),
    names_from = contestant_id
  )


# when pivoting wider: 
# make sure resulting rows uniquely identify only the cases you want.
# (else you get NAs)



# ----------------------------------------------------
#   Functions
# ----------------------------------------------------


# ---- anatomy of a function -----------------------

# name, arguments, value
# y = f(x)
?cor

# default arguments

# jargon: what are "generics" and "methods"


# ---- writing functions -----------------------

mean2 <- function(x) {
  sum_x <- sum(x)
  n_x <- length(x)
  the_mean <- sum_x / n_x
  message('hey, I calculated a mean')
  return(list(sum = sum_x, n = n_x, mean = the_mean))
}

z <- c(1, 2, 3, 4, 5)

mean2(z)


times2 <- function(z) {
  z * 2
}

times2(z)

# data frame considerations?
# dplyr verbs are actually challenging to use within functions
# because of "tidy evaluation"

# what is an easy way to get variables out of a data frame
# when you're writing a function?
scotus_tidy[["justice"]]

# (this is because all data frames are secretly lists)



# ---- different function types -----------------------

# prefix functions: mean(x)
# infix functions: 1 + 2
# replacement functions: attributes(data) <- vector
# "special" functions: if (...), for (...)
# "functionals": functions that call other functions


# demonstration of "control flow"
if (2 + 2 == 4) {
  print("math makes sense")
} else {
  print("what's going on")
}


# demonstration of "for loop"
v <- vector()

for (i in 1:10) {
  v[i] <- i * 2
}

v

# but we hate for loops


# ---- functionals -----------------------

# apply functions

apply(scotus_raw, MARGIN = 1, FUN = mean)
apply(scotus_raw, MARGIN = 2, FUN = mean)

# data frames are technically "lists"
as.list(scotus_raw)
lapply(scotus_raw, mean, na.rm = TRUE)


# also sapply to "simplify"
# vapply to return "vectors"
# tapply for "ragged arrays" (works like group_by %>% summarize)
# replicate: repeatedly apply 
# mapply: multivariate apply



# supplying functions to arguments in ggplot

scotus_tidy %>%
  group_by(justice) %>%
  summarize(mean_liberal = mean(vote, na.rm = TRUE)) %>%
  ggplot() +
  aes(x = justice, y = mean_liberal) +
  geom_col() +
  scale_y_continuous(
    breaks = seq(from = 0, to = 1, by = 0.1),
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  coord_flip()

# why scale_y_* when it's the horizontal axis?
# why limits = c(0, 1) and not c(0, 100)?






# ---- functionals, anonymous functions, lambda functions --------

# bonus: 
# - verb_all: apply function to all columns
# - verb_if: apply function IF a variable-level predicate is TRUE
# - verb_at: apply function to select columns

scotus_tidy

scotus_tidy %>% rename_all(toupper)
scotus_tidy %>% rename_all(function(x) toupper(x))
scotus_tidy %>% rename_all(~ toupper(.))

# anon functions and lambda functions technically are different things!
class(function(x) toupper(x))
class(~ toupper(.))



scotus_tidy %>% mutate_if(
  .predicate = is.character, 
  .funs = as.factor
)
scotus_tidy %>% mutate_if(is.character, function(x) as.factor(x))
scotus_tidy %>% mutate_if(is.character, ~ as.factor(.))


# with *_at() I like to include argument names always
# you can include MULTIPLE functions
scotus_wide %>% 
  summarize_at(
    .vars = vars(starts_with("case")), 
    .funs = list(mean, sd)
  )

# if you supply a list of functions, you can name the list elements
scotus_wide %>% 
  summarize_at(
    vars(starts_with("case")),
    list(mean = function(x) mean(x),
         sd = function(x) sd(x))
  )

scotus_wide %>% 
  summarize_at(
    .vars = vars(starts_with("case")),
    .funs = list(
      liberal_votes = ~ sum(.),
      conservative_votes = ~ sum(. == 0),
      disposition = ~ case_when(
        mean(., na.rm = TRUE) > 0.5 ~ "liberal",
        mean(., na.rm = TRUE) < 0.5 ~ "conservative",
        mean(., na.rm = TRUE) == 0.5 ~ "no majority"
      )
    )
  )



# ---- rundown of cool functions -----------------------

# coercing object types
as.logical(1)
as.factor(1)
as.character(1)
as.matrix(wide)
as.list(long)
as.list(long) %>% as_tibble()


# {forcats} functions for factors
# - fct_relevel: rearrange factor levels
# - fct_reorder: reorder levels using a function
# - fct_rev: reverse levels
scotus_tidy %>%
  group_by(justice) %>%
  summarize(mean_liberalism = mean(vote, na.rm = TRUE)) %>%
  mutate(
    justice = fct_relevel(justice, "Rehnquist", "Scalia", "Thomas", "Kennedy", "OConnor"),
    # justice = fct_reorder(justice, mean_liberalism),
    # justice = fct_rev(justice)
  )
  ggplot(aes(x = justice, y = mean_liberalism)) +
  geom_col()


# glue!!!
g <- 1
str_glue("combining text with objects like {g}")
as.character(str_glue("combining text with objects like {g}"))
# how would we make "Justice Breyer", "Justice Ginsburg" etc...

# Helpful with plotting and RMarkdown!!!
# scales::number, percent, comma (accuracy = ...)
# english::english (probably need to install)
scales::percent(seq(0, 1, .1))
scales::number(seq(0, 1, .1), accuracy = 0.5)
scales::comma(seq(0, 10000, 1000))
english::english(seq(0, 10000, 1000))


# gtools::mixedsort is your sorting champion
as.character(1:100)
sort(as.character(1:100))
gtools::mixedsort(as.character(1:100))
