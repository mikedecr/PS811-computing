# ----------------------------------------------------
#   PS 811: Statistical Computing
#   Spring 2020
#   "Functions you can pull up to"
#   Loops, functional programming, mapping
# ----------------------------------------------------

# The purpose of this lesson is to teach you about 
#   VECTORIZED operations.
# These are routines that will
# - make your code run faster
# - make you a more creative problem solver
# - let you do push around A LOT of data EFFICIENTLY and ELEGANTLY

# we cover:
# - loops
# - apply functions
# - lists, list indexing
# - "scoped" dplyr verbs (if, at, all)
# - anonymous functions, quosure/lambda/purrr functions
# - NESTING and MAPPING (!!!)



# ---- packages and data -----------------------

library("here")
library("tidyverse")
library("broom")

# US House elections data

house <- 
  read_csv(here("data", "house-elections.csv")) %>%
  filter(election_year >= 1952) %>%
  print()

# looking at the relationship between 
#   ideology scores (y) 
#   and district voting for president (x)
# We'd imagine that House members are more LIBERAL
#   if their districts vote more heavily for DEMOCRATS for president

ggplot(house) +
  aes(x = rep_share_pres, y = nominate_dim1) +
  geom_point() +
  geom_smooth(method = "lm", aes(color = party))


# ---- LOOPS -----------------------
  
# For every value i in {1, 2, ..., N}, do some f(i)

# Simple loop to demonstrate main idea
for (j in 1:10) {
  print(j)
}

# The routine LOOPS through the vector 1:10
# - call the element "j"
# - do an operation that involves j
# - repeat until we exhaust the vector


# ---- loops in R...HORRIBLE -----------------------

# you probably don't NEED them
# which is good, because they're famously slow and cumbersome

# "for each row, save the number of columns"
# looping requires me to make a container ahead of time
columns_loop <- numeric()

for (row in 1:nrow(house)) {
  columns_loop[row] <- length(house[row, ])
}


# ---- VECTORIZE your routine -----------------------

# if your routine is just the SAME FUNCTION 
# iterated for every instance
# this will go way faster by applying a "vectorized" function

# apply function "length" for every row (margin = 1) in house
# super speedy
columns <- apply(X = house, MARGIN = 1, FUN = length)




# ---- the "apply functions" -----------------------

# a family of functions for vectorized operations
# I want you to be familiar with the IDEA of these functions.
# Of these functions, lapply() is maybe the only one 
#   I still find useful (and only sometimes)


# apply(): apply a function over rows or columns of a grid/array.
#   Pro: VERY useful for matrix-oriented methods 
#        where every element will be the same data type!
#   Con: Unless you're writing statistical algorithms,
#        it isn't common to work DIRECTLY with matrices
?apply



# lapply(): apply a function over the elements of a list.
#    LISTS!!! They're great but weird.
#    Pro: can contain elements of arbitary type.
#    Con: weird to work with sometimes?

# data frames are lists! Just repackaged
house

house_list <- as.list(house)

# aaah!!
house_list

# preview every element in the list
lapply(house_list, head)

list(a = c(1, 2, 3),
     b = "hello!!!!!",
     d = list)

# the weird part: indexing
str(house_list) 
str(house_list[1]) # what's the data type of the first element of this list?
                   # lol guess again

str(house_list[[1]]) # what's the data type of the data WITHIN the 
                     # first element of the list?
                     # Gotta do this weird double indexing thing

# https://twitter.com/hadleywickham/status/643381054758363136?lang=en

# tapply(): apply a function(s) to groups of data.
#   Common in "dinosaur-style" R.  
#   We HATE tapply. It gives you a stupid array with named rows/cols named array

# horrible...
tapply(
  X = house$nominate_dim1, 
  INDEX = list(house$party, house$election_year), 
  FUN = mean, 
  na.rm = TRUE  
)

# what do we do instead? SO much easier
house %>%
  group_by(election_year, party) %>%
  summarize(
    mean_nom = mean(nominate_dim1, na.rm = TRUE)
  ) 




# ---- enter... functional programming with dplyr/purrr -------------

# vectorization: good
# R's built in vectorizing functions: not consistent w/ our workflow

# two big things we want to learn.
# 1) verb_at, verb_if, verb_all
# 2) group_by() %>% nest() %>% map()




# ---- if, at, all verbs -----------------------

# verbs: mutate, rename, select, filter, transmute, count, ...

# "scoped verbs"
# - if: apply function(s) to variables that match a logical condition
# - at: apply function(s) to a selection of variables
# - all: apply function(s) to every variable



# verb_if()
# example: mutate character variables to factors
# predictate is a function that results in a logical: TRUE or FALSE
house %>%
  mutate_if(
    .predicate = is.character, 
    .funs = as.factor
  )

# .predicate and .funs arguments take objects of type "function"
is.character
class(is.character)
is.character("a")
as.factor("a")

# functions that take function names as arguments like this 
#   are called FUNCTIONALS
#   hence the term "functional programming"

# other examples
select_if(house, is.numeric)




# verb_at()
# example: convert percentage variables to proportions
house %>%
  mutate_at(
    .vars = vars(starts_with("rep_share")),
    .funs = function(x) x / 100
  ) %>% 
  select(starts_with("rep_share"))

# you can DEFINE YOUR OWN FUNCTIONS on the fly
# this is the same basic idea as saving a custom function,
#   only difference being whether you save the function with a name.
make_into_proportion <- function(x) {
  return(x / 100)
}

make_into_proportion(88)

# if you DON'T save the function w/ a name, 
#   and instead you just use it as a one-off routine,
#   this is called an ANONYMOUS FUNCTION

# The other way to do build custom functions on the fly is by doing 
#   what is sometimes called a "lambda function"
#   or a "quosure-style" function
#   or a "purrr-style" function

# Advanced R book

# it works a lot like an anonymous function
# example: select variables that contain NA values
select_if(
  house,
  .predicate = ~ sum(is.na(.)) > 0
)
# function(z) sum(is.na(z)) > 0

# instead of saying function(z), you say `~`
# and then instead of calling z, you call `.`
house %>%
  mutate_at(
    .vars = vars(rep_share_house, rep_share_pres),
    .funs = ~ . / 100
  )

# verb_all()
# this example shows how you can apply multiple functions
# by (1) supplying functions as a list and (2) optionally naming each fx
summarize_all(
  house, 
  .funs = list(uniques = ~ n_distinct(.), 
               obj_type = class)
)


# ---- nesting and mapping -----------------------

# OMG this is the moment I've been waiting for
# This is seriously INSANE.

# One huge benefit of having a data-frame based workflow
#   is the way that it can ORGANIZE your work.

# We're about to make that one level of abstraction up
#  with NESTED DATA FRAMES

# group_by() %>% nest()
# collapses data within a grouping variable
house %>%
  group_by(election_year) %>%
  nest() 

# This is a NESTED data frame.
# It is a data frame that contains data frames (whoa dude...)
# the `data` column is of type LIST. We call this a list-column.
# It's a column that IS a list! Every element in this list is a data frame,
#   but we could have list columns that contain different stuff.

# Why do this? 
# We've already seen that we can create new variables with grouped scope
# summarize variables with grouped scope, etc.
# But these operations have some type restrictions.

# by working with nested data frames, I can create outputs 
#   of essentially ARBITRARY TYPE
# and the results STAY ORGANIZED in the data frame.

# For example, we want to know the relationship between 
#   ideology (y) and district voting (x),
#   BUT it may vary by party and over time.
# So maybe let's estimate a model within each party x year.
# How would we have done this before? Loop over party within year? LAME

nested_house <- house %>%
  group_by(party, election_year) %>%
  nest() %>%
  print()

# make a new column containing model results.
# using the purrr::map() function. (purrr is part of tidyverse)
# map() can be used in a nested data context to map a function 
#   over a list column.

# how it works:
# mutate(new_variable = map(.x = data_column, .f = function))

# This example: call lm() using "quosure style" function.

nested_models <- nested_house %>%
  mutate(
    model = map(
      .x = data, 
      .f = ~ lm(nominate_dim1 ~ rep_share_pres, data = .x)
    )
  ) %>%
  print()


# Can just call function names though.
# Extra function arguments after function name
nested_tidy <- nested_models %>%
  mutate(coefs = map(model, tidy, conf.int = TRUE)) %>%
  print()


# unnest required columns when you're done
coefs <- nested_tidy %>%
  unnest(coefs) %>%
  print()
  
# can't unnest non-data-frame objects but I can pull them out

nested_tidy %>% unnest(model) # hits error
nested_tidy %>% pull(model)   # extracts vector from a data frame
nested_tidy$model             # works the same as this


# unnest directly into plot!
coefs %>%
  filter(term == "rep_share_pres") %>%
  ggplot() +
  aes(x = election_year, y = estimate, color = party) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 1)
  ) 


# other ways you can use nesting and mapping
# reading in a list of data frames...
# install.packages("rio")

tibble(filename = list.files(here("data"))) %>%
  filter(str_sub(filename, -3L, -1L) %in% c("csv", "dta", "rds", "xls")) %>%
  mutate(
    data = map(
      .x = filename, 
      .f = ~ {
        here("data", .x) %>% # file path
        rio::import() %>%          # import data
        as_tibble()                # convert to tidyverse-style DF
      }
    )
  )


# ---- learn more about this stuff -----------------------

# - map functions that return specific output formats (not a list column):
#   map_int, map_dbl, map_chr, map_df
# - map a function with TWO input data columns, map2(), or over
#   an arbitrary number of input columns, pmap()
# - R 4 Data Science: 
#     https://r4ds.had.co.nz/iteration.html
#     https://r4ds.had.co.nz/many-models.html
# - Purrr cheat sheet: 
#     https://github.com/rstudio/cheatsheets/blob/master/purrr.pdf
# - other helpful blog posts
#     https://www.hvitfeldt.me/blog/purrr-tips-and-tricks/
#     https://jennybc.github.io/purrr-tutorial/index.html
# - I have some blog posts where I use map() to do helpful things
#     applying different treatment randomizations to nested datasets: 
#       https://mikedecr.github.io/post/randomization-inference-purrr/
#     applying different model specifications to nested datasets:
#       https://mikedecr.github.io/post/viz-partials/





  