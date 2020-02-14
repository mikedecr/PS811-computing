# ----------------------------------------------------
#   DATA FRAMES, the essential unit REAL DATA
#   
#   This file introduces the TIDYVERSE
#     and the "dplyr" family of data manipulation "verbs"
# ----------------------------------------------------

# start a new R session

# always load packages FIRST

library("here")       # file paths
library("magrittr")   # pipe operators!
library("tidyverse")  # workhorse package


# I'll sometimes refer to functions 
#   by their package name using this syntax:
#   package::function() 



# ---- read data -----------------------

# download `HSall_members.csv` from Canvas.
# Save to `data` folder

# in order to import the data, 
#   we need to tell R where it is!
# as in, "import the file called folder/folder/data.xyz"
# BUT, how do we build the path to folder/folder/data.xyz?

# we will use a system built on the {here} package

# where is project root?
here()


# if you build file paths with here(), 
#   then all file searching starts at project root.
# This is GOOD.

# import a CSV with read_csv()
federal <- read_csv(here("data", "HSall_members.csv"))

# path to the file, translated:
# - start here(), 
# - go into "data" folder
# - grab "HSall_members.csv"

# use R to view names of files and folders
list.files()             # in current directory
list.files(here())       # same result
list.files(here("data")) # in root/data folder...etc.


# read_csv() imports CSV data file.
# it comes from the readr package (part of tidyverse)
# it is BETTER than read.csv(), which is older/different!


# print a data preview
federal


# ---- data frames -----------------------

# - rectangular table of data
# - each ROW is an observation
# - each COLUMN is a variable

# what are the variable names?
names(federal)

# learn about all variables at once
# especially helpful for NUMERIC variables

summary(federal) # kinda ugly


# how do I access the "chamber" variable?

chamber             # NO
federal$chamber    # YES


# The data$variable syntax is annoying, BUT...
#   Tidyverse functions get around this.


# ---- diatribe about attach() -----------------------

# You MAY have learned to attach() a dataset in the past.
# No offense to your old instructors, 
#   but attaching is a DANGEROUS practice,
#   and you should NEVER do it.

# "I will come into your house and set your computer on fire"



# ----------------------------------------------------
#   The data manipulation "verbs" (dplyr)
# ----------------------------------------------------

# the following functions come from the "dplyr" package 
# it is a part of the tidyverse (is bundled therein)

# why call them "verbs?"
# because the functions are organized around what you DO to data

# the VERBS
# - select(): pick COLUMNS (variables) from a data frame
# - filter(): pick ROWS (observations) from a data frame
# - rename(): rename variables in a data frame
# - mutate(): add/change variables in a data frame
# - count(): tabulate variables in a data frame
# - summarize(): generate summary stats from a data frame
# - group_by(): partition a data frame into multiple groups 
# - arrange(): sort the rows of a data frame
# - and more...but these are the main ones

# HOW THEY WORK:
# - the first argument is ALWAYS name of the data frame
# - lets you work w/ variables without annoying data$variable syntax


# AN EXAMPLE: remember the federal data?
federal

# let's rename "icpsr" (identifier code) to "legislator_icpsr"
rename(federal, legislator_icpsr = icpsr)

# started with a data frame (federal)
# return a new data frame (with different names)


# try it yourself with a different variable ------




# ------------------------------------------------



# REMEMBER: must save results if you want to come back to them
federal_renamed <- rename(federal, legislator_icpsr = icpsr)

# it is NOT always smart to save over your original data
# (you could make a mistake)

# we will learn cool stuff to get around this...!!!!





# ----------------------------------------------------
#   Here's how we're going to do this
# ----------------------------------------------------

# 1. Briefly overview the functionality of key verbs
# 2. Show how to combine them using the PIPE OPERATOR
# 3. Keep exploring functionality/workflow 
#    of verbs and pipes









# ---- select() columns -----------------------


# how to keep specified variables
select(federal, bioname, state_abbrev)

# how to DROP specified variables (with minus sign)
select(federal, -party_code, -chamber)


# save a slimmer dataset for later
slim_federal <- select(
  federal,
  congress,
  chamber, 
  state_abbrev,
  party_code,
  bioname, 
  nominate_dim1
)

# remember...it's a data frame!
slim_federal




# ---- filter() rows -----------------------


# filter() is like "keep if..."

# example: keep if state is Wisconsin
filter(slim_federal, state_abbrev == "WI")

# you supply a LOGICAL STATEMENT 
#   (i.e., evaluates to TRUE or FALSE)
#   (these are sometimes called "predicates")


# examples of logical statements

"a" == "b"     # "is equal to" (DOUBLE EQUALS)

"a" != "b"     # "is not equal to"

2 > 1          # greater than
2 >= 1         # greater than or equal to

2 < 1          # less than
2 <= 1         # less than or equal to


# filter() keeps the rows where the statement is TRUE
filter(slim_federal, party_code == 100)


# keep only most recent Congress, no presidents
this_congress <- 
  filter(
    slim_federal, 
    congress == max(congress),
    chamber != "President"
  )


# ---- mutate() (add/change) variables -----------------------

# can create/modify several new variables at once

mutate(
  this_congress,
  bioname = toupper(bioname),
  congress_year = 1787 + (congress * 2),
  election_year = congress_year - 1
)




# ---- summarize() a data frame -----------------------

# returns a ONE-ROW data frame of summary stats
# (functions must return a 1-length result!)

summarize(
  this_congress,
  mean_ideology = mean(nominate_dim1),
  sd_ideology = sd(nominate_dim1)
)


# how to skip NAs in common functions
summarize(
  this_congress,
  mean_ideology = mean(nominate_dim1, na.rm = TRUE),
  sd_ideology = sd(nominate_dim1, na.rm = TRUE)
)



# ----------------------------------------------------
#   The PIPE OPERATOR
# ----------------------------------------------------

# This is where things get even more sick...


# suppose we have a function, f()
# it takes argument x

# we write this as f(x), or as...

# x %>% f()

# says "take object x and pass it to function f()"



# %>% is called the "pipe operator" 
#   originally debuted in the {magrittr} package
#   (~*~*~*~*~   ceci n'est pas une pipe: %>%   ~*~*~*~*~)







# these do the same thing:

names(this_congress)
this_congress %>% names()

# pipe operator assumes that the piped object 
#   should be the FIRST ARGUMENT to f()

# (we will later violate this assumption)


# try it yourself! 
# lay pipe with shortcut "super + shift + m"






# You may be wondering: 
# why on Earf would I want to do this?

# The data manipulation "verbs" are designed to be piped!
# They all take a data frame as the FIRST ARGUMENT.
# They all return a data frame.

# This lets us use pipe operator
#   to combine multiple operations
#   in one concise "chain"


# example:

slim_federal <- federal %>%
  select(
    congress, chamber, state_abbrev, 
    party_code, bioname, nominate_dim1
  ) %>%
  filter(chamber != "President") %>%
  print()

# start with `federal`, THEN
# - select certain variables, THEN
# - rename a variable, THEN
# - assign the result to `slim_federal`...
# - (and print for your visual benefit!)




# Why is this good?

# - EFFICIENT! do a lot with fewer keystrokes
# - READABLE! the VERBS "say" what each step does
# - LINEAR! order of operations no longer a P.I.T.A.
#     why do 
#       j(i(h((g(f(x)))))),
#     when I could do 
#       x %>% f() %>% g() %>% h() %>% i() %>% j()
#     <https://twitter.com/dmi3k/status/1191824875842879489>



# retrace steps to "this congress" 

this_congress <- federal %>%
  filter(congress == max(congress)) %>%
  filter(chamber != "President") %>%
  mutate(
    bioname = toupper(bioname),
    congress_year = 1787 + (congress * 2),
    election_year = congress_year - 1
  ) %>%
  select(
    congress, congress_year, election_year,
    bioname, chamber, state_abbrev, party_code, nominate_dim1
  ) %>%
  print()




# ---- "good style" -----------------------

# _Tidyverse style guide_ chapter on pipe operators: 
#   https://style.tidyverse.org/pipes.html#introduction
# R 4 Data Science: "When not to use the pipe"
#   https://r4ds.had.co.nz/pipes.html#when-not-to-use-the-pipe









# ----------------------------------------------------
#   Putting Pipes to USE
# ----------------------------------------------------


# ---- group_by() -----------------------

# implicitly partitions a data frame

slim_federal

slim_federal %>% group_by(congress)


# This is useful for doing 
#   mutate() or summarize() within groups


# average NOMINATE (d1) score 
slim_federal %>%
  summarize(
    avg_nom_dim1 = mean(nominate_dim1, na.rm = TRUE)
  )  

# same, but for each congress x chamber x party group
slim_federal %>%
  group_by(congress, chamber, party_code) %>%
  summarize(
    avg_nom_dim1 = mean(nominate_dim1, na.rm = TRUE)
  )  


# calculate a "relative NOMINATE score"
# which is a deviation from the average
slim_federal %>%
  mutate(
    rel_nominate_dim1 = nominate_dim1 - mean(nominate_dim1, na.rm = TRUE)
  )

# now within congress x party
slim_federal %>%
  group_by(congress, party_code) %>% 
  mutate(
    rel_nominate_dim1 = nominate_dim1 - mean(nominate_dim1, na.rm = TRUE)
  )



# you can also filter with grouped data

# e.g. get most liberal and conservative member of every congress x chamber
slim_federal %>%
  group_by(congress, chamber) %>%
  filter(nominate_dim1 == min(nominate_dim1, na.rm = TRUE))



# ungroup() will ungroup a data frame (natch...)



# ----------------------------------------------------
#   Discuss Skills Task 2!!
# ----------------------------------------------------




# ---- more mutate() -----------------------


# We often want to RECODE data.
#   If old_variable is [something],
#   make new_variable [something].
# the case_when() function is good for this
#   which is a flexible if/else command.

# party_codes: 100 is Dem., 200 is Rep., 328 is Ind.

this_congress %>%
  mutate(
    party = case_when(
      party_code == 100 ~ "Democrat", 
      party_code == 200 ~ "Republican", 
      party_code == 328 ~ "Independent", 
  )
)

# (watch your parentheses)

# Translated:
# in a variable called party...
# - if party_code is 100, it gets the value "Democrat"
# - if party_code is 200, it gets ... etc.



# NOTE: if there are unmatched rows, they default to NA
# observe:
this_congress %>%
  mutate(
    is_senator = case_when(chamber == "Senate" ~ "Yes") 
  )

# how to create a catch-all category
this_congress %>%
  mutate(
    is_senator = case_when(
      chamber == "Senate" ~ "Yes",
      TRUE ~ "No"
    )
  )


# try yourself! Create a "major party" variable 
# 1 if Democrat OR Republican
# 0 otherwise











# ---- more select() -----------------------

# there is a family of convenience functions 
#   for selecting variables
#   called "tidyselect"

federal 

# select a range of variables...
select(federal, congress:state_icpsr)

# variable name contains a substring
select(federal, contains("icpsr"))

# variable name starts with a substring
select(federal, starts_with("nominate"))

# variable name ends with a substring
select(federal, ends_with("dim1"), ends_with("dim2"))

# you can also negate all of these 
select(federal, -contains("nokken_poole"))

# everything() is catch all for "and everything else"
select(federal, bioname, nominate_dim1, everything())




# ---- filter -----------------------

# combining logical statements with & ("and") and | ("or")
slim_federal %>%
  filter(state_abbrev == "ND" | state_abbrev == "SD")

# what's going on here?
slim_federal %>%
  filter(state_abbrev == "ND" & state_abbrev == "SD")


# quiz yourself: 
# if 100 is the current "Democrat" party_code,
# and 200 is the current "Republican" party_code,
# write a filter() statement that keeps neither Ds nor Rs

this_congress %>%  # ...





# note to MIKE:  do this with %in% 







# an IMPORTANT point about missing data

# NA is R's code for "missing data."
# It means...
#   "there should be a value here, 
#    but I don't know what it is"

# When you ask R to do things with NA values,
# often R's result is NA, which means "I don't know"
sum(1, 2, 3, NA)

# this is why we had to do `na.rm = TRUE` above

# when we filter, 
#   R doesn't know if anything is "equal to" NA
NA == NA

# but you can still check if something is NA
is.na(NA)
is.na(4)

# This is how you can filter with NA values
filter(slim_federal, is.na(nominate_dim1))



# ---- rename() variables -----------------------

# this is pretty easy
# we've already seen it

# BUT know that we can rename multiple variables at once
rename(
  this_congress,
  congress_num = congress,
  institution = chamber
)



# ---- count() -----------------------

# understated in its usefulness, 
#   count() tabulates variables

this_congress %>%
  count(party_code)

# can do multiples
this_congress %>%
  count(chamber, party_code)

# which has the same effect as grouping
this_congress %>%
  group_by(chamber) %>%
  count(party_code)

# the grouped method is advantageous
# because it makes it easier to add a "total" column
this_congress %>%
  group_by(chamber) %>%
  count(party_code) %>%
  add_tally(name = "total") 

# but doing it the long way also works
# this works because data STILL GROUPED 
this_congress %>%
  group_by(chamber) %>%
  count(party_code) %>%
  mutate(total = sum(n))



# ---- arrange -----------------------

# sort a data frame

# default, ascending order
this_congress %>%
  arrange(nominate_dim1)

# can do descending order
this_congress %>%
  arrange(desc(nominate_dim1))

# and functions of columns
this_congress %>%
  arrange(desc(abs(nominate_dim1))) %>%
  print(n = 20)





# ---- making data frames -----------------------

# create data frames from scratch 
# column by column w/ tibble()
# it works a lot like mutate()

states <- 
  tibble(
    state = state.name,
    state_abb = state.abb,
    region = state.region
  ) %>%
  print()


# row-by-row with tribble()
tribble(
  ~ name, ~ home_state, ~ weakness,
  "Michael", "Missouri", "back pain",
  "Micah", "Ohio", "nearsighted"
)




# ---- pull -----------------------

# like select, but result is a VECTOR
this_congress %>%
  pull(nominate_dim1)

# why? 
this_congress %>%
  pull(nominate_dim1) %>%
  mean(na.rm = TRUE)

# sometimes the old way is fine
mean(this_congress$nominate_dim1, na.rm = TRUE)



# ---- Other pipey tools -----------------------

# The {tidyverse} package imports the pipe operator
#   so we don't need to library("magrittr") as we did above.

# However! {magrittr} contains one other valuable operator 
#   that {tidyverse} does not contain,
#   so I like to load it explicitly.

# That operator is the "exposition pipe" %$% 

# the exposition pipe passes forward ONLY VARIABLE NAMES
#   (not the whole data frame)
this_congress %$% mean(nominate_dim1, na.rm = TRUE)

# You don't see %$% as often in the wild, 
#   but I like using it.

