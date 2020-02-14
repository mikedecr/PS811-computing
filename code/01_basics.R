# ----------------------------------------------------
#   Getting up and running with R
#   
#   The purpose of this file is 
#     to familiarize you with R's INTERFACE
#     and basic features of the language
#   
#   Let's speed through this
#     so we can get to REAL DATA.
#   
# ----------------------------------------------------



# start a new R session
# (always in your intended project)

# remember: 
# "super + enter" sends commands from SOURCE to CONSOLE
# ( "super" = "Cmd" if Mac.
#   "super" = "ctrl" if Windows )

# ( if you haven't already noticed,
#   you can comment out text with `#` )



# ---- R is a calculator -----------------------

1 + 1


5 - 4


# white space doesn't matter
# but pay attention to good/bad style

3*2      # "bad style" (cramped)
3 * 2    # good style


# if a statement isn't complete, 
#   R will search next line
12 / 
 4

# This will be valuable for writing code
#   with good style later!


# order of operations works as it should

4^(20 / 2)


# euler's consant and natural log
exp(1)
log(1)


pi
cos(pi)





# ---- "Assigning" a.k.a. storing results -----------------------

# greather than, plus a hyphen
# (just use "alt + hyphen")
some_nums <- 6789

# assigning with `=` does work, 
#   but stylistically it isn't recommended
also_nums = some_nums


# allowed in names:
# - underscores (as you see above)
# - numbers (just not at the beginning)
# - periods
# - names are case sensitive!



# ---- evaluation quirks -----------------------

# we mentioned this last week...

# assigning takes the form 
# result <- statement

# R evaluates everything in statement BEFORE assignment
# "result" does not remember "statement"
# it only remembers the RESULT of "statement"

# demonstration:
a <- 3
b <- a + 2
a <- 9

# will `b` be 5? or 11?
b






# ---- Everything is an "object" -----------------------

# what does this mean? 

# 1) EVERYTHING can be assigned with a name

# including character data (aka "strings")
my_name <- "Michael DeCrescenzo"


# 2) results from operations can ALWAYS be passed to other operations
#    (as long as the outputs/inputs conform).

# meaning, if f() returns a NUMBER, 
#   and g() is a function of NUMBER,
#   then I can do g(f(x)) as much as I want

# This is a HUGE advantage of R over other platforms

# this is horrible but it demonstrates the point
# result = 1
omg_why <- length(sum(c(1, 2, 3) * 3)) + log(1)

# what is the  `omg_why`th element  of object `my_name`?
my_name[omg_why]

# this result is also an object





# ---- vectors -----------------------

# vectors are a generalization of "variable"
# they take potentially multiple values
ex_vec <- c(0, 1, 1, 1, 0, 0, 1, 1)

dvec <- 2 * ex_vec

# multiplication of vectors is ELEMENT-WISE product
# (aka "hadamard product")
dvec * dvec 

# if you want a "dot-product" aka "matrix multiplication"
# (don't know what that is? it's fine)
dvec %*% dvec


# vectors coerce everything to be the same data type
# e.g. this is coerced to character data
c(1, 2, 3, "four")


# CREATE YOUR OWN VECTOR
# - use c(), 
# - make it NUMERIC
# - assign it to a name






# ---- functions -----------------------

# function name, followed by parentheses

mean(ex_vec)
sd(ex_vec)
sum(ex_vec)

# length = "how many elements" 
length(ex_vec)


# You can write your own functions 
#   (we'll learn more about this later)

# a function is an object...so you give it a aname
another_sd <- function(x) {
  
  n <- length(x)
  variance <- sum((x - mean(x))^2) / (length(x) - 1)
  result <- sqrt(variance)
  return(result)

}

another_sd(ex_vec)


# asking R for help
help(sd)
?sd






