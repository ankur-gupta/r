rm(list = ls())
graphics.off()
set.seed(10)

dplyr <- "ggplot2"
library(ggplot2)
library("ggplot2")
library(dplyr)



pop.df <- expand.grid(state = c("CA", "WI"), year = c(2005, 2015))
pop.df$pop <- c(37, 6 , 39, 6)

subset(pop.df, year == 2015) # year is the column name

x <- 2015
subset(pop.df, year == x) # year is the column name, x is the global variable

# 1 : Basic problem
results.by.year.1 <- function(df, year) {
    df.subset <- subset(df, year == year)
    # Do some computation
    return_value <- nrow(df.subset)
    return(return_value)
}
# Let's say we write a function to perform some computation on this dataframe.
# Basically, for every year we do some computation and return the results.
# In real life, the results will be something more complex but for this example,
# we consider the number of rows in the subsetted dataframe to be the result.

# The basic problem with the code is that we will always get the all the non-NA rows.
#(# Note that this cannot be used to reliably get back all the rows!)
# Note correct result should be 2.
results.by.year.1(pop.df, 2015) # Incorrect result. Silent mistake!



# 2 : Change the variable name
results.by.year.2 <- function(df, yr) {
    df.subset <- subset(df, year == yr)
    # Do some computation
    return_value <- nrow(df.subset)
    return(return_value)
}
# So, we can solve this problem by simply changing the variable name from "year" to "yr".
# And surely enough, we get the correct result.
results.by.year.2(pop.df, 2015) # Correct result




# But this does not solve the problem. More dangerously, this hides the problem!
# Say, you write this function once and use it successfully i your codebase.
# Now, six months later, you reuse this function but this time you use a different dataframe.
# Not only do you have population but also yearly returns in dollars on a $50000 state bond.
# The dataframe looks like this:
pop.df$yr = c(2005, 1000, 3000, 5000)
pop.df$rate = pop.df$yr * 100 / 50000 # Interest rate in percentage
print(pop.df)

# Re-run
# 2 : Change the variable name
results.by.year.2 <- function(df, yr) {
    df.subset <- subset(df, year == yr)
    # Do some computation
    return_value <- nrow(df.subset)
    return(return_value)
}
# So, we can solve this problem by simply changing the variable name from "year" to "yr".
# And surely enough, we get the correct result.
results.by.year.2(pop.df, 2015) # Now the result is incorrect

# So how do we write code that does not depend upon the extra columns?
# In fact, how do we choose the variable name for the second argument of results.by.year.x()?
# We need to check against every possible column name. We cannot do that.


# 3 : Add a test
results.by.year.3 <- function(df, yr) {
    testthat::expect_false("yr" %in% names(df))
    df.subset <- subset(df, year == yr)
    # Do some computation
    return_value <- nrow(df.subset)
    return(return_value)
}
# One quick and dirty way is to simply add a check -- sort of like an assert statement.
results.by.year.3(pop.df, 2015) # Throws error. At least it's not a silent mistake.
# This will throw an error but hey, at least this is not a silent mistake!


# 4 : Don't use subset
results.by.year.4 <- function(df, yr) {
    df.subset <- df[df[["year"]] == yr, ,drop=FALSE]
    # Do some computation
    return_value <- nrow(df.subset)
    return(return_value)
}
# Alternatively, we can not use subset at all. This is the most robust.
results.by.year.4(pop.df, 2015) # Correct answer

# OK, what about the dplyr::filter and dplyr::filter_ functions?
# Hadley Wickham's dplyr package contains filter() function which does the same thing as
# subset(). He provides two functions  --
# the non-standard evaluation (NSE) function dplyr::filter(), and
# the standard evaluation (SE) function dplyr::filter_()
# Do these solve the problem? Let's see.

# Basic examples
# NSE
dplyr::filter(pop.df, year == 2015) # correct result
year <- 2015
dplyr::filter(pop.df, year == year) # incorrect result

# SE - doesn't help
dplyr::filter_(pop.df, "year == 2015") # correct result
year <- 2015
dplyr::filter_(pop.df, "year == year") # incorrect result


# 5: SE with dplyr::filter_()
results.by.year.5 <- function(df, yr) {
    df.subset <- dplyr::filter_(pop.df, "year == yr")
    # Do some computation
    return_value <- nrow(df.subset)
    return(return_value)
}
# Let's look at that example from before.
results.by.year.5(pop.df, 2015) # incorrect answer
# We still get wrong results here.


# There happen to be more pitfalls with these filter functions.
# Let's first look at NSE function call with a string.
dplyr::filter(pop.df, "year == 2015") # throws an error, which is good.
# This throws an error which is good. At least no silent mistake!

# Now let's try SE function with
dplyr::filter_(pop.df, "year == 2015") # correct result
# Correct result! Excellent!

# Now let's use a non-string expression!
year <- 2015
dplyr::filter_(pop.df, year == 2015) # incorrect result. Why? The year == 2015 is evaluated as SE.
# Since year was 2015, you get back all the rows.
dplyr::filter_(pop.df, year == 2005) # incorrect result.


# Final takeaways
# Be careful which functions use non-standard evaluation. Try to use SE instead.
# For subsetting, either use an assert-like statement for the column names at least.
# For programming, permanent code, package writing, use indexing "[" instead!

# This works
yr <- 2015
condition <- lazyeval::interp(~year == x, x = yr)
dplyr::filter_(pop.df, .dots = condition)
dplyr::filter_(pop.df, condition)

results.by.year.6 <- function(df, yr) {
    condition <- lazyeval::interp(~year == x, x = yr)
    df.subset <- dplyr::filter_(pop.df, .dots = condition)
    # Do some computation
    return_value <- nrow(df.subset)
    return(return_value)
}
# Let's look at that example from before.
results.by.year.6(pop.df, 2015) # correct result
# We still get wrong results here.


condition <- lazyeval::interp(~year %in% x, x = c(13.800, 80.23864328, 90.674327, 1/3))
dplyr::filter_(pop.df, .dots = condition)

df <- data.frame(x = seq(1, 10, length = 100))[80:90, , drop = FALSE]
large1 <- 34.8094859837589789592375972379277907239049032 * 8236545343483648 * 523985755234324654
large2 <- large1*1.00000000000000000000001
large1 == large2
df$x[1] <- large
df$x[1] == large
system.time({condition <- lazyeval::interp(~x %in% y, y = c(1001.89, 1:1000000));
dplyr::filter_(df, .dots = condition)})
system.time({df[df$x %in% c(1001.89, 1:1000000), , drop=FALSE]})
# condition = ~year == I(yr)
# dplyr::filter_(pop.df, condition)

# f <- function(.data, ..., .dots) {
# browser()
#  dots <- lazyeval::all_dots(.dots, ...)
#  as.data.frame(filter_(tbl_df(.data), .dots = dots))
# }
# f(pop.df, year == 2015)
# f(pop.df, "year == 2015")



