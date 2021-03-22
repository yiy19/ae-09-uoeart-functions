------
  title: Write your own R functions
document: github_document
------
  

library(gapminder)
str(gapminder)

### Test your function

## get to know the functions mentioned above
min(gapminder$lifeExp)
max(gapminder$lifeExp)
range(gapminder$lifeExp)

## some natural solutions
# max - min
max(gapminder$lifeExp) - min(gapminder$lifeExp)
# with: The environment has the caller's environment as its parent. 
# This is useful for simplifying calls to modeling functions
# it makes the code run faster (in a gist)
with(gapminder, max(lifeExp) - min(lifeExp))
# range(gapminder$lifeExp)[2] grabs the SECOND element in the vector
# created by range, which returns min and max in a dataframe or a vector
# indexing is faster because it specifically tells the computer what to look for
# instead of search for something
range(gapminder$lifeExp)[2] - range(gapminder$lifeExp)[1]
with(gapminder, range(lifeExp)[2] - range(lifeExp)[1])
diff(range(gapminder$lifeExp))

# function
max_minus_min <- function(x) max(x) - min(x)
max_minus_min(gapminder$lifeExp)

# Test your function
max_minus_min(1:10)
max_minus_min(runif(1000)) 
# I know that 10 minus 1 is 9. I know that random uniform [0, 1] variates will be between 0 and 1. 
# Therefore max - min should be less than 1. If I take LOTS of them, max - min should be pretty close to 1.

# test on real, but different data
max_minus_min(gapminder$gdpPercap)
max_minus_min(gapminder$pop)
max(gapminder$gdpPercap)
min(gapminder$gdpPercap)

# test on weird stuff
max_minus_min(gapminder) ## hey sometimes things "just work" on data.frames!
max_minus_min(gapminder$country) ## factors are kind of like integer vectors, no?
max_minus_min("eggplants are purple") ## i have no excuse for this one

# I will scare you now
max_minus_min(gapminder[c('lifeExp', 'gdpPercap', 'pop')])
# In the first case, a data.frame containing just the quantitative variables is eventually coerced into numeric vector. 
# We can compute max minus min, even though it makes absolutely no sense at all.In the first case, a data.frame containing just the quantitative variables is eventually coerced into numeric vector. We can compute max minus min, even though it makes absolutely no sense at all.
max_minus_min(c(TRUE, TRUE, FALSE, TRUE, TRUE))
# In the second case, a logical vector is converted to zeroes and ones, which might merit an error or at least a warning.

### Check the validity of arguments

# stopifnot
mmm <- function(x) {
  stopifnot(is.numeric(x))
  max(x) - min(x)
}

mmm(gapminder)
mmm(gapminder$country)
mmm("eggplants are purple")
mmm(gapminder[c('lifeExp', 'gdpPercap', 'pop')])
mmm(c(TRUE, TRUE, FALSE, TRUE, TRUE))

# if then not 
mmm2 <- function(x) {
  if(!is.numeric(x)) {
    stop('I am so sorry, but this function only works for numeric input!\n',
         'You have provided an object of class: ', class(x)[1])
  }
  max(x) - min(x)
}

mmm2(gapminder)


# In this part, we generalize this function, 
# learn more technical details about R functions, 
# and set default values for some arguments.

library(gapminder)

mmm <- function(x) {
  stopifnot(is.numeric(x))
  max(x) - min(x)
}

quantile(gapminder$lifeExp)
quantile(gapminder$lifeExp, probs = 0.5)
median(gapminder$lifeExp)
quantile(gapminder$lifeExp, probs = c(0.25, 0.75))
boxplot(gapminder$lifeExp, plot = FALSE)$stats

the_probs <- c(0.25, 0.75)
the_quantiles <- quantile(gapminder$lifeExp, probs = the_probs)
max(the_quantiles) - min(the_quantiles)

# write function
qdiff1 <- function(x, probs) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x = x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}

qdiff1(gapminder$lifeExp, probs = c(0.25, 0.75))

IQR(gapminder$lifeExp) # hey, we've reinvented IQR

qdiff1(gapminder$lifeExp, probs = c(0, 1))

mmm(gapminder$lifeExp)

qdiff1(gapminder$pop, probs = c(0.25, 0.75))

# provide default values 
qdiff3 <- function(x, probs = c(0, 1)) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs)
  max(the_quantiles) - min(the_quantiles)
}

qdiff3(gapminder$lifeExp)
mmm(gapminder$lifeExp)
qdiff3(gapminder$lifeExp, c(0.1, 0.9))

# validity check
qdiff3 <- function(x, probs = c(0, 1)) {
  if(!is.numeric(x)) {
    stop('I am so sorry, but this function only works for numeric input!\n',
         'You have provided an object of class: ', class(x)[1])
  } |
  if(length(probs)!=2) {
    stop('I am so sorry, but this function only works when there two probabilities!\n',
        'You have provided either less than or more thwn two probabilities')
  } |
    if(range(probs)!=c[0, 1]) {
      stop('I am so sorry, but this function only works for probabilities within the range of [0, 1]\n',
           'You have provided probabilities outside the range')
    }
  the_quantiles <- quantile(x, probs)
  max(the_quantiles) - min(the_quantiles)
}

qdiff3(gapminder$lifeExp, probs = c(0,))
qdiff3(gapminder$lifeExp, probs = c(0,1, 2))


qdiff4 <- function(x, probs = c(0, 1)) {
  stopifnot(is.numeric(x)) 
    stopifnot(range(probs) = 1) 
    stopifnot(nrow(probs) = 2)
  the_quantiles <- quantile(x, probs)
  max(the_quantiles) - min(the_quantiles)
}

qdiff3 <- function(x, probs = c(0, 1)) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs)
  max(the_quantiles) - min(the_quantiles)
}

# handle NAs
z <- gapminder$lifeExp
z[3] <- NA
quantile(gapminder$lifeExp)

# when na.rm = FALSE
quantile(z)
# when na.rm = TRUE
quantile(z, na.rm = TRUE)

# default to na.rm = TRUE
qdiff4 <- function(x, probs = c(0, 1)) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs, na.rm = TRUE)
  max(the_quantiles) - min(the_quantiles)
}
qdiff4(gapminder$lifeExp)
qdiff4(z)

# provide argument
qdiff5 <- function(x, probs = c(0, 1), na.rm = TRUE) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs, na.rm = na.rm)
  max(the_quantiles) - min(the_quantiles)
}

qdiff5(gapminder$lifeExp)
qdiff5(z)
qdiff5(z, na.rm = FALSE)


# ... argument

qdiff6 <- function(x, probs = c(0, 1), na.rm = TRUE, ...) {
  the_quantiles <- quantile(x = x, probs = probs, na.rm = na.rm, ...)
  max(the_quantiles) - min(the_quantiles)
}

qdiff6(z, probs = c(0.25, 0.75), type = 1)
qdiff6(z, probs = c(0.25, 0.75), type = 4)

# formal testing: testthat

library(testthat)
test_that('invalid args are detected', {
  expect_error(qdiff6("eggplants are purple"))
  expect_error(qdiff6(iris))
})

test_that('NA handling works', {
  expect_error(qdiff6(c(1:5, NA), na.rm = FALSE))
  expect_equal(qdiff6(c(1:5, NA)), 4)
})

qdiff_no_NA <- function(x, probs = c(0, 1)) {
  the_quantiles <- quantile(x = x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}

test_that('NA handling works', {
  expect_that(qdiff_no_NA(c(1:5, NA)), equals(4))
})



