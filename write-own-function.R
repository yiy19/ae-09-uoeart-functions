------
  title: Write your own R functions
document: github_document
------
  

library(gapminder)
str(gapminder)

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


