# Some useful statistics analyisis for: 
# 1) Categories = counts (frequencies)
# 2) Quantitive variables = quartiles and mean

library(datasets)

head(iris)

summary(iris$Species) # Categorical variable
summary(iris$Sepal.Length) # Quantitative variable
summary(iris) # Entire dataset


library(pacman)

pacman::p_load(pacman,dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate,
               plotly, rio, rmarkdown, shiny, stringr, tidyr)

p_load(psych)

# How to get infos of packages
p_help(psych) # Opens package documentation in broswer
p_help(psych, web = F) # Opens help in RStudio viewer

describe(iris$Sepal.Length) # Using one quantitive variable
describe(iris) # Using entire dataframe

# Selectors in R

head(iris)
hist(iris$Petal.Length)
summary(iris$Petal.Length)

summary(iris$Species)  # Get names and n for each species

# SELECT BY CATEGORY #######################################
# Versicolor
hist(iris$Petal.Length[iris$Species == "versicolor"],
     main = "Petal Length: Versicolor")
# Virginica
hist(iris$Petal.Length[iris$Species == "virginica"],
     main = "Petal Length: Virginica")
# Setosa
hist(iris$Petal.Length[iris$Species == "setosa"],
     main = "Petal Length: Setosa")

# SELECT BY VALUE ##########################################
# Short petals only (all Setosa)
hist(iris$Petal.Length[iris$Petal.Length < 2],
     main = "Petal Length < 2")

# MULTIPLE SELECTORS #######################################
# Short Virginica petals only
hist(iris$Petal.Length[iris$Species == "virginica" & 
                         iris$Petal.Length < 5.5],
     main = "Petal Length: Short Virginica")

# CREATE SUBSAMPLE #########################################
# Format: data[rows, columns]
# Leave rows or columns blank to select all
i.setosa <- iris[iris$Species == "setosa", ]

# EXPLORE SUBSAMPLE ########################################
head(i.setosa)
summary(i.setosa$Petal.Length)
hist(i.setosa$Petal.Length)