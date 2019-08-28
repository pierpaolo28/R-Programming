# There are two different types of packages (libraries) available in R
# Base Packages are installed with R but not loaded by default
# Contributed Packages needs instead to be downloaded, installed and loaded 
# seperatly
# Contributed packages can be installed from: CRAN, Crantastic and GitHub

# Some of the most common R packages are:
# dplyr = manipulating dataframes
# tidyr = cleaning up data
# stringr = for working with strings and text informations
# lubridate = manipulate data information
# httr = working with website data
# ggvis, ggplot2 = interactive visualization
# shiny = to create interactive applications that we can install on websites
# rio = importing and exporting data
# rmarkdown = to create interactive notebooks and rich documents

# pacman (package manager) can be used to install and load multiple packages at
# the same time
install.packages("pacman")
# importing pacman
library(pacman)

pacman::p_load(pacman,dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate,
               plotly, rio, rmarkdown, shiny, stringr, tidyr)

# It is possible to unload contributed packages using: p_unload(all)
# Base packages can instead be unloaded using: detach("package:datasets", unload = TRUE)

library(datasets)

head(iris)
summary(iris)
plot(iris)

# Clear plots window: dev.off()
# Clear console window: cat("\014")

# To get help in R, just type ? and the command you need help with (eg. ?plot)

# The $ operator is the corrispondent of the . operator in Python
plot(iris$Species) #Categorical Variable
plot(iris$Petal.Length) # Quantitative Variable
plot(iris$Species, iris$Petal.Width) # Categorical vs Quantitative
plot(iris$Petal.Length, iris$Petal.Width) # Quantitative vs Quantitative
plot(iris)

# Adding options to plots
plot(iris$Petal.Length, iris$Petal.Width,
     col = "#cc0000",
     pch = 19,
     main = "Iris Dataset: Petal Lenght vs Petal Width",
     xlab = "Petal Lenght",
     ylab = "Petal Width")

# Examples plotting formulas
plot(sin, 0, 2*pi)
plot(exp, 0, 7)
