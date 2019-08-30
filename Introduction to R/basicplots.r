#########################################################
# BARPLOTS

load(datasets)

?mtcars

head(mtcars)

# Barplots can be used to dysplay categorical variables
cylinders <- table(mtcars$cyl)
barplot(cylinders)

#########################################################
# HISTOGRAMS

# Histograms can be used to visualize 
# quantitive data (scaled, measured, interval or ratio level).
# Using instograms, we can find out more about four different 
# characteristics of our data: Shape, Gaps, Outliers, Symmetry.

head(iris)
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)

#########################################################
# GROUP OF GRAPHS

# Creating groups of graphs
par(mfrow = c(3, 1)) # 3 rows, 1 column of graphs
# In this case, I am going to create an histogram for each of 
# the different species
hist(iris$Petal.Width [iris$Species == "setosa"], # This parameter here
                                                  # is called selector
     xlim = c(0,3),
     main = "Petal Width for Setosa",
     xlab = "",
     col = "red")
hist(iris$Petal.Width [iris$Species == "versicolor"],
     xlim = c(0,3),
     main = "Petal Width for Versicolor",
     xlab = "",
     col = "purple")
hist(iris$Petal.Width [iris$Species == "virginica"],
     xlim = c(0,3),
     main = "Petal Width for Virginica",
     xlab = "",
     col = "blue")

# Restoring graphical parameters
par(mfrow = c(1, 1))

#########################################################
# SCATTERPLOTS

# Scatterplots are used to analyse bivariate distributions
# (visualizing the association between two quantitive variables)
# When using scatterplots we can find out different characteristics 
# about our data: 
# 1) we can see if the relationship between the two variables
# is linear (straight line)
# 2) check if there is a consistent spread between the two variables
# 3) check for outliers
# 4) check if there is any correlation between the variables

head(mtcars)

# Let's start by looking at the unvariate distributions
hist(mtcars$wt)
hist(mtcars$mpg)

# Bivariate distributions
plot(mtcars$wt, mtcars$mpg)

#########################################################
# OVERLAYING PLOTS

# It is usefult to create overlaying plots when working with data
# that complement and support eachother (they are not competing)

?lynx
head(lynx)

hist(lynx,
     breaks = 14,
     freq = FALSE,
     col = "thistle1", # Color of the curve
     main = "Histogram of Annual Canadian Lynx Trappings, 1821-1934",
     xlab = "Number of Lynx Trapped")

# We are now going to overlay on our histogram a plot with a normal 
# distribution having the same properties as our data
curve(dnorm(x, mean = mean(lynx), sd = sd(lynx)),
      col = "thistle4", 
      lwd = 2, # Line width of 2 pixels
      add = TRUE) # Superimpose on previous graph 

# Adding Kernel Density Estimators
lines(density(lynx), col = "blue", lwd = 2)
# Making our Kernel averaging across (moving average)
lines(density(lynx, adjust = 3, col = "purple" , lwd = 2))

# Rug plot (adds lots of small vertical lines for each individual
# datapoint underneath our plot)
rug(lynx, lwd = 2, col = "gray")
