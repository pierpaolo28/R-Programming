library(pacman)

pacman::p_load(pacman,dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate,
               plotly, rio, rmarkdown, shiny, stringr, tidyr, gridExtra, scales)

# IMPORTING WITH RIO #######################################

getwd()
setwd('C:\\Users\\hp\\Desktop\\AI\\R Programming\\Titanic')

# CSV
df <- import("./titanic-train.csv")
head(df)

# View in table format the dataset
View(df)

# List all columns names in the dataset
names(df)

# Inspect the number of observables, features and data types in the dataset
str(df)

# Getting set of descriptive statistics, depending on the type of variable.
# In case of a Numerical Variable -> Gives Mean, Median, Mode, Range and Quartiles.
# In case of a Factor Variable -> Gives a table with the frequencies.
# In case of Factor + Numerical Variables -> Gives the number of missing values.
# In case of character variables -> Gives the length and the class.
summary(df)

# Factors are variables in R which take on a limited number of different 
# values; such variables are often refered to as categorical variables.
df$Survived <- as.factor(df$Survived)
df$Pclass <- as.factor(df$Pclass)
df$Sex <- as.factor(df$Sex)
df$Embarked <- as.factor(df$Embarked)

ggplot(df, aes(x=Survived, fill = Survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y= "Number of Passengers",
       title= "Survival Rate")

prop.table(table(df$Survived)) # cell percentages

ggplot(df, aes(x=Sex, fill= Survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y= "Number of Passengers",
       title= "Survival Rate by Gender")

prop.table(table(df$Survived, df$Sex),1) # row percentages
prop.table(table(df$Survived, df$Sex),2) # column percentages

ggplot(df, aes(x=Pclass, fill=Survived)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Number of Passengers",
       title = "Survival Rate based on Passenger Class")


library(dplyr)

# Subplots using filtered dataset
p1 <-  ggplot(subset(df, df$Pclass == 1), aes(x=Sex, fill=Survived)) +
  theme_bw() +
  ylim(0, 350) +
  labs(title = "Passenger Class 1") +
  geom_bar()
p2 <- ggplot(subset(df, df$Pclass == 2), aes(x=Sex, fill=Survived)) +
  theme_bw() +
  ylim(0, 350) +
  labs(title = "Passenger Class 2") +
  geom_bar()
p3 <- ggplot(subset(df, df$Pclass == 3), aes(x=Sex, fill=Survived)) +
  theme_bw() +
  ylim(0, 350) +
  labs(title = "Passenger Class 3") +
  geom_bar()
grid.arrange(p1, p2, p3, nrow = 1)

# Percentages bar charts
ggplot(df, aes(x= Survived,  group=Pclass)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Survived") +
  facet_grid(~Pclass) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c('Dead','Survived'))

# Count plots with percentages
df %>% 
  group_by(Pclass) %>% 
  summarize(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x=Pclass, y=count)) +
  geom_col() +
  geom_text(aes(label = paste0(round(100 * percent, 1), "%")), vjust = -0.25)

ggplot(df, aes(x=Age, fill=Survived)) +
  theme_bw() +
  geom_histogram(binwidth = 10) +
  labs(y="Number of Passengers",
       x = "Age")

ggplot(df, aes(x = Age, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex~Pclass) +
  geom_histogram(binwidth = 10) +
  labs(y = "Survived", x = "Age")
