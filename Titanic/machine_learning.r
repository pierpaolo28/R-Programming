library(pacman)

# .rs.restartR() Restart Kernel Session

# Data Preprocessing
pacman::p_load(pacman,tidyverse, forcats, stringr, caTools)

# Data Visualizations
pacman::p_load(DT,data.table, pander, ggplot2, scales, grid, gridExtra, corrplot, VIM, knitr, vcd, caret)

# Machine Learning
pacman::p_load(xgboost,MLmetrics, 'randomForest', 'rpart', 'rpart.plot', 'car', 'e1071', pROC, ROCR, knitr, glmnet)

train <- read_csv('./titanic-train.csv')

test  <- read_csv('./test.csv')

dim(train)

dim(test)

train$set <- "train"

test$set  <- "test"

test$Survived <- NA

df <- rbind(train, test)


# Checking data
str(df)


# Dataset dimensions
dim(df)


# Unique values per column
lapply(df, function(x) length(unique(x))) 


# Checking for Missing values
missing_values <- df %>% summarize_all(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  
  geom_bar(stat="identity",fill="red")+
  
  coord_flip()+theme_bw()

df$Embarked
df$Embarked <- as.character(df$Embarked)
df$Embarked[is.na(df$Embarked)] <- "S"
sapply(df, function(x) {sum(is.na(x))})
df$Embarked

# Create a new variable which contains siblings, spouse and individuals
df$Family.size <- df$SibSp + df$Parch + 1

# Finding median of Fare
fare.median <- median(df$Fare, na.rm = T)

# Assigning a value to blank rows
df[is.na(df$Fare), "Fare"] <- fare.median

# modeling for age
age.formula <- "Age ~ Pclass + Sex"
age.model <- lm(
  formula = age.formula,
  data = df
)
age.row <- df[is.na(df$Age), c("Pclass", "Sex")]

# predict age for NA filled rows
age.predict <-predict(age.model, newdata = age.row)

# assign value of age into combined dataset
df[is.na(df$Age), "Age"] <- age.predict

df$Pclass <- as.factor(df$Pclass)
df$Sex <- as.factor(df$Sex)
df$Embarked <- as.factor(df$Embarked)

# Splitting dataset
train_df <- df[1:891,]
test_df <- df[892:1309,]

# Getting Survived data in levels
train_df$Survived <- as.factor(train_df$Survived)

# Set a random seed
set.seed(100)

# Machine Learning Prediction using Random Forest 
model_elements <- as.formula("Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family.size")

model <- randomForest(formula = model_elements, data = train_df, ntree = 500, mtry = 3, 
                              nodesize = 0.01 * nrow(test_df),  keep.forest = TRUE)

# Prediction 
survived_preds <- predict(model, newdata = test_df)

# new data set with two variables
PassengerId <- test_df$PassengerId
preds <- as.data.frame(PassengerId)
preds$survived_preds <- survived_preds
table(preds$survived_preds)
