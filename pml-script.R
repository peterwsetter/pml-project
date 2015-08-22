## Author Peter Setter
## Script for Practice Machine Learning Final Project
## Purpose: Train a model to classify bicep curls as correctly performed (A) or one of four
## common mistakes (B, C, D, E)
## Inputs: Training and testing data from:
## Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. 
## Qualitative Activity Recognition of Weight Lifting Exercises. 
## Proceedings of 4th International Conference in Cooperation with SIGCHI 
## (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.
## Output: 20 text files containing predictions on the test set.

## Library Calls
library(dplyr)
library(caret)
library(knitr)

## Download the training data
train.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
train.file <- "train.csv"
download.file(train.url, train.file, method = "curl")
train.all <- read.csv(train.file)

## Divide the training data into a true training set and a "quiz" set
## "classe" is the A-E description
## "num_window" identifies the particular repitition of the exercise
train <- train.all %>% group_by(num_window, classe) %>% sample_frac(0.7)
quiz <- train.all[-train$X,]

## First Run: Normalized Range
## Issue: Certain columns are only calculated at the end of a rep --> na
NormalRange <- function(x) {
  ## Computes the normalized range of a vector
  ## Args: x a vector
  ## Returns: (max(x) - min(x))/mean(x)
  if(is.factor(x)) {
    x <- as.numeric(x)
  }
  return((max(x)-min(x)/mean(x)))
}

train.nr <- train %>% select(-1, -2, -4, -5, -6) %>% group_by(num_window, classe) %>%
  summarise_each(funs(NormalRange)) %>%
quiz.nr <- quiz %>% select(-1, -2, -4, -5, -6) %>% group_by(num_window, classe) %>%
  summarise_each(funs(NormalRange))

nr.model <- train(classe ~ ., data = train.nr[-1,], method = "rf")

###############
## Filter and select summary statistics
## The creators of the data set calculated numerous summary statistics that
## appear in rows with new_window = 'yes'

## Define unary function to filter & select
SumStats <- . %>% filter(new_window == 'yes') %>% 
  select(classe, contains("max"), contains("min"), contains("kurtosis"),
         contains("skewness"), contains("amplitude"), contains("avg"),
         contains("var"))

train.ss <- SumStats(train)
quiz.ss <- SumStats(quiz)

ss.model <- train(classe ~ ., data = train.ss[-1,], method = "rf")
