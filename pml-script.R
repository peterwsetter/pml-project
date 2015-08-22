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


## Download the training data
train.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
train.file <- "train.csv"
if (!exists("train.csv")) {
  download.file(train.url, train.file, method = "curl")
}
train.all <- read.csv(train.file)

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

ss.model <- train(classe ~ ., data = train.ss[-1,], method = "nb")
