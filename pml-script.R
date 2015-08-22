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
## Data set contains #DIV/0! for some of its variables
## Set #DIV/0! to NA
train.all <- read.csv(train.file, na.strings = c("NA", "#DIV/0!"))

## Divide the training data into a true training set and a "quiz" set
## "classe" is the A-E description
## "num_window" identifies the particular repitition of the exercise
train <- train.all %>% group_by(num_window, classe) %>% sample_frac(0.7)
quiz <- train.all[-train$X,]

###############
## Filter and select summary statistics
## The creators of the data set calculated numerous summary statistics that
## appear in rows with new_window = 'yes'

## Define unary function to filter & select
SumStats <- . %>% filter(new_window == 'yes') %>% ungroup %>%
  select(classe, contains("max"), contains("min"), contains("kurtosis"),
         contains("skewness"), contains("amplitude"), contains("avg"),
         contains("var"))

train.ss <- SumStats(train)
quiz.ss <- SumStats(quiz)

CV <- function(x){
  return(abs((sd(x)/mean(x))))
}
train.CV <- summarise_each(train.ss, funs(CV))

train.ss.col <- ncol(train.ss)

for (i in train.ss.col:2) {
  if(is.na(train.CV[1,i]) | train.CV[1, i] <= 1) {
    train.ss [i] <- NULL
  }
}

set.seed(2015)
ss.model.rpart <- train(classe ~ ., data = train.ss, method = "rpart")

prdct.ss.rpart <- predict(ss.model.rpart, newdata = quiz.ss)

confusionMatrix(data = prdct.ss.rpart, quiz.ss$classe)

ss.model.rf <- train(classe ~ ., data = train.ss, method = "rf")

prdct.ss.rf <- predict(ss.model.rf, newdata = quiz.ss)

confusionMatrix(data = prdct.ss.rf, quiz.ss$classe)

ss.model.gbm <- train(classe ~ ., data = train.ss, method = "gbm")

prdct.ss.gbm <- predict(ss.model.gbm, newdata = quiz.ss)

confusionMatrix(data = prdct.ss.gbm, quiz.ss$classe)$overall[1]
