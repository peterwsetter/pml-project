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
##############
## Function Definitions
CV <- function(x){
    ## Calculate the coefficient of variance
    ## Args:
    ##  x: a numeric vector
    ## Returns:
    ##  coeffcient of variance
    return(abs((sd(x)/mean(x))))
}

## Define unary function to remove rows that contain summary statistics
## since these do not appear in the testing data set
Filtered <- . %>% filter(new_window == 'no') %>% ungroup %>%
    select(-X, -user_name, -contains("time"), -contains("window"), 
           -contains("max"), -contains("min"), -contains("kurtosis"),
           -contains("skewness"), -contains("amplitude"), -contains("avg"),
           -contains("var"), -contains("stddev")) 
##%>%
##    .[complete.cases(.),]

## Unary function for training a model using train.filtered
## Argument is the method 
control.spec <- trainControl(method = 'cv', number = 10)

TrainClasse <- . %>% train(classe ~ ., 
                           data = train.filtered, 
                           trControl = control.spec,
                           method = .)


pml_write_files = function(x){
    ## Writes files for submission to the PML course website
    ## Args:
    ##  x: Character vector
    ## Returns
    ##  A text file containing the character in each index of the vector
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
############
## Download the training data
train.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
train.file <- "train.csv"
if (!file.exists("train.csv")) {
    download.file(train.url, train.file, method = "curl")
}
## Data set contains #DIV/0! for some of its variables
## Set #DIV/0! to NA
train.full <- read.csv(train.file, na.strings = c("NA", "#DIV/0!"))

## Divide the training data into a true training set and a "quiz" set
## "classe" is the A-E description
set.seed(2016)
train <- train.full %>% group_by(classe) %>% sample_frac(0.7)
quiz <- train.full[-train$X,]

###############
## Filter and select summary statistics
## The creators of the data set calculated numerous summary statistics that
## appear in rows with new_window = 'yes'

train.filtered <- Filtered(train)
quiz.filtered <- Filtered(quiz)

#########
## Find the coeffiecient of variance for the variables.
train.CV <- summarise_each(train.filtered, funs(CV))
train.col <- ncol(train.filtered) -1
avg.CV <- rowMeans(train.CV[-train.col], na.rm = TRUE)

## Select (by elimination) columns where the CV is > 1 
for (i in train.col:1) {
    if(train.CV[1, i] <= avg.CV) {
        train.filtered [i] <- NULL
    }
}
##########
##Train models
## CART
model.cart <- TrainClasse('rpart')

## Random forest (randomForest package)
model.rf <- TrainClasse('rf')

## Stochastic Gradient Boosting
model.gbm <- TrainClasse('gbm')

########
## Validate on quiz data
predict.quiz <- predict(model.rf, newdata = quiz.filtered)
confusionMatrix(predict.quiz, quiz.filtered$classe)$overall[1]

#########
## Download testing data
test.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
test.file <- "test.csv"
if (!file.exists(test.file)) {
    download.file(test.url, test.file, method = "curl")
}

## Process the data exactly as it was done for the training data
test <- read.csv(test.file, na.strings = c("NA", "#DIV/0!"))
test.filtered <- Filtered(test)

## Find predictions and write to files for submission
predict.test <- predict(model.rf, test.filtered) %>% as.character
pml_write_files(predict.test)
