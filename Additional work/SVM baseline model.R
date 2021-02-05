packages_needed <- c('SnowballC','slam','tm',
                     'Matrix','rvest',
                     'XML','stringr','stringi',
                     'dplyr','tidyverse', "caret")

for (i in packages_needed){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org", quiet=TRUE)
  require(i, character.only=TRUE)
}

library(caret)
#import Simon Lyngsø Data
df <- read.csv("~/Documents/git/ADS_report/Simon og nicolai/df.csv")

samplesize <- floor(0.7*nrow(df))
set.seed(200)
train_x <- sample(seq_len(nrow(df)),size = samplesize)
train <- df[train_x,]
test <- df[-train_x,]

train$target_rating <- as.factor(train$target_rating)

set.seed(123)
svmModel <- train(
  target_rating ~ ., 
  data = train, 
  method = "svmRadial" #,
  #trControl = trainControl(method = "cv", number = 10, repeats = 5)
)

library(tidyverse)
# Make predictions on the test data
predicted.classes <- svmModel %>% predict(test)
# Compute model accuracy rate
mean(predicted.classes == test$target_rating)

table(pred_class,test$target_rating)
svmModel

