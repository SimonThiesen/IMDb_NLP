#ML Modelling part 2

trainData <- read.csv("~/Documents/Kandidat/2 Semester/Data Mining/Churn Prediction/train.csv")
testData <- read.csv("~/Documents/Kandidat/2 Semester/Data Mining/Churn Prediction/test.csv")
View(trainData)

# Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics
library(visdat)   # for additional visualizations

# Feature engineering packages
library(caret)    # for various ML tasks
library(recipes)  # for feature engineering tasks
library(pacman)
p_load(plyr, corrplot)
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(smotefamily)
#install.packages("smotefamily")
#Preprocessing
sapply(trainData, function(x) sum(is.na(x)))

str(trainData)

# Engineering the target feature
trainData$Churn %>%
  hist()
smotefamily::SMOTE(trainData[,-20], trainData[,20])

data_example = sample_generator(10000,ratio = 0.80)
genData = SMOTE(data_example[,-3],data_example[,3])
genData_2 = SMOTE(data_example[,-3],data_example[,3],K=7)
View(trainData)
View(genData$data)
View(genData_2$data)

##SMOTE() to balance target feature distribution

#Check for Normal distribution
trainData$Total.night.minutes %>%
  hist()
trainData$Total.night.minutes %>%
  hist()
trainData$Total.night.minutes %>%
  hist()
trainData$Total.night.minutes %>%
  hist()
trainData$Total.night.minutes %>%
  hist()
trainData$Total.night.minutes %>%
  hist()
trainData$Total.night.minutes %>%
  hist()
trainData$Total.night.minutes %>%
  hist()
#if not ND then transform.
#Log ^2... exp... resiproc

#If ND scale.
#Remember to fit same scale to test set. (VERY IMPORTANT)
scale(trainData$Total.night.minutes)

numeric.var <- sapply(trainData, is.numeric) 

#Plotting correlation matrix for all numeric columns
numeric.var <- sapply(trainData, is.numeric)
corr.matrix <- cor(trainData[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

#removing columns with null, or correlated:
trainData2 <- trainData %>%
  dplyr::select(-Total.night.calls, -Total.day.calls, -Total.eve.calls, -Account.length, -Total.day.minutes, -Total.eve.minutes, -Total.night.minutes, -Total.intl.minutes, -Area.code)
trainData2$Churn <- as.factor(trainData2$Churn)
str(trainData2)
View(trainData2)

#Plotting correlation matrix for the data without correlated columns.
numeric.var2 <- sapply(trainData2, is.numeric)
corr.matrix2 <- cor(trainData2[,numeric.var2])
corrplot(corr.matrix2, main="\n\nCorrelation Plot for Numerical Variables", method="number")

# plotting variables to check for even distribution.
p1 <- ggplot(trainData2, aes(x=Area.code)) + ggtitle("State") + xlab("State") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(trainData2, aes(x=International.plan)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(trainData2, aes(x=Voice.mail.plan)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(trainData2, aes(x=Churn)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)

#Trying to fit a linear and logistic model
lmModel <- lm(Churn ~ ., data=trainData2)
summary(lmModel)
logModel <- glm(Churn ~ ., family = "binomial", data = trainData2)
summary(logModel)
RMSE(lmModel$fitted.values, trainData2$Churn)
RMSE(logModel$fitted.values, trainData2$Churn)

#Checking the contribution for each variable
anova(logModel, test="Chisq")

#Trying a randomforest model.
rfModel <- randomForest(Churn ~., data = trainData2)
print(rfModel)


#Building a logistic model with cross_validation
set.seed(123)
cv_model3 <- train(
  Churn ~ ., 
  data = trainData2, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10, repeats = 5)
)

#Checking model accuracy incl. confusion matrix --precio recall

pred_class <- predict(rfModel, trainData2)
confusionMatrix(
  data = relevel(pred_class, ref = "Yes"), 
  reference = relevel(trainData2$Churn, ref = "Yes")
)

#Making prob for each row
#install.packages("ROCR")
library(ROCR)

m1_prob <- predict(cv_model3, trainData2, type = "prob")["1"]

perf1 <- prediction(m1_prob, trainData2$Churn) %>%
  performance(measure = "tpr", x.measure = "fpr")

plot(perf1, col = "black", lty = 2)

#Trying to tune our model
set.seed(123)
cv_model_pls <- train(
  Churn ~ ., 
  data = trainData2, 
  method = "pls",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 16
)

cv_model_pls$bestTune

cv_model_pls$results %>%
  dplyr::filter(ncomp == pull(cv_model_pls$bestTune))

ggplot(cv_model_pls)

install.packages("vip")
library(vip)
vip(cv_model3, num_features = 10)

testData2 <- testData %>%
  dplyr::select(-Total.night.calls, -Total.day.calls, -Total.eve.calls, -Account.length, -Total.day.minutes, -Total.eve.minutes, -Total.night.minutes, -Total.intl.minutes, -Area.code)
testData2$Churn <- as.factor(mapvalues(testData2$Churn,
                                        from=c("0","1"),
                                        to=c("No", "Yes")))

preds <- predict(rfModel, newdata = testData2)
preds <- as.factor(mapvalues(preds,
                            from=c("No","Yes"),
                            to=c("0", "1")))

View(preds)

submission <- data.frame(Id = 1:length(preds), Churn = preds)
write.csv(submission, file = "testSubmission4.csv", row.names = F)
nrow(submission)
