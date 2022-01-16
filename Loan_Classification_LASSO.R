library(tidyverse)
library(skimr)
library(glmnet)
library(caret)
library(Matrix)
library(ROCR)

loan_data <- read.csv('loan_default_dataset.csv', header = T)

summaryStats <- skim(loan_data)
summaryStats

histogram(loan_data$Credit_score~loan_data$Credit_score)
histogram(loan_data$Age~loan_data$Age)

#Count of default vs. not default (0s and 1s)
ggplot(data = loan_data) +
  geom_bar(mapping = aes(x=Default))

#Create dummy's with model.matrix(response variable ~ . (dot means all), data = data.csv)
loan_predictors_dummy <- model.matrix(Default~., data = loan_data)
loan_predictors_dummy <- data.frame(loan_predictors_dummy[,-1])
loan_data <- cbind(Default=loan_data$Default, loan_predictors_dummy)
#Convert to factor so caret library knows this is classification
loan_data$Default <- as.factor(loan_data$Default)
#levels here is Adjusting 0 or 1 in default to show as default or not
levels(loan_data$Default) <- c("notdefault", "default")

set.seed(11)
index <- createDataPartition(loan_data$Default,p = .8, list = FALSE)
loan_train <- loan_data[index,]
loan_test <- loan_data[-index,]


loan_model <- train(Default~ ., 
                      data = loan_train,
                      method = 'glmnet',
                      #method = 'cv' cv is cross validation
                      trControl = trainControl(method = 'cv',
                                               #number lets the model know how many validation sets to create
                                               number = 5,
                                               classProbs = TRUE,
                                               summaryFunction = twoClassSummary),
                      metric = 'ROC')
loan_model
#Variable importance
plot(varImp(loan_model))
#List coefficients selected
coef(loan_model$finalModel, loan_model$bestTune$lambda)
#Get the predicted probabilities of the test data
predprob_lasso <- predict(loan_model, loan_test, type = 'prob')
#Builds confusion matrix
pred_lasso <- prediction(predprob_lasso$default, loan_test$Default, label.ordering = c("notdefault", "default"))
#tpr and fpr = True positive rate and false positive rate
perf_lasso <- performance(pred_lasso, "tpr", "fpr")
plot(perf_lasso, colorize = TRUE)

#Get the AUC
auc_lasso <- unlist(slot(performance(pred_lasso, "auc"), "y.values"))
auc_lasso
