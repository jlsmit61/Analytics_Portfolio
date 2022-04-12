library(tidyverse)
library(caret)

Rejects_Data <- (read.csv(file = 'Rejects_Analysis.csv', header = T))
#Remove hidden column
Rejects_Data <- select(Rejects_Data, -ï..PROCESS_ID)


library(skimr)
summaryStats <- skim(Rejects_Data)
summaryStats

#Clean missing values from DocuSign attachments validation
#Rejects_Data$DocuSign_Used <- ifelse(Rejects_Data$DocuSign_Used == "N/A", "Nothing to Sign", Rejects_Data$DocuSigned_Used)
library(lubridate)
#Clean dates
Rejects_Data <- mutate(Rejects_Data, Start_Month = month(as.Date(Rejects_Data$Start_Date, '%m/%d/%Y')),
                                     Start_Day = day(as.Date(Rejects_Data$Start_Date, '%m/%d/%Y')),
                                     Start_Year = year(as.Date(Rejects_Data$Start_Date, '%m/%d/%Y')),
                                     End_Month = month(as.Date(Rejects_Data$End_Date, '%m/%d/%Y')),
                                     End_Day = day(as.Date(Rejects_Data$End_Date, '%m/%d/%Y')),
                                     End_Year = year(as.Date(Rejects_Data$End_Date, '%m/%d/%Y')))

Rejects_Data$Start_Date <- NULL
Rejects_Data$End_Date <- NULL
Rejects_Data$End_Year <- NULL
Rejects_Data$Start_Year <- NULL
Rejects_Data$DocuSign_Used <- ifelse(Rejects_Data$DocuSign_Used == "false", "FALSE", Rejects_Data$DocuSign_Used)
Rejects_Data$DocuSign_Used <- ifelse(Rejects_Data$DocuSign_Used == "true", "TRUE", Rejects_Data$DocuSign_Used)


#Remove end dates that haven't occured yet
Rejects_Data$End_Month <- ifelse(Rejects_Data$End_Month > 3, NA, Rejects_Data$End_Month)
Rejects_Data$End_Month <- ifelse(is.na(Rejects_Data$End_Month), na.omit(Rejects_Data$End_Month), Rejects_Data$End_Month)

Rejects_Data$DaysBin <- c()
Rejects_Data$DaysBin[Rejects_Data$End_Day - Rejects_Data$Start_Day < 2] <- "Same Day"
Rejects_Data$DaysBin[Rejects_Data$End_Day - Rejects_Data$Start_Day >=2 & Rejects_Data$End_Day - Rejects_Data$Start_Day <=5] <- "2 to 5 Days"
Rejects_Data$DaysBin[Rejects_Data$End_Day - Rejects_Data$Start_Day >=6 & Rejects_Data$End_Day - Rejects_Data$Start_Day <=14] <- "Over 1 Week"
Rejects_Data$DaysBin[Rejects_Data$End_Day - Rejects_Data$Start_Day >=15 & Rejects_Data$End_Day - Rejects_Data$Start_Day <=21] <- "Over 2 Weeks"
Rejects_Data$DaysBin[Rejects_Data$End_Day - Rejects_Data$Start_Day >=22 & Rejects_Data$End_Day - Rejects_Data$Start_Day <=28] <- "Over 3 Weeks"
Rejects_Data$DaysBin[Rejects_Data$End_Month - Rejects_Data$Start_Month >= 1] <- "Over 1 Month"

#New Feature: Multiple attachments, DS used
Rejects_Data$DsAttachments <- c()
Rejects_Data$DsAttachments[Rejects_Data$Attachments > 1 & Rejects_Data$DocuSign_Used == TRUE] <- 1
Rejects_Data$DsAttachments <- ifelse(is.na(Rejects_Data$DsAttachments), 0, Rejects_Data$DsAttachments)

Rejects_Data$DS_Expiration_SameDay <- ifelse(Rejects_Data$DocuSign_Used == "TRUE" & Rejects_Data$DaysBin ==  "Same Day", 1 ,0)
Rejects_Data$DS_Expiration_Days <- ifelse(Rejects_Data$DocuSign_Used == "TRUE" & Rejects_Data$DaysBin ==  "2 to 5 Days", 1 ,0)
Rejects_Data$DS_Expiration_1wk <- ifelse(Rejects_Data$DocuSign_Used == "TRUE" & Rejects_Data$DaysBin ==  "Over 1 Week", 1 ,0)
Rejects_Data$DS_Expiration_2wks <- ifelse(Rejects_Data$DocuSign_Used == "TRUE" & Rejects_Data$DaysBin ==  "Over 2 Weeks", 1 ,0)
Rejects_Data$DS_Expiration_3wks <- ifelse(Rejects_Data$DocuSign_Used == "TRUE" & Rejects_Data$DaysBin ==  "Over 3 Weeks", 1 ,0)
Rejects_Data$DS_Expiration_1_Month <- ifelse(Rejects_Data$DocuSign_Used == "TRUE" & Rejects_Data$DaysBin ==  "Over 1 Month", 1 ,0)

Rejects_Data$Process <- ifelse(Rejects_Data$NAME == "AACR Approval Subprocess", 'AACR',
                        ifelse(Rejects_Data$NAME == "AACR Manual Update Process", "AACR", 
                        ifelse(Rejects_Data$NAME == "Account Documentation Submission Process", "ADS",
                        ifelse(Rejects_Data$NAME == "Account Retitling Process", "Acct Retitle",
                        ifelse(Rejects_Data$NAME == "ACH Instructions", "AINS", 
                        ifelse(Rejects_Data$NAME == "Advisory Goal Tool Process", "AGT",
                        ifelse(Rejects_Data$NAME == "Advisory Program Schedule Generation", "AWA",
                        ifelse(Rejects_Data$NAME == "Advisory Target Allocation Process", "ATAP",
                        ifelse(Rejects_Data$NAME == "Advisory Workflow Application (AWA)", "AWA",
                        ifelse(Rejects_Data$NAME == "Annual Advisory Client Review (AACR)", "AACR",
                        ifelse(Rejects_Data$NAME == "AWA Subsession", "AWA",
                        ifelse(Rejects_Data$NAME == "Beneficiary Change Process", "Bene Change",
                        ifelse(Rejects_Data$NAME == "Blank Form Packet Creator", "AO",
                        ifelse(Rejects_Data$NAME == "Client Transfer Application", "CTA",
                        ifelse(Rejects_Data$NAME == "CostBasis Process", "Cost Basis",
                        ifelse(Rejects_Data$NAME == "Create Bulk PDF of CRT Letters", "CRT",
                        ifelse(Rejects_Data$NAME == "DRS_DRIP Process", "DRS_DRP",
                        ifelse(Rejects_Data$NAME == "DTC (Free Delivery) Request Process", "DTC",
                        ifelse(Rejects_Data$NAME == "DWAC Subprocess", "DWAC",
                        ifelse(Rejects_Data$NAME == "Fee Waiver Process", "Fee Waiver",
                        ifelse(Rejects_Data$NAME == "FI Allocation Account Opening", "Fixed Income",
                        ifelse(Rejects_Data$NAME == "FI Master Account Opening", "Fixed Income",
                        ifelse(Rejects_Data$NAME == "Find Existing Pre-sale Disclosure", "Retirement Plans",
                        ifelse(Rejects_Data$NAME == "Household Maintenance", "HM",
                        ifelse(Rejects_Data$NAME == "IER Account Opening Process", "IER",
                        ifelse(Rejects_Data$NAME == "Insurance Submission Process", "Ins Submission",
                        ifelse(Rejects_Data$NAME == "IRA Simplifiers Process", "AO",
                        ifelse(Rejects_Data$NAME == "LiquidOffice ACAT Process", "ACAT",
                        ifelse(Rejects_Data$NAME == "Margin Option Process", "M/O",
                        ifelse(Rejects_Data$NAME == "Mutual Fund Networking Request Process", "MFNR",
                        ifelse(Rejects_Data$NAME == "Open an account", "AO",
                        ifelse(Rejects_Data$NAME == "Periodic Investment Purchase and Sell (PIPS)", "PIPS",
                        ifelse(Rejects_Data$NAME == "PIM Composite Update Process", "PIM Comp Update",
                        ifelse(Rejects_Data$NAME == "Qualified Plan Account Opening", "Retirement Plans",
                        ifelse(Rejects_Data$NAME == "Qualified Plan Maintenance", "Retirement Plans",
                        ifelse(Rejects_Data$NAME == "Qualified Plan Pre-sale Disclosure Process", "Retirement Plans",
                        ifelse(Rejects_Data$NAME == "Review Previous Day Repurchase Trades", "Review Prev Day Trades",
                        ifelse(Rejects_Data$NAME == "Schedule C Update Subprocess", "Schedule C",
                        ifelse(Rejects_Data$NAME == "SSI Approval Subprocess", "SSI",
                        ifelse(Rejects_Data$NAME == "Statement Household Maintenance", "HM",
                        ifelse(Rejects_Data$NAME == "Supervision Group Acceptance Process", "AO",
                        ifelse(Rejects_Data$NAME == "Systematic and Standing Instructions Process", "SSI",
                        ifelse(Rejects_Data$NAME == "Tax Gain or Loss Sub Process", "Tax GL",
                        ifelse(Rejects_Data$NAME == "Trade Correction Process", "Trade Corr",
                        ifelse(Rejects_Data$NAME == "Transfer to Client Resource Team Process", "TCRT",
                        ifelse(Rejects_Data$NAME == "UTMA UGMA Exception State Registration Process", "UTMA Excp",
                        ifelse(Rejects_Data$NAME == "UTMA UGMA Freeze Subprocess", "UTMA Retitle",
                        ifelse(Rejects_Data$NAME == "Verbal Account Opening Approval Subprocess", "AO", "Pls Fix"))))))))))))))))))))))))))))))))))))))))))))))))

Rejects_Data$NAME <- NULL
Rejects_Data$Start_Day <- NULL
Rejects_Data$End_Day <- NULL
Rejects_Data$Start_Month <- NULL
Rejects_Data$End_Month <- NULL

#Rejects_Data$End_Year <- ifelse(is.na(Rejects_Data$End_Year), na.omit(Rejects_Data$End_Year), Rejects_Data$End_Year)

#Dummy Variables
#get predictors without response (and those predictors with only one value as well as date/timestamp values)
Rejects_Data_predictors <- select(Rejects_Data, -Reject)
#create dummy vars expect for the response
dummies_model <- dummyVars(~., data = Rejects_Data_predictors)

#provide only predictors that are now convert to dummy variables
Rejects_Data_predictors_dummy <- data.frame(predict(dummies_model, newdata = Rejects_Data))

#recombine predictors including dummy variables with response
Rejects_Data <- cbind(Reject = Rejects_Data$Reject, Rejects_Data_predictors_dummy)

#Drop NA's (1 for each bin)
Rejects_Data$DaysBin2.to.5.Days <- ifelse(is.na(Rejects_Data$DaysBin2.to.5.Days), na.omit(Rejects_Data$DaysBin2.to.5.Days), Rejects_Data$DaysBin2.to.5.Days)
Rejects_Data$DaysBinOver.1.Month <- ifelse(is.na(Rejects_Data$DaysBinOver.1.Month), na.omit(Rejects_Data$DaysBinOver.1.Month), Rejects_Data$DaysBinOver.1.Month)
Rejects_Data$DaysBinOver.1.Week <- ifelse(is.na(Rejects_Data$DaysBinOver.1.Week), na.omit(Rejects_Data$DaysBinOver.1.Week), Rejects_Data$DaysBinOver.1.Week)
Rejects_Data$DaysBinOver.2.Weeks <- ifelse(is.na(Rejects_Data$DaysBinOver.2.Weeks), na.omit(Rejects_Data$DaysBinOver.2.Weeks), Rejects_Data$DaysBinOver.2.Weeks)
Rejects_Data$DaysBinOver.3.Weeks <- ifelse(is.na(Rejects_Data$DaysBinOver.3.Weeks), na.omit(Rejects_Data$DaysBinOver.3.Weeks), Rejects_Data$DaysBinOver.3.Weeks)
Rejects_Data$DaysBinSame.Day <- ifelse(is.na(Rejects_Data$DaysBinSame.Day), na.omit(Rejects_Data$DaysBinSame.Day), Rejects_Data$DaysBinSame.Day)

#Convert FRAUD_NOFRAUD to a factor
Rejects_Data$Reject <- as.factor(Rejects_Data$Reject)
Rejects_Data$Reject <- fct_recode(Rejects_Data$Reject, NoReject = 'No_Reject', Reject = 'Reject')

#Logistic Classification
#set.seed()
#Split data for partitioning and indexing 
set.seed(99)
index <- createDataPartition(Rejects_Data$Reject,p = .8, list = FALSE)
Rejects_data_train <- Rejects_Data[index,]
Rejects_data_test <- Rejects_Data[-index,]

set.seed(99)



# Rejects_LOG_CLASS_model <- train(Reject~ ., 
#                                data = Rejects_data_train,
#                                method = 'glmnet',
#                                #method = 'cv' cv is cross validation
#                                trControl = trainControl(method = 'cv',
#                                                         #number lets the model know how many validation sets to create
#                                                         number = 5,
#                                                         classProbs = TRUE,
#                                                         summaryFunction = twoClassSummary),
#                                metric = 'ROC')
# Rejects_LOG_CLASS_model
# #Variable importance
# plot(varImp(Rejects_LOG_CLASS_model))
# varImp(Rejects_LOG_CLASS_model)
# #List coefficients selected
# coef(Rejects_LOG_CLASS_model$finalModel, Rejects_data_test$bestTune$lambda)
# #Get the predicted probabilities of the test data
# predprob_lasso_LOG <- predict(Rejects_LOG_CLASS_model, Rejects_data_test, type = 'prob')
# #Use cbind() to bring together probs and tran_id
# #Builds confusion matrix
# library(ROCR)
# pred_lasso_log <- prediction(predprob_lasso_LOG$Reject, Rejects_data_test$Reject, label.ordering = c("NoReject", "Reject"))
# 
# #tpr and fpr = True positive rate and false positive rate
# perf_lasso_log <- performance(pred_lasso_log, "tpr", "fpr")
# plot(perf_lasso_log, colorize = TRUE)
# 
# #Get the AUC
# auc_lasso_log <- unlist(slot(performance(pred_lasso_log, "auc"), "y.values"))
# auc_lasso_log

#Classification Decision Tree
#Split data at 8:2
#install.packages('rpart')
library(rpart)
set.seed(99)
Rejects_DTree_model <- train(Reject~.,
                           data = Rejects_data_train,
                           method = 'rpart',
                           trControl = trainControl(method = 'cv', number = 5,
                                                    #Estimate class probabilities
                                                    classProbs = TRUE,
                                                    #needed to get ROC
                                                    summaryFunction = twoClassSummary),
                           metric = 'ROC')
#Prunning to avoid overfitting

#Provides information of parameter tuning via cross validation
Rejects_DTree_model

plot(Rejects_DTree_model) #provides plot of paraneter tuning via cross validation

#plot variable importance
plot(varImp(Rejects_DTree_model))

#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(Rejects_DTree_model$finalModel, type = 5)

#First, get the prdicted probablities of the test data.
predprob_rejects_DTree <- predict(Rejects_DTree_model, Rejects_data_test, type = 'prob')

library(ROCR)
pred_lasso_DTree <- prediction(predprob_rejects_DTree[,2], Rejects_data_test$Reject, label.ordering = c('NoReject', 'Reject'))
perf_lasso_DTree <- performance(pred_lasso_DTree, 'tpr', 'fpr')
plot(perf_lasso_DTree, colorize = TRUE)

#Get AUC
auc_lasso_DTree <- unlist(slot(performance(pred_lasso_DTree, "auc"), "y.values"))

auc_lasso_DTree

#XGBoost
library(xgboost)
set.seed(99)
Rejects_xgb_model <- train(Reject~.,
                         data = Rejects_data_train,
                         method = 'xgbTree',
                         trControl = trainControl(method = 'cv', number = 5,
                                                  #Estimate class probabilities
                                                  classProbs = TRUE,
                                                  #needed to get ROC
                                                  summaryFunction = twoClassSummary),
                         metric = 'ROC')
#Prunning to avoid overfitting

Rejects_xgb_model
Rejects_xgb_model$bestTune

plot(Rejects_xgb_model) #provides plot of parameter tuning via cross validation

#plot variable importance
plot(varImp(Rejects_xgb_model))
varImp(Rejects_xgb_model)

#SHAP for interactions of variables. Wasn't a lot to see here. Earlier scatterplots were much more helpful
#install.packages('SHAPforxgboost')
library(SHAPforxgboost)
xdata <- as.matrix(select(Rejects_data_train, -Reject))
shap <- shap.prep(Rejects_xgb_model$finalModel, X_train = xdata)
shap.plot.summary(shap)
top4 <- shap.importance(shap, names_only = TRUE)[1:4]
for (x in top4) {
  p <- shap.plot.dependence(
    shap, 
    x = x, 
    color_feature = "auto", 
    smooth = FALSE, 
    jitter_width = 0.01, 
    alpha = 0.4
  ) +
    ggtitle(x)
  print(p)
}

#First, get the prdicted probablities of the test data.
predprob_rejects_xgb <- predict(Rejects_xgb_model, Rejects_data_test, type = 'prob')
#use cbind() to bring together predprob[,2] with tran_id

library(ROCR)
pred_lasso_xgb <- prediction(predprob_rejects_xgb[,2], Rejects_data_test$Reject, label.ordering = c('NoReject', 'Reject'))
perf_lasso_xgb <- performance(pred_lasso_xgb, 'tpr', 'fpr')
plot(perf_lasso_xgb, colorize = TRUE)

#Get AUC
auc_lasso_xgb <- unlist(slot(performance(pred_lasso_xgb, "auc"), "y.values"))

auc_lasso_xgb

#Random Forest
library(randomForest)
rf_grid <- expand.grid(mtry = c(1, 3, 7))
rf_grid

set.seed(99)
Rejects_rf_model <- train(Reject~.,
                        data = Rejects_data_train,
                        method = "rf",
                        trControl = trainControl(method = "cv", number = 5,
                                                 classProbs = TRUE,
                                                 summaryFunction = twoClassSummary),
                        metric = "ROC",
                        tuneGrid = rf_grid,
                        
                        verbose = FALSE)
Rejects_rf_model

plot(Rejects_rf_model)

plot(varImp(Rejects_rf_model))

predprob_rejects_rf <- predict(Rejects_rf_model, Rejects_data_test, type = 'prob')
#use cbind() to bring together predprob[,2] with tran_id

library(ROCR)
pred_lasso_rf <- prediction(predprob_rejects_rf[,2], Rejects_data_test$Reject, label.ordering = c('NoReject', 'Reject'))
perf_lasso_rf <- performance(pred_lasso_rf, 'tpr', 'fpr')
plot(perf_lasso_rf, colorize = TRUE)

auc_lasso_rf <- unlist(slot(performance(pred_lasso_rf, "auc"), "y.values"))

auc_lasso_rf
