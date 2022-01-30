library(tidyverse)
library(caret)
Fraud_Data <- read.csv(file = 'New_Alliance_data_.csv', header = T)

library(skimr)
summaryStats <- skim(Fraud_Data)
summaryStats


library(lubridate)
date_format <- as.Date(Fraud_Data$TRAN_TS)

#JLS
Fraud_Data$PWD_UPDT_TS <- ifelse(is.na(Fraud_Data$PWD_UPDT_TS), Fraud_Data$CUST_SINCE_DT, Fraud_Data$PWD_UPDT_TS)
Fraud_Data$PH_NUM_UPDT_TS <- ifelse(is.na(Fraud_Data$PH_NUM_UPDT_TS), Fraud_Data$CUST_SINCE_DT, Fraud_Data$PH_NUM_UPDT_TS)
Fraud_Data$CUST_SINCE_DT <- ifelse(is.na(Fraud_Data$CUST_SINCE_DT), na.omit(Fraud_Data$CUST_SINCE_DT), Fraud_Data$CUST_SINCE_DT)
Fraud_Data$TRAN_TS <- ifelse(is.na(Fraud_Data$TRAN_TS), na.omit(Fraud_Data$TRAN_TS), Fraud_Data$TRAN_TS)
Fraud_Data$TRAN_DT <- ifelse(is.na(Fraud_Data$TRAN_DT), na.omit(Fraud_Data$TRAN_DT), Fraud_Data$TRAN_DT)
Fraud_Data$ACTVY_DT <- ifelse(is.na(Fraud_Data$ACTVY_DT), na.omit(Fraud_Data$ACTVY_DT), Fraud_Data$ACTVY_DT)

#Create Month, Day, Year variables from transaction timestamp
#JLS & MC Code (TS_Month, TS_Day, TS_Year)
Fraud_Data <- mutate(Fraud_Data, TS_Month = month(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data, TS_Day = day(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data, TS_Year = year(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data, PWD_Month = month(as.Date(Fraud_Data$PWD_UPDT_TS,'%m/%d/%Y')),
                     Fraud_Data, PWD_Day = day(as.Date(Fraud_Data$PWD_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data, PWD_Year = year(as.Date(Fraud_Data$PWD_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data, PH_Month = month(as.Date(Fraud_Data$PH_NUM_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data, PH_Day = day(as.Date(Fraud_Data$PH_NUM_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data, PH_Year = year(as.Date(Fraud_Data$PH_NUM_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data, CUST_DT_Month = month(as.Date(Fraud_Data$CUST_SINCE_DT, '%m/%d/%Y')),
                     Fraud_Data, CUST_DT_Day = day(as.Date(Fraud_Data$CUST_SINCE_DT, '%m/%d/%Y')),
                     Fraud_Data, CUST_DT_Year = year(as.Date(Fraud_Data$CUST_SINCE_DT, '%m/%d/%Y')),
                     Fraud_Data, TR_DT_Month = month(as.Date(Fraud_Data$TRAN_DT, '%m/%d/%Y')),
                     Fraud_Data, TR_DT_Day = day(as.Date(Fraud_Data$TRAN_DT, '%m/%d/%Y')),
                     Fraud_Data, TR_DT_Year = year(as.Date(Fraud_Data$TRAN_DT, '%m/%d/%Y')),
                     Fraud_Data, ACTVY_Month = month(as.Date(Fraud_Data$ACTVY_DT, '%m/%d/%Y')),
                     Fraud_Data, ACTVY_Day = day(as.Date(Fraud_Data$ACTVY_DT, '%m/%d/%Y')),
                     Fraud_Data, ACTVY_Year = year(as.Date(Fraud_Data$ACTVY_DT, '%m/%d/%Y')))
#JLS
Fraud_Data$PWD_UPDT_TS <- NULL
Fraud_Data$PH_NUM_UPDT_TS <- NULL
Fraud_Data$CUST_SINCE_DT <- NULL
Fraud_Data$TRAN_TS <- NULL
Fraud_Data$TRAN_DT <- NULL
Fraud_Data$ACTVY_DT <- NULL

#Missing Value Handling
Fraud_Data$TRAN_ID <- ifelse(is.na(Fraud_Data$TRAN_ID), na.omit(Fraud_Data$TRAN_ID), Fraud_Data$TRAN_ID)
Fraud_Data$TRAN_AMT <- ifelse(is.na(Fraud_Data$TRAN_AMT), na.omit(Fraud_Data$TRAN_AMT), Fraud_Data$TRAN_AMT)
Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL <- ifelse(is.na(Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL), na.omit(Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL), Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL)
Fraud_Data$CUST_AGE <- ifelse(is.na(Fraud_Data$CUST_AGE), round(mean(Fraud_Data$CUST_AGE), 0), Fraud_Data$CUST_AGE)
Fraud_Data$OPEN_ACCT_CT <- ifelse(is.na(Fraud_Data$OPEN_ACCT_CT), na.omit(Fraud_Data$OPEN_ACCT_CT), Fraud_Data$OPEN_ACCT_CT)
Fraud_Data$WF_dvc_age <- ifelse(is.na(Fraud_Data$WF_dvc_age), round(mean(Fraud_Data$WF_dvc_age), 0), Fraud_Data$WF_dvc_age)
Fraud_Data$CARR_NAME <- ifelse(is.na(Fraud_Data$CARR_NAME), "other/unlisted", Fraud_Data$CARR_NAME)
Fraud_Data$RGN_NAME <- ifelse(is.na(Fraud_Data$RGN_NAME), "unknown", Fraud_Data$RGN_NAME)
Fraud_Data$STATE_PRVNC_TXT <- ifelse(is.na(Fraud_Data$STATE_PRVNC_TXT), "unknown", Fraud_Data$STATE_PRVNC_TXT)
Fraud_Data$ALERT_TRGR_CD <- ifelse(is.na(Fraud_Data$ALERT_TRGR_CD), "NO_DEVICE_ON_RECORD", Fraud_Data$ALERT_TRGR_CD)
Fraud_Data$DVC_TYPE_TXT <- ifelse(is.na(Fraud_Data$DVC_TYPE_TXT), "NO_DEVICE_ON_RECORD", Fraud_Data$DVC_TYPE_TXT)
Fraud_Data$AUTHC_PRIM_TYPE_CD <- ifelse(is.na(Fraud_Data$AUTHC_PRIM_TYPE_CD), "NO_AUTH_REQUIRED", Fraud_Data$AUTHC_PRIM_TYPE_CD)
Fraud_Data$AUTHC_SCNDRY_STAT_TXT <- ifelse(is.na(Fraud_Data$AUTHC_SCNDRY_STAT_TXT), "AUTH_BYPASSED", Fraud_Data$AUTHC_SCNDRY_STAT_TXT)
Fraud_Data$CUST_ZIP <- ifelse(is.na(Fraud_Data$CUST_ZIP), na.omit(Fraud_Data$CUST_ZIP), Fraud_Data$CUST_ZIP)
Fraud_Data$CUST_STATE <- ifelse(is.na(Fraud_Data$CUST_STATE), na.omit(Fraud_Data$CUST_STATE), Fraud_Data$CUST_STATE)
Fraud_Data$ACTN_CD <- ifelse(is.na(Fraud_Data$ACTN_CD), "SCHPMT", Fraud_Data$ACTN_CD)
Fraud_Data$ACTN_INTNL_TXT <- ifelse(is.na(Fraud_Data$ACTN_INTNL_TXT), "P2P_COMMIT", Fraud_Data$ACTN_INTNL_TXT)
Fraud_Data$TRAN_TYPE_CD <- ifelse(is.na(Fraud_Data$TRAN_TYPE_CD), "P2P", Fraud_Data$TRAN_TYPE_CD)
#MC Code
Fraud_Data$TS_Month <- ifelse(is.na(Fraud_Data$TS_Month), na.omit(Fraud_Data$TS_Month), Fraud_Data$TS_Month)
Fraud_Data$TS_Day <- ifelse(is.na(Fraud_Data$TS_Day), na.omit(Fraud_Data$TS_Day), Fraud_Data$TS_Day)
Fraud_Data$TS_Year <- ifelse(is.na(Fraud_Data$TS_Year), na.omit(Fraud_Data$TS_Year), Fraud_Data$TS_Year)
#JLS Code
Fraud_Data$PWD_Month <- ifelse(is.na(Fraud_Data$PWD_Month), na.omit(Fraud_Data$PWD_Month), Fraud_Data$PWD_Month)
Fraud_Data$PWD_Day <- ifelse(is.na(Fraud_Data$PWD_Day), na.omit(Fraud_Data$PWD_Day), Fraud_Data$PWD_Day)
Fraud_Data$PWD_Year <- ifelse(is.na(Fraud_Data$PWD_Year), na.omit(Fraud_Data$PWD_Year), Fraud_Data$PWD_Year)
Fraud_Data$PH_Month <- ifelse(is.na(Fraud_Data$PH_Month), na.omit(Fraud_Data$PH_Month), Fraud_Data$PH_Month)
Fraud_Data$PH_Day <- ifelse(is.na(Fraud_Data$PH_Day), na.omit(Fraud_Data$PH_Day), Fraud_Data$PH_Day)
Fraud_Data$PH_Year <- ifelse(is.na(Fraud_Data$PH_Year), na.omit(Fraud_Data$PH_Year), Fraud_Data$PH_Year)
Fraud_Data$CUST_DT_Month <- ifelse(is.na(Fraud_Data$CUST_DT_Month), na.omit(Fraud_Data$CUST_DT_Month), Fraud_Data$CUST_DT_Month)
Fraud_Data$CUST_DT_Day <- ifelse(is.na(Fraud_Data$CUST_DT_Day), na.omit(Fraud_Data$CUST_DT_Day), Fraud_Data$CUST_DT_Day)
Fraud_Data$CUST_DT_Year <- ifelse(is.na(Fraud_Data$CUST_DT_Year), na.omit(Fraud_Data$CUST_DT_Year), Fraud_Data$CUST_DT_Year)
Fraud_Data$CUST_DT_Month <- ifelse(is.na(Fraud_Data$CUST_DT_Month), na.omit(Fraud_Data$CUST_DT_Month), Fraud_Data$CUST_DT_Month)
Fraud_Data$CUST_DT_Day <- ifelse(is.na(Fraud_Data$CUST_DT_Day), na.omit(Fraud_Data$CUST_DT_Day), Fraud_Data$CUST_DT_Day)
Fraud_Data$CUST_DT_Year <- ifelse(is.na(Fraud_Data$CUST_DT_Year), na.omit(Fraud_Data$CUST_DT_Year), Fraud_Data$CUST_DT_Year)
Fraud_Data$TR_DT_Month <- ifelse(is.na(Fraud_Data$TR_DT_Month), na.omit(Fraud_Data$TR_DT_Month), Fraud_Data$TR_DT_Month)
Fraud_Data$TR_DT_Day <- ifelse(is.na(Fraud_Data$TR_DT_Day), na.omit(Fraud_Data$TR_DT_Day), Fraud_Data$TR_DT_Day)
Fraud_Data$TR_DT_Year <- ifelse(is.na(Fraud_Data$TR_DT_Year), na.omit(Fraud_Data$TR_DT_Year), Fraud_Data$TR_DT_Year)
Fraud_Data$ACTVY_Month <- ifelse(is.na(Fraud_Data$ACTVY_Month), na.omit(Fraud_Data$ACTVY_Month), Fraud_Data$ACTVY_Month)
Fraud_Data$ACTVY_Day <- ifelse(is.na(Fraud_Data$ACTVY_Day), na.omit(Fraud_Data$ACTVY_Day), Fraud_Data$ACTVY_Day)
Fraud_Data$ACTVY_Year <- ifelse(is.na(Fraud_Data$ACTVY_Year), na.omit(Fraud_Data$ACTVY_Year), Fraud_Data$ACTVY_Year)

#Consolidate Carriers to show as listed or unlisted.
Fraud_Data$CARR_NAME <- ifelse(Fraud_Data$CARR_NAME != 'other/unlisted', 'listed', 'other/unlisted')

#Clean and consolidate region names
Fraud_Data$RGN_NAME <- ifelse(Fraud_Data$RGN_NAME == "mid atlantic", "northeast", Fraud_Data$RGN_NAME)
Fraud_Data$RGN_NAME <- ifelse(Fraud_Data$RGN_NAME == "pacific northwest", "northwest", Fraud_Data$RGN_NAME)
Fraud_Data$RGN_NAME <- ifelse(Fraud_Data$RGN_NAME == "pacific", "west coast", Fraud_Data$RGN_NAME)
Fraud_Data$RGN_NAME <- ifelse(Fraud_Data$RGN_NAME == "south central", "south central", Fraud_Data$RGN_NAME)
Fraud_Data$RGN_NAME <- ifelse(Fraud_Data$RGN_NAME == "north west", "northwest", Fraud_Data$RGN_NAME)
Fraud_Data$RGN_NAME <- ifelse(Fraud_Data$RGN_NAME == "south east", "southeast", Fraud_Data$RGN_NAME)
Fraud_Data$RGN_NAME <- ifelse(Fraud_Data$RGN_NAME == "great lakes", "great lakes",
                       ifelse(Fraud_Data$RGN_NAME == "southwest", "southwest",
                       ifelse(Fraud_Data$RGN_NAME == "mountain", "mountain", 
                       ifelse(Fraud_Data$RGN_NAME == "midwest", "midwest",
                       ifelse(Fraud_Data$RGN_NAME == "unknown", "unknown", "international")))))



#Transaction amount greater than available cash? 
#VK Code
Fraud_Data$Can_Afford <- c()
Fraud_Data$Can_Afford <- ifelse(Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL >= Fraud_Data$TRAN_AMT,1,0)


#Group by Age
Fraud_Data$AgeBin <- c()
Fraud_Data$AgeBin[Fraud_Data$CUST_AGE <= 25] <- "25 and Younger"
Fraud_Data$AgeBin[Fraud_Data$CUST_AGE > 25 & Fraud_Data$CUST_AGE <= 49] <- "26 to 49"
Fraud_Data$AgeBin[Fraud_Data$CUST_AGE > 49 & Fraud_Data$CUST_AGE <= 70] <- "50 to 70"
Fraud_Data$AgeBin[Fraud_Data$CUST_AGE > 70] <- "Over 70"


#Does transaction take place for customer who owns no accounts? 
Fraud_Data$NonAccountTrans <- c()
Fraud_Data$NonAccountTrans <- ifelse(Fraud_Data$OPEN_ACCT_CT == 0, "Yes","No")



#Number of accounts in customer household. 
Fraud_Data$NumbAccountBin <- c()
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT == 0] <- "None"
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT > 0 & Fraud_Data$OPEN_ACCT_CT <= 3] <- "1 to 3"
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT > 3 & Fraud_Data$OPEN_ACCT_CT <= 7] <- "4 to 7"
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT > 7 & Fraud_Data$OPEN_ACCT_CT <= 10] <- "8 to 10"
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT > 10] <- "Over 10"

#Build vector of US states to determine, is transaction in US or not.
#JLS Code
Fraud_Data$Foreign <- ifelse(Fraud_Data$STATE_PRVNC_TXT %in% c("alabama","alaska", "arizon", "arkansas",
                                                               "california", "colorado", "connecticut",
                                                               "delaware", "florida", "georgia", "hawaii",
                                                               "idaho", "illinois", "indiana", "iowa", "kansas",
                                                               "kentucky", "louisiana", "maine", "maryland", "massachusetts",
                                                               "michigan", "minnesota", "mississippi", "missouri",
                                                               "montana", "nebraska", "nevada", "new hampshire", "new jersey",
                                                               "new mexico", "new york", "north carolina", "north dakota",
                                                               "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island",
                                                               "south carolina", "south dakota", "tennessee", "texas",
                                                               "utah", "vermont", "virginia", "washington", "west virginia",
                                                               "wisconsin", "wyoming"),0,1)

#Scatter plot for Transaction amt by month of the year, fraud or not
Fraud_Month <- ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = TS_Month, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Scatter plot for Transaction amt by customer age, fraud or not
Fraud_Age <- ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = CUST_AGE, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Transaction amount by age groupings, fraud or not
Fraud_Age_Groupings <- ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = AgeBin, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Scatter plot for Transaction amt by day of the month, fraud or not
Fraud_Day <- ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = TS_Day, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Scatter plot for Transaction amt by number of accts customer has at bank, fraud or not
Fraud_NumbAccts <- ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = NumbAccountBin, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Bar chart for transaction by location of transaction (US OR INTL), fraud or not
Fraud_Foreign <- ggplot(data = Fraud_Data) + 
  geom_bar(mapping = aes(x = Foreign, fill = FRAUD_NONFRAUD))

ggplot(data = Fraud_Data)+
  stat_count(mapping = aes(x = CARR_NAME, color = FRAUD_NONFRAUD))

Carrier_list_Unlist <- Fraud_Data %>% 
  group_by(CARR_NAME, FRAUD_NONFRAUD) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(FRAUD_NONFRAUD == "Fraud",
         count > 50)
RGN_Fraud <- Fraud_Data %>% 
  group_by(Fraud_Data$RGN_NAME, FRAUD_NONFRAUD) %>% 
  summarize(Fraud_count = n()) %>% 
  arrange(desc(Fraud_count)) %>% 
  filter(FRAUD_NONFRAUD == "Fraud")
  
RGN_NonFraud <- Fraud_Data %>% 
  group_by(Fraud_Data$RGN_NAME, FRAUD_NONFRAUD) %>% 
  summarize(NonFraud_count = n()) %>% 
  arrange(desc(NonFraud_count)) %>% 
  filter(FRAUD_NONFRAUD == "Non-Fraud")
  
cmb_RGN <- cbind(RGN_NAME = RGN_Fraud$`Fraud_Data$RGN_NAME`, Fraud_Prop = (RGN_Fraud$Fraud_count / RGN_NonFraud$NonFraud_count))
               

#Dummy Variables
#get predictors without response (and those predictors with only one value as well as date/timestamp values)
Fraud_Data_predictors <- select(Fraud_Data, -FRAUD_NONFRAUD, -ACTN_CD, -ACTN_INTNL_TXT, -TRAN_TYPE_CD)
#create dummy vars expect for the response
dummies_model <- dummyVars(~., data = Fraud_Data_predictors)

#provide only predictors that are now convert to dummy variables
Fraud_Data_predictors_dummy <- data.frame(predict(dummies_model, newdata = Fraud_Data))

#recombine predictors including dummy variables with response
Fraud_Data <- cbind(FRAUD_NONFRAUD = Fraud_Data$FRAUD_NONFRAUD, Fraud_Data_predictors_dummy)

#Convert FRAUD_NOFRAUD to a factor
Fraud_Data$FRAUD_NONFRAUD <- as.factor(Fraud_Data$FRAUD_NONFRAUD)
Fraud_Data$FRAUD_NONFRAUD <- fct_recode(Fraud_Data$FRAUD_NONFRAUD, NonFraud = 'Non-Fraud', Fraud = 'Fraud')

#Logistic Classification
#JLS & VK Code
#set.seed()
#Split data for partitioning and indexing 
set.seed(99)
index <- createDataPartition(Fraud_Data$FRAUD_NONFRAUD,p = .8, list = FALSE)
Fraud_data_train <- Fraud_Data[index,]
Fraud_data_test <- Fraud_Data[-index,]

set.seed(99)
Fraud_LOG_CLASS_model <- train(FRAUD_NONFRAUD~ ., 
                               data = Fraud_data_train,
                               method = 'glmnet',
                               #method = 'cv' cv is cross validation
                               trControl = trainControl(method = 'cv',
                                                        #number lets the model know how many validation sets to create
                                                        number = 5,
                                                        classProbs = TRUE,
                                                        summaryFunction = twoClassSummary),
                               metric = 'ROC')
Fraud_LOG_CLASS_model
#Variable importance
plot(varImp(Fraud_LOG_CLASS_model))
#List coefficients selected
coef(Fraud_LOG_CLASS_model$finalModel, Fraud_data_test$bestTune$lambda)
#Get the predicted probabilities of the test data
predprob_lasso_LOG <- predict(Fraud_LOG_CLASS_model, Fraud_data_test, type = 'prob')
#Builds confusion matrix
library(ROCR)
pred_lasso_log <- prediction(predprob_lasso_LOG$Fraud, Fraud_data_test$FRAUD_NONFRAUD, label.ordering = c("NonFraud", "Fraud"))

#tpr and fpr = True positive rate and false positive rate
perf_lasso_log <- performance(pred_lasso_log, "tpr", "fpr")
plot(perf_lasso_log, colorize = TRUE)

#Get the AUC
auc_lasso_log <- unlist(slot(performance(pred_lasso_log, "auc"), "y.values"))
auc_lasso_log

#Classification Decision Tree
#Split data at 8:2
#install.packages('rpart')
library(rpart)
set.seed(99)
Fraud_DTree_model <- train(FRAUD_NONFRAUD~.,
                           data = Fraud_data_train,
                           method = 'rpart',
                           trControl = trainControl(method = 'cv', number = 5,
                                                    #Estimate class probabilities
                                                    classProbs = TRUE,
                                                    #needed to get ROC
                                                    summaryFunction = twoClassSummary),
                           metric = 'ROC')
#Prunning to avoid overfitting

#Provides information of parameter tuning via cross validation
Fraud_DTree_model

plot(Fraud_DTree_model) #provides plot of paraneter tuning via cross validation

#plot variable importance
plot(varImp(Fraud_DTree_model))

#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(Fraud_DTree_model$finalModel, type = 5)

#First, get the prdicted probablities of the test data.
predprob_fraud_DTree <- predict(Fraud_DTree_model, Fraud_data_test, type = 'prob')

library(ROCR)
pred_lasso_DTree <- prediction(predprob_fraud_DTree[,2], Fraud_data_test$FRAUD_NONFRAUD, label.ordering = c('Fraud', 'NonFraud'))
perf_lasso_DTree <- performance(pred_lasso_DTree, 'tpr', 'fpr')
plot(perf_lasso_DTree, colorize = TRUE)

#Get AUC
auc_lasso_DTree <- unlist(slot(performance(pred_lasso_DTree, "auc"), "y.values"))

auc_lasso_DTree

#XGBoost
library(xgboost)
set.seed(99)
Fraud_xgb_model <- train(FRAUD_NONFRAUD~.,
                         data = Fraud_data_train,
                         method = 'xgbTree',
                         trControl = trainControl(method = 'cv', number = 5,
                                                  #Estimate class probabilities
                                                  classProbs = TRUE,
                                                  #needed to get ROC
                                                  summaryFunction = twoClassSummary),
                         metric = 'ROC')
#Prunning to avoid overfitting

Fraud_xgb_model

plot(Fraud_xgb_model) #provides plot of parameter tuning via cross validation

#plot variable importance
plot(varImp(Fraud_xgb_model))
varImp(Fraud_xgb_model)

#First, get the prdicted probablities of the test data.
predprob_fraud_xgb <- predict(Fraud_xgb_model, Fraud_data_test, type = 'prob')

library(ROCR)
pred_lasso_xgb <- prediction(predprob_fraud_xgb[,2], Fraud_data_test$FRAUD_NONFRAUD, label.ordering = c('Fraud', 'NonFraud'))
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
Fraud_rf_model <- train(FRAUD_NONFRAUD~.,
                        data = Fraud_data_train,
                        method = "rf",
                        trControl = trainControl(method = "cv", number = 5,
                                                 classProbs = TRUE,
                                                 summaryFunction = twoClassSummary),
                        metric = "ROC",
                        tuneGrid = rf_grid,
                        
                        verbose = FALSE)
Fraud_rf_model

plot(Fraud_rf_model)

plot(varImp(Fraud_rf_model))

predprob_fraud_rf <- predict(Fraud_rf_model, Fraud_data_test, type = 'prob')

library(ROCR)
pred_lasso_rf <- prediction(predprob_fraud_rf[,2], Fraud_data_test$FRAUD_NONFRAUD, label.ordering = c('Fraud', 'NonFraud'))
perf_lasso_rf <- performance(pred_lasso_rf, 'tpr', 'fpr')
plot(perf_lasso_rf, colorize = TRUE)

auc_lasso_rf <- unlist(slot(performance(pred_lasso_rf, "auc"), "y.values"))

auc_lasso_rf
