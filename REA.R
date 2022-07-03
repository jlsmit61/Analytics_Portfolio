library(tidyverse)
library(skimr)
library(lubridate)
library(caret)
library(vcd)
library(multcompView)

messages <- read_csv("Network_Messages_Capstone.csv")
#skim(messages)

#Convert relevant timestamps to datetime
messages$delivered_at <- ymd_hms(messages$delivered_at)
messages$notif_read_at <- ymd_hms(messages$notif_read_at)
messages$notif_read <- ifelse(messages$notif_read == "TRUE", "Read", "Not_Read")

#Create new features for day of the week, time of day
messages$Day_Of_Week <- wday(messages$delivered_at, label = TRUE)
messages <- messages[!(messages$mesg_status == "failed"),] 
messages$Hour_of_Day <- hour(messages$delivered_at)
messages$Hour_of_Day <- ifelse(is.na(messages$Hour_of_Day), na.omit(messages$Hour_of_Day), messages$Hour_of_Day)
messages$Time_of_Day <- ifelse(messages$Hour_of_Day < 12, "Morning",
                           ifelse(messages$Hour_of_Day >= 12 & messages$Hour_of_Day <17, "Afternoon", "Evening"))
messages$Weekend <- ifelse(messages$Day_Of_Week == "Sat" | 
                             messages$Day_Of_Week == "Sun", 1, 0)

#NEW FEATURES. HAVEN'T RUN MODEL WITH THESE YET
messages$Friday_Afternoon <- ifelse(messages$Day_Of_Week == "Fri" & 
                                      messages$Time_of_Day == "Afternoon", 1, 0)
messages$Monday_Morning <- ifelse(messages$Day_Of_Week == "Mon" &
                                    messages$Time_of_Day == "Morning", 1, 0)

#impute standard date in future for missing values of messages not read
messages$notif_read_at_na <- ifelse(is.na(messages$notif_read_at), as.POSIXct("2022-12-01"), messages$notif_read_at) 
messages$notif_delivered_at_na <- ifelse(is.na(messages$delivered_at), as.POSIXct("2022-12-01"), messages$delivered_at)   

#Create variable RRTM for timliness of a message being read
messages$RRTM <- format((messages$notif_read_at_na - messages$notif_delivered_at_na) / 60, scientific = FALSE)
messages$RRTM <- as.numeric(messages$RRTM)
messages$RRTM <- ifelse(messages$RRTM < 0, 0, messages$RRTM)


#Variables needed for feature engineering
avg_char_count <- mean(messages$character_length)
avg_subject_length <- mean(messages$subject_length)
avg_RRTM <- mean(messages$RRTM)

#Date (Day and Month and Year)
messages$read_day <- day(messages$notif_read_at)
messages$read_day <- ifelse(is.na(messages$read_day), 1, messages$read_day)
messages$read_month <- month(messages$notif_read_at)
messages$read_month <- ifelse(is.na(messages$read_month), 12, messages$read_month)
messages$read_year <- year(messages$notif_read_at)
messages$read_year <- ifelse(is.na(messages$read_year), 2022, messages$read_year)
messages$delivered_day <- day(messages$delivered_at)
messages$delivered_day <- ifelse(is.na(messages$delivered_day), na.omit(messages$delivered_day), messages$delivered_day)
messages$delivered_month <- month(messages$delivered_at)
messages$delivered_month <- ifelse(is.na(messages$delivered_month), na.omit(messages$delivered_month), messages$delivered_month)
messages$delivered_year <- year(messages$delivered_at)
messages$delivered_year <- ifelse(is.na(messages$delivered_year), na.omit(messages$delivered_year), messages$delivered_year)


#Bins for length of time
messages$TimeBin <- c()
messages$TimeBin[messages$RRTM <= 5] <- "5 minutes or less"
messages$TimeBin[messages$RRTM >5 & messages$RRTM <=60] <- "5m to 1hr"
messages$TimeBin[messages$RRTM >60 & messages$RRTM <=480] <- "1 to 8hrs"
messages$TimeBin[messages$RRTM >480 & messages$RRTM <=1440] <- "8hrs to 1 day"
messages$TimeBin[messages$RRTM >1440 & messages$RRTM <=10080] <- "1 to 7 days"
messages$TimeBin[messages$RRTM >10080 & messages$RRTM <=43200] <- "Over 1 Week"
messages$TimeBin[messages$RRTM >43200 & messages$RRTM <=345600] <- "Over 4 Months"
messages$TimeBin[messages$RRTM >345600 & messages$RRTM <=500000] <- "Over 8 Months"
messages$TimeBin[messages$RRTM >500000] <- "Unread Msg"

#Remove Features no longer needed
messages$notif_read_at <- NULL
messages$notif_details_read_at <- NULL
messages$notif_read_at_na <- NULL
messages$notif_delivered_at_na <- NULL
messages$delivered_at <- NULL
messages$created_at <- NULL
messages$updated_at <- NULL
messages$notif_sent <- NULL
messages$conversation_id <- NULL
messages$message_id...8 <- NULL
messages$message_id...11 <- NULL
messages$mesg_created_at <- NULL
messages$mesg_updated_at <- NULL
messages$mesg_sender_id <- NULL
messages$content_type <- NULL
messages$use_case <- NULL
messages$notif_created_at <- NULL
messages$notif_updated_at <- NULL
messages$mesg_date <- NULL
messages$notification_id <- NULL
messages$sender_type <- NULL
messages$read_year <- NULL
messages$delivered_year <- NULL

#Time to read message greater than average
messages$Long_Read_Time <- ifelse(messages$RRTM > avg_RRTM, 1, 0)

#Long Read Time and attachment included
messages$Attachment_LRT <- ifelse((messages$RRTM > avg_RRTM) & messages$attachments == "TRUE", 1, 0)
messages$EmbAttachment_LRT <- ifelse((messages$RRTM > avg_RRTM) & messages$embed_attachment == "TRUE", 1, 0)
messages$Long_Subject_RT <- ifelse((messages$subject_length > avg_subject_length) & messages$RRTM > avg_RRTM, 1, 0)
messages$Large_Char_RT <- ifelse((messages$character_length > avg_char_count) & messages$RRTM > avg_RRTM, 1,0)
messages$Subject_Large <- ifelse((messages$subject_length > avg_subject_length), 1, 0)
messages$Char_Large <- ifelse((messages$character_length > avg_char_count), 1, 0)

#Quick Read and any attachments
messages$PromptRead_AnyAttach <- ifelse((messages$TimeBin == "5 minutes or less") &
                                          messages$attachments == "TRUE" &
                                          messages$embed_attachment == "TRUE", 1,0)
#Quick read and long subject plus embedded attachment
messages$PromptRead_LS_EmbAttach <- ifelse((messages$TimeBin == "5 minutes or less") &
                                            messages$subject_length > avg_subject_length &
                                            messages$embed_attachment == "TRUE", 1,0)

#Quick read and long character count plus embedded attachment
messages$PromptRead_LC_EmbAttach <- ifelse((messages$TimeBin == "5 minutes or less") &
                                         messages$character_length > avg_char_count &
                                         messages$embed_attachment == "TRUE", 1, 0)
messages$Thu_EmbAttach <- ifelse((messages$Day_Of_Week == "Thu") &
                                       (messages$embed_attachment == "TRUE"), 1,0)
messages$Wed_EmbAttach <- ifelse((messages$Day_Of_Week == "Wed") &
                                   (messages$embed_attachment == "TRUE"), 1,0)
messages$Weekend_EmbAttach <- ifelse((messages$Weekend == 1) & 
                                       (messages$embed_attachment == "TRUE"), 1,0)
messages$Morning_EmbAttach <- ifelse((messages$Time_of_Day == "Morning") &
                                       messages$embed_attachment == "TRUE", 1, 0)
messages$Afternoon_EmbAttach <- ifelse((messages$Time_of_Day == "Afternoon") &
                                       messages$embed_attachment == "TRUE", 1, 0)
messages$Evening_EmbAttach <- ifelse((messages$Time_of_Day == "Evening") &
                                       messages$embed_attachment == "TRUE", 1, 0)
messages$Month_Day_BINs_embAtt <- ifelse(messages$delivered_day <11 & messages$embed_attachment == "TRUE", "First10Days",
                                         ifelse((messages$delivered_day >=11 & messages$delivered_day <21) & 
                                                  messages$embed_attachment == "TRUE", "Middle10Days", 
                                                ifelse(messages$delivered_day>=21 & messages$embed_attachment == "TRUE","Last10Days", "NoEmbAttach")))

#skim(messages)
#write.csv(messages, "messages_250K_with_DVs.csv")

ggplot(data = messages) + 
  geom_bar(mapping = aes(x = embed_attachment, fill = notif_read))

ggplot(data = messages) + 
  geom_bar(mapping = aes(x = messages$notif_read))

ggplot(data = messages) + 
  geom_bar(mapping = aes(x = attachments, fill = notif_read))

ggplot(data = messages) + 
  geom_bar(mapping = aes(x = PromptRead_LC_EmbAttach, fill = notif_read))

ggplot(data = messages) + 
  geom_bar(mapping = aes(x = TimeBin)) +
  coord_flip()


messages_read <- select(messages, -RRTM, -read_day, -read_month, -delivered_day, -delivered_month,
                        -notif_details_read, -Long_Read_Time, -Attachment_LRT, -EmbAttachment_LRT,
                        -Long_Subject_RT, -Large_Char_RT, -TimeBin)
#write_csv(messages_read, "messages_read.csv")

#######################################################################################################
#Drivers

examine_driver <- function(driver,sort=TRUE) {
  mosaic( formula(paste("notif_read ~",driver)), data=messages_read, inside=TRUE, equal=TRUE )
  SUMMARY <- aggregate(formula(paste('notif_read=="Read"~', driver)),data=messages_read,FUN=mean)
  AOV <- aov(formula(paste('notif_read=="Read"~', driver)),data=messages_read)
  TUKEY <- TukeyHSD(AOV)
  LETTERS <- multcompLetters4(AOV,TUKEY)
  SUMMARY$letters <- LETTERS[[1]][1]$Letters[match(SUMMARY[,1],names( LETTERS[[1]][1]$Letters ) )]
  SUMMARY$n <- as.numeric(table(messages_read[,driver]))
  names(SUMMARY)[2] <- "ProbRead"
  if(sort==TRUE) {
    SUMMARY <- SUMMARY[order(SUMMARY$ProbRead,decreasing=TRUE),] }
  rownames(SUMMARY) <- NULL
  SUMMARY
}




messages_read$Day_Of_Week_fct <- as.factor(messages$Day_Of_Week)
examine_driver("Day_Of_Week_fct")

#Remember to remove fct from model after examining driver
messages_read <- select(messages_read, -Day_Of_Week_fct)

messages_read$Time_Of_Day_fct <- as.factor(messages$Time_of_Day)
examine_driver("Time_Of_Day_fct")

#Remember to remove fct from model after examining driver

messages_read <- select(messages_read, -Time_Of_Day_fct)

#Remember to remove fct from model after examining driver
messages_read <- select(messages_read, -Day_Of_Week_fct)

messages_read$Thu_EmbAttach__fct <- as.factor(messages$Thu_EmbAttach)
examine_driver("Thu_EmbAttach__fct")

#Remember to remove fct from model after examining driver
messages_read <- select(messages_read, -Thu_EmbAttach__fct)

messages_read$Wed_EmbAttach__fct <- as.factor(messages$Wed_EmbAttach)
examine_driver("Wed_EmbAttach__fct")

#Remember to remove fct from model after examining driver
messages_read <- select(messages_read, -Wed_EmbAttach__fct)

messages_read$Weekend_EmbAttach__fct <- as.factor(messages$Weekend_EmbAttach)
examine_driver("Weekend_EmbAttach__fct")

#Remember to remove fct from model after examining driver
messages_read <- select(messages_read, -Weekend_EmbAttach__fct)

messages_read$Morning_EmbAttach__fct <- as.factor(messages$Morning_EmbAttach)
examine_driver("Morning_EmbAttach__fct")

#Remember to remove fct from model after examining driver
messages_read <- select(messages_read, -Morning_EmbAttach__fct)

messages_read$Afternoon_EmbAttach__fct <- as.factor(messages$Afternoon_EmbAttach)
examine_driver("Afternoon_EmbAttach__fct")

#Remember to remove fct from model after examining driver
messages_read <- select(messages_read, -Afternoon_EmbAttach__fct)


messages_read$Evening_EmbAttach__fct <- as.factor(messages$Evening_EmbAttach)
examine_driver("Evening_EmbAttach__fct")

#Remember to remove fct from model after examining driver
messages_read <- select(messages_read, -Evening_EmbAttach__fct)

#examine drivers on important variables for Classification Model




#examine drivers on important variables for Regression Model
messages_read$delivered_month_fct <- as.factor(messages$delivered_month)
examine_driver("delivered_month_fct")

#Remember to remove fct from model after examining driver
messages_read <- select(messages_read, -delivered_month_fct)


messages_read$Long_Read_Time_fct <- as.factor(messages$Long_Read_Time)
examine_driver("Long_Read_Time_fct")

#Remember to remove fct from model after examining driver
messages_read <- select(messages_read, -Long_Read_Time_fct)

messages_read$Attachment_LRT_fct <- as.factor(messages$Attachment_LRT)
examine_driver("Attachment_LRT_fct")

#Remember to remove fct from model after examining driver
messages_read <- select(messages_read, -Attachment_LRT_fct)

messages_read$Month_Days_BINs_embAttach_fct <- as.factor(messages$Month_Day_BINs_embAtt)
examine_driver("Month_Days_BINs_embAttach_fct")

#Remember to remove fct from model after examining driver
messages_read <- select(messages_read, -Month_Days_BINs_embAttach_fct)




###############################################################################################################

#get predictors
messages_read_predictors <- select(messages_read, -notif_read)

#create dummy vars
dummies_model <- dummyVars(~.,data = messages_read_predictors)

#provide only predictors that are now convert to dummy variables
messages_predictors_dummy <- data.frame(predict(dummies_model, newdata = messages_read))

#recombine predictors including dummy variables with response
messages_read <- cbind(notif_read = messages_read$notif_read, messages_predictors_dummy)

#Convert notif_read to a factor
messages_read$notif_read <- as.factor(messages_read$notif_read)
messages_read$notif_read <- fct_recode(messages_read$notif_read, Read = "Read", Not_Read = "Not_Read")

#Split data for partitioning and indexing
set.seed(99)
index <- createDataPartition(messages_read$notif_read, p=.75, list = FALSE)


messages_data_train <- messages_read[index,]
messages_data_test <- messages_read[-index,]



#Read or Not_Read Boosted Model

library(xgboost)
set.seed(99)
REA_xgb_model <- train(notif_read~.,
                         data = messages_data_train,
                         method = 'xgbTree',
                         trControl = trainControl(method = 'cv', number = 5,
                                                  #Estimate class probabilities
                                                  classProbs = TRUE,
                                                  #needed to get ROC
                                                  summaryFunction = twoClassSummary),
                         metric = 'ROC')
#Prunning to avoid overfitting

REA_xgb_model
REA_xgb_model$bestTune

plot(REA_xgb_model) #provides plot of parameter tuning via cross validation

#plot variable importance
plot(varImp(REA_xgb_model))
varImp(REA_xgb_model)

#First, get the prdicted probablities of the test data.
predprob_messages_xgb <- predict(REA_xgb_model, messages_data_test, type = 'prob')
#use cbind() to bring together predprob[,2] with tran_id

library(ROCR)
pred_lasso_xgb <- prediction(predprob_messages_xgb[,2], messages_data_test$notif_read, label.ordering = c('Not_Read', 'Read'))
perf_lasso_xgb <- performance(pred_lasso_xgb, 'tpr', 'fpr')
plot(perf_lasso_xgb, colorize = TRUE)

#Get AUC
auc_lasso_xgb <- unlist(slot(performance(pred_lasso_xgb, "auc"), "y.values"))

auc_lasso_xgb
#AUC of .72 for real world is probably solid. Gives credence to important variables for rec's


###############################################################################################################################


#Timeliness of Message Being Read Boosted Regression Model

messages <- select(messages, -notif_read, -notif_details_read)
#write_csv(messages, "messages_RRTM.csv")
messages_RRTM_predictors <- select(messages, -RRTM)
#create dummy vars expect for the response
dummies_model <- dummyVars(~., data = messages_RRTM_predictors)

#provide only predictors that are now convert to dummy variables
messages_RRTM_data_predictors_dummy <- data.frame(predict(dummies_model, newdata = messages))

#recombine predictors including dummy variables with response
messages <- cbind(RRTM = messages$RRTM , messages_RRTM_data_predictors_dummy)

set.seed(99)
index <- createDataPartition(messages$RRTM, p = .75, list = FALSE)
messages_RRTM_data_train <- messages[index,]
messages_RRTM_data_test <- messages[-index,]

library(xgboost)

set.seed(99)

xgb_grid <- expand.grid(
  nrounds = c(50,200),
  eta = c(.025, .05, .1),
  max_depth = c(2,3,4,5),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1)

REA_RRTM_gbm <- train(RRTM~.,
                   data = messages_RRTM_data_train,
                   method = "xgbTree",
                   trControl = trainControl(method = "cv", number = 5),
                   
                   #tuneGrid = xgb_grid,
                   verbose = FALSE)

plot(REA_RRTM_gbm)

plot(varImp(REA_RRTM_gbm))

RRTM_probs <- predict(REA_RRTM_gbm, messages_RRTM_data_test)

ASE.tree <- RMSE(RRTM_probs, messages_RRTM_data_test$RRTM)^2
ASE.tree
#ASE is weirdly high number that doesn't mean much

RMSE_RRTM <- RMSE(RRTM_probs, messages_RRTM_data_test$RRTM)
Max_RRTM <- max(messages$RRTM)
Min_RRTM <- min(messages$RRTM)

Normalized_RMSE <- RMSE_RRTM / (Max_RRTM - Min_RRTM)
Normalized_RMSE * 100
#Normalized_RMSE says Average error is .41% of the range of values in our data.
