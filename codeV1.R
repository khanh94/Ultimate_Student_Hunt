setwd("/Users/khanh94/Documents/Kaggle/Ultimate Hunt")

library(xgboost)
library(data.table)
library(lubridate)

train <- fread('train.csv')
test <- fread('test.csv')
submission <- fread('submission.csv')

ID <- test$ID

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}


train <- train[, ":="(ID = NULL,
                      Park_ID = NULL, 
                      #Var1 = NULL,
                      Date = as.Date(train$Date, format="%d-%m-%Y"), 
                      #Location_Type = as.numeric(as.factor(Location_Type))
                      Location_Type = NULL
                      #Max_Breeze_Speed = NULL
)]

test <- test[, ":="(ID = NULL,
                      Park_ID = NULL,
                      #Var1 = NULL,
                      Date = as.Date(test$Date, format="%d-%m-%Y"), 
                      #Location_Type = as.numeric(as.factor(Location_Type))
                      Location_Type = NULL
                      #Max_Breeze_Speed = NULL
)]

train$Season = getSeason(train$Date)
test$Season = getSeason(test$Date)

train$Season = as.numeric(factor(train$Season, levels=c("Winter", "Spring", "Fall", "Summer"), ordered=TRUE))
test$Season = as.numeric(factor(test$Season, levels=c("Winter", "Spring", "Fall", "Summer"), ordered=TRUE))

train$day <- as.factor(weekdays(train$Date))
test$day <- as.factor(weekdays(test$Date))

#train$weekend[(train$day == 'Saturday')|(train$day == "Sunday")] = 1
#test$weekend[(test$day == 'Saturday')|(test$day == "Sunday")] = 1

train$day = as.numeric(train$day)
test$day = as.numeric(test$day)

train$month <- month(train$Date)
test$month <- month(test$Date)

#train$Range_Atmospheric_Pressure = train$Max_Atmospheric_Pressure - train$Min_Atmospheric_Pressure
#test$Range_Atmospheric_Pressure = test$Max_Atmospheric_Pressure - test$Min_Atmospheric_Pressure

train$Wind_Inter = train$Direction_Of_Wind*train$Max_Breeze_Speed
test$Wind_Inter = test$Direction_Of_Wind*test$Max_Breeze_Speed


train$Date = NULL
test$Date = NULL


for(col in colnames(train))
{
  if(is.numeric(train[[col]]) == TRUE){
    train[[col]][is.na(train[[col]])] = median(train[[col]], na.rm=TRUE)
  }
}

for(col in colnames(test))
{
  if(is.numeric(test[[col]]) == TRUE){
    test[[col]][is.na(test[[col]])] = median(test[[col]], na.rm=TRUE)
  }
}

target = log10(train$Footfall)
train$Footfall = NULL

model_xgb_cv <- xgb.cv(data=as.matrix(train), 
                       label=as.matrix(target), 
                       nfold=5, 
                       objective="reg:linear", 
                       nrounds=1, 
                       eta=0.03, 
                       max_depth=3, 
                       subsample=0.75, 
                       colsample_bytree=0.8, 
                       min_child_weight=1, 
                       eval_metric="rmse")

preds = rep(0, nrow(test))

for (z in 1:1){
  set.seed(z + 80)
  model_xgb <- xgboost(data=as.matrix(train), 
                       label=as.matrix(target), 
                       objective="reg:linear", 
                       nrounds=10000, 
                       eta=0.03, 
                       max_depth=3, 
                       subsample=0.75, 
                       colsample_bytree=0.8, 
                       min_child_weight=1, 
                       booster='gbtree',
                       lambda=2,
                       alpha=1,
                       eval_metric="rmse")
  preds <- preds + predict(model_xgb, as.matrix(test))  
}

preds <- preds/1

final_sub = data.frame(ID = ID, Footfall = 10^preds)

write.csv(final_sub, file='final_sub.csv')

