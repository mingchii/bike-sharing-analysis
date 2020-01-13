
library(dplyr)
library(caTools)
library(rpart)
library(caret)
library(ggplot2)
library(lubridate)
library(car)
library(rpart.plot)
library(tidyr)
library(boot)
library(keras)


require(devtools)
install_github("rstudio/reticulate")
install_github("rstudio/tensorflow")
install_github("rstudio/keras")

library(keras)
install_keras()

rm(list = ls())

OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}


fulldata <- read.csv("full data.csv", stringsAsFactors = TRUE)

## whether the date is weekend or weekday
#fulldata$Date <- ymd(fulldata$Date)
#fulldata <- mutate(fulldata, DayofWeek = factor(wday(Date,label = TRUE),ordered = FALSE))
#c <- data.frame(Sat = ifelse(fulldata$DayofWeek == "?g??",1,0), Sun = ifelse(fulldata$DayofWeek == "?g??",1,0))
#fulldata <- mutate(fulldata, WeekendOrNot = ifelse(c$Sat+c$Sun == 1, TRUE, FALSE))
#rm(c)
##


fulldata$Date <- ymd(fulldata$Date) # this function should be in accordance with the type of time presenting in computer
fulldata[,c(1, 2, 15, 16, 18)] <- NULL
fulldata$nTripYesterday <- NA


for (i in fulldata$station.name) {
  a <- filter(fulldata, fulldata$station.name == i)
  a$nTripYesterday <- c(NA, head(a$nTrip,-1))
  fulldata[fulldata$station.name == i,"nTripYesterday"] <- a$nTripYesterday
}


fulldata <- drop_na(fulldata)


fulldata$day <- as.factor(fulldata$day)
fulldata$month <- as.factor(fulldata$month)
fulldata$Median.Household.Income<-as.numeric(fulldata$Median.Household.Income)
fulldata$Median.home.value<-as.numeric(fulldata$Median.home.value)
fulldata$Population.Density<-as.numeric(fulldata$Population.Density)
fulldata$Population<-as.numeric(fulldata$Population)
fulldata$Number.of.people.using.bicycle<-as.numeric(fulldata$Number.of.people.using.bicycle)


train <- filter(fulldata, year < 2015)
test <- filter(fulldata, year >= 2015)
test.temp <- test
test$Date <- NULL
train$Date <- NULL


# what is the porportion that is used to training models
nrow(train)/(nrow(train)+nrow(test))


### linear regression 
model.lm <- lm(nTrip ~ .-day-year-station.name-Temperature.Average, data = train)
summary(model.lm)
vif(model.lm)


#Based on VIF, we remove those variables which have high VIF
lm.updated <- lm(nTrip ~ .-day-year-station.name-Temperature.Maximum-Temperature.Minimum-HDD-Business_Administrative_Support_Services-Business_Certain_Services-Business_Financial_Services-Business_Information-Business_Insurance-Business_Professional_Scientific_Technical_Services-Business_Real_Estate_Leasing    , data = train)
summary(lm.updated)

vif(lm.updated)

pred.lm <- predict(lm.updated, newdata = test)
lm.osr2 <- OSR2(pred.lm, train$nTrip, test$nTrip)
lm.osr2


## Bootstrap for R-Squared
########################

# function to obtain R-Squared from the data 
rsq <- function(formula, data, index) {
  d <- data[index,]
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 

Rsquare <- boot(data = train, statistic = rsq, R = 1000, 
                formula = nTrip ~ .-day-year-station.name-Temperature.Maximum-Temperature.Minimum-HDD-Business_Administrative_Support_Services-Business_Certain_Services-Business_Financial_Services-Business_Information-Business_Insurance-Business_Professional_Scientific_Technical_Services-Business_Real_Estate_Leasing )

Rsquare
boot.ci(Rsquare, type = "basic")




### Cart model

cpvalue <- data.frame(cp = seq(0.002,0.1,by = 0.002))
set.seed(123)
model.cart <- train(nTrip ~ .-day-year-station.name-Temperature.Maximum-Temperature.Minimum-HDD-Business_Administrative_Support_Services-Business_Certain_Services-Business_Financial_Services-Business_Information-Business_Insurance-Business_Professional_Scientific_Technical_Services-Business_Real_Estate_Leasing  , 
                    data = train,
                    method = "rpart", 
                    tuneGrid = cpvalue, 
                    trControl = trainControl(method = "cv", number = 5),
                    metric = "RMSE",na.action=na.exclude)

model.cart
model.bestcart <- model.cart$finalModel
ggplot(model.cart$results, aes(x=cp, y=RMSE)) + geom_point(size=1) +
  xlab("Complexity Parameter (cp)") + geom_line()


test.mm <- as.data.frame(model.matrix(nTrip~.+0, data = test))
pred.cart <- predict(model.bestcart, newdata = test.mm)
cart.osr2 <- OSR2(pred.cart, train$nTrip, test$nTrip)
cart.osr2

prp(model.bestcart)


### Random Forest

set.seed(123)
model.rf <- train(nTrip ~ .-day-year-station.name-Temperature.Maximum-Temperature.Minimum
                  -HDD-Business_Administrative_Support_Services-Business_Certain_Services
                  -Business_Financial_Services-Business_Information-Business_Insurance
                  -Business_Professional_Scientific_Technical_Services-Business_Real_Estate_Leasing  ,
                  data = train,
                  method = "rf",
                  tuneGrid = data.frame(mtry = 20), # best is 20
                  trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                  metric = "RMSE",na.action=na.exclude)

#model.rf$results
model.bestRF <- model.rf$finalModel

ggplot(model.rf$results, aes(x = mtry, y = Rsquared)) + geom_point(size = 3) + geom_line() + 
  ylab("CV Rsquared") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

pred.rf <- predict(model.bestRF, newdata = test.mm) 
RF.osr2 <- OSR2(pred.rf, train$nTrip, test$nTrip)
RF.osr2



c(lm.osr2, cart.osr2, RF.osr2)




### drawing plot 
#####################

library(ggplot2)
library(hrbrthemes)


# A basic scatterplot with color depending on WeekendOrNot
# depend on which station it is where n can control which station it is range from 1 to 35
n = 1
station.list <- unique(fulldata$station.name)
which.station <- fulldata[fulldata$station.name == station.list[n],]

ggplot(which.station, aes(x=Date, y=nTrip, color=WeekendOrNot)) + 
  geom_point(size=1) + ylab("Number of trips per day") + xlab("Date") +
  ggtitle(station.list[n])



# Bar chart of Average Bike Usage by Month
# Monthly nTrip
monthly.use <- data.frame(Month = as.factor(fulldata$month), nTrip = fulldata$nTrip, Year = year(fulldata$Date))
monthly.bar.temp <- group_by(monthly.use, Year, Month) %>% summarise( nTrip = sum(nTrip))
monthly.bar <- group_by(monthly.bar.temp, Month) %>% summarise( nTrip = sum(nTrip) / 2)


ggplot(monthly.bar, aes( y=nTrip, x=Month)) + 
  geom_bar(stat="identity") +
  ggtitle("Average Bike Usage by Month")



# How the trips are varying according to the weather?

##Plot the number of trips Vs. temperature
options(repr.plot.width=10, repr.plot.height=4)
ggplot(fulldata, aes(Temperature.Average)) + 
  geom_bar(stat = "count", aes(fill = WeekendOrNot), position = "dodge")  +
  ylab("Total number of bicycle trips") +
  xlab("Mean temperature for the day") + 
  ggtitle("              Temperature vs. Number of Bicycle Trips") +
  theme_grey()

# Test set prediction plots
ggplot(tail(test.temp,50), aes(x = Date, y = nTrip, col = "")) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=tail(pred.lm, 50), col = "red")) +
  geom_line(aes(y=tail(pred.rf, 50)), col="green") +
  scale_color_discrete(name = "types", labels = c("Actual value", "Linear regression", "Random Forest")) +
  theme(legend.position="top") +
  ggtitle("Predicted Values vs Actual Values")


##LSTM
test %>% head()
summary(train,test)

#Making Model:

lag_setting  <- 7
batch_size   <- 3
tsteps       <- 1
epochs       <-5
model <- keras_model_sequential()

model %>%
  layer_lstm(units            = 100, 
             input_shape      = c(tsteps, 1), 
             batch_size       = batch_size,
             return_sequences = TRUE,
             stateful         = TRUE) %>% 
  layer_lstm(units            = 100, 
             return_sequences = TRUE, 
             stateful         = TRUE) %>%
  layer_lstm(units            = 50, 
             return_sequences = FALSE, 
             stateful         = TRUE) %>% 
  layer_dense(units = 1)

model %>% 
  compile(loss = 'mae', optimizer = 'adam')

model

#Implementing lSTM 


if (2>1) {
  #train and test split
  df_trn <- data.frame(train$nTrip) #t_temp[1:(nrow(t_temp)-100),] # select n - 100 rows as training set
  df_tst <- data.frame(test$nTrip) #t_temp[(nrow(t_temp)-99):nrow(t_temp),] # select remaining rows as test set
  colnames(df_trn)=c("trips")
  colnames(df_tst)=c("trips")
  
  #Combine data into one data frame with key as training and testing data
  df <- rbind(
    cbind(df_trn,key = "training"),
    cbind(df_tst,key = "testing") )
    
    
  # find values for normalizing recipe and bake
  #rec_obj <- recipe(trips ~ ., df) %>%
   # step_sqrt(trips) %>%
   # step_center(trips) %>%
   # step_scale(trips) %>%
   # prep()
  #normalize(x, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
  
  #center_history <- rec_obj$steps[[2]]$means["trips"]
  #scale_history  <- rec_obj$steps[[3]]$sds["trips"]
  #c("center" = center_history, "scale" = scale_history)
  
  # Training Set
  value_lag = lag(df$trips, n = lag_setting)
  df=mutate(df,value_lag)
  df_processed_tbl <-df #bake(rec_obj, df)
  
  train_length <- round((floor((nrow(df_trn) - lag_setting)/batch_size))*batch_size) # Lag is introduced to make a Y vector for comparing. After introducing lag, the remaining number of rows should be divisible by the batch size.
  #select lag table 
 
  #filter(!is.na(value_lag)) 
  lag_train_tbl <- filter(df_processed_tbl,key == "training") 

  #filter(key == "training") 
  tail(train_length)
  
  x_train_vec <- lag_train_tbl$value_lag # select only the values from table as vector
  x_train_arr <- array_reshape(x_train_vec, c( 16515,1, 1)) #create input dimension with (nrows, batchsize, ndimensions)
  y_train_vec <- lag_train_tbl$trips 
  y_train_arr <- array_reshape(y_train_vec, c(16515,1,1))
  # Testing Set
  lag_test_tbl <- filter(df_processed_tbl,key=="testing") 
  #mutate(
      #value_lag = lag(df$trips, n = lag_setting)
    #) 
    #filter(!is.na(value_lag)) 

  x_test_vec <- lag_test_tbl$value_lag
  x_test_arr <- array_reshape(x_test_vec, c( 8394, 1, 1)) # same as that of the train set
  y_test_vec <- lag_test_tbl$trips
  y_test_arr <- array( y_test_vec, c(8394,1, 1))
  #fit model for the number of epochs
  for (i in 1:epochs) {
    
    model %>% fit(x          = x_train_arr,
                  y          = y_train_arr, 
                  batch_size = batch_size,
                  epochs     = 1,
                  verbose    = 1, 
                  shuffle    = FALSE)
    model %>% reset_states()
    cat("Epoch: ", i)
    
    
    
  }
  
  # Make Predictions
  pred_out <- model %>% 
  predict(x_test_arr, batch_size = batch_size) %>%
    .[,1] 
  

  
  # Combine actual data with predictions
  
  tbl_1 <- df_trn %>%
    add_column(key = "actual")
  tbl_2 <- df_tst %>%
    add_column(key = "actual")
  tbl_3 <- pred_tbl %>%
    add_column(key = "predict")
  
  ret <- list(tbl_1, tbl_2, tbl_3) %>%
    reduce(time_bind_rows, index = index) %>%
    arrange(key, index) %>%
    mutate(key = as_factor(key))
  
} else {
  print(paste0(i, " Branch number has less than 150 historical number of records"))
  ret <- 0
  }



#Above is LSTM function for training and testing model. Function has following logic flow:
  
#1.Filter data for an selected branch. Select only columns of index and value i.e. date and deposit amount.  
#2.Check for outliers again as there might be few residual spikes.  
#3.Remove outliers from data.   
#4.Round down the number of data points to multiple of batch size for easy division of data as LSTM has strict requirement on dimensions.  
#5.Check if the number of data points are more than 150 (if less, then ignore the branch as the number of training and test records are less).   
#6.Reserve 100 data points for testing and use rest for training.   
#7.Create a data frame with both, training and testing, label each data point accordingly.   
#8.Normalize data and capture the mean and variance to add later to output data.   
#9.Lag setting introduces few redundancies as data is being considered from first date in the data. Hence number of training samples also need to be adjusted based on lag setting so that data is equally divisible by the batch size.   
#10.Vectorize the data for train and test and create dimensions for input to LSTM.   
#11.Use model created to use it to fit to data.   
#12.Predict it on the test data.   
#13.Transform data to original scale using centre value and distribution.   
#14.Use same dates as the test data and store predicted value in the "ret" table.   
#15.This table is the return value of function.






ret_all <- as.vector(0) # empty arrary for storing all the results
ret_b <- as.vector(0) 

for (i in unique(m$SOLID)[1:5]) { # for loop across all the branches
  ret <-LSTM_Model(i) # call function and it returns a vector
  if (ret_all == 0) { # ret is output of the function. if some branches have very less data then ignore the branche and move ahead
    if(ret == 0) {
      
    }else {
      ret_b <- rep(i, nrow(ret))
      ret_all <- cbind(ret_b, ret)
    }
    
  } else {
    if (ret == 0) {# ret is output of the function. if some branches have very less data then ignore the branche and move ahead
      
    } else {
      ret_b <- rep(i, nrow(ret))
      ret_b <- cbind(ret_b, ret)
      ret_all <- rbind(ret_all, ret_b)
    }
  }
}



#Loop over sample branches to create a consolidated table. When there are many branches, a single model will not work. If we refer to earlier exercise of clustering, a model at can at best be optimized for a cluster which have common characteristics.



ggplot(ret_all, aes(index, value, color = key)) + geom_line(size = 0.2) + stat_smooth(method = "loess") + 
  facet_wrap(~ret_b, scales = "free")
#``

