# clear all objects from the workspace
rm(list = ls())

# import basic libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(patchwork)
library(tidyverse)
library(ISLR)
library(fastDummies)
library(DiagrammeR)
library(caret)
library(forecast)

# read the data
setwd("~/Desktop/RCode/Data Analytics/Project")
data <- read.csv("2020_Green_Taxi_Trip_Data__January_-_June_.csv")

# check the data
dim(data)
head(data)
names(data)
str(data)

# Check the level of categorical data
table(data$VendorID)
table(data$store_and_fwd_flag)
table(data$RatecodeID)
table(data$PULocationID)
table(data$DOLocationID)
table(data$payment_type)
table(data$trip_type)

# date-time transformation
data$pickup_time <- as.POSIXlt(strptime(data$lpep_pickup_datetime, format ="%m/%d/%Y %I:%M:%S %p"))
data$dropoff_time <- as.POSIXlt(strptime(data$lpep_dropoff_datetime, format ="%m/%d/%Y %I:%M:%S %p"))

# extract date information from pick-up time
data$pickup_month <- data$pickup_time$mon
data$pickup_wday <- data$pickup_time$wday
data$pickup_hour <- data$pickup_time$hour
data$pickup_day <- data$pickup_time$yday

# calculate the duration
data$timediff <- as.numeric(difftime(data$dropoff_time, data$pickup_time, units="hours"))

# calculate the speed
data$speed <- data$trip_distance/data$timediff

# generate new feature if_tip
data$if_tip <- ifelse(data$tip_amount>0, 1, 0)

# generate new feature if_airport
data$if_airport <- ifelse((data$RatecodeID==2)|(data$RatecodeID==3), 1, 0)

# calculate tip percentage to the total amount
data$tip_percentage <- data$tip_amount/data$total_amount*100

# double check missing values of all numeric variables
NAs <- sapply(data, function(x) sum(is.na(x)))
NAs

# remove unuseful columns
df.temp <- subset(data, select = -c(lpep_pickup_datetime, lpep_dropoff_datetime, pickup_time, dropoff_time,
                                    PULocationID, DOLocationID,ehail_fee))

# data cleaning - filter out rows
df.temp %>% filter(!is.na(speed),
                !is.na(payment_type),
                total_amount>2.5 & total_amount<200,
                tip_amount>=0,
                trip_distance>0 & trip_distance<100,
                timediff > 0.02,
                speed < 140,
                pickup_month < 6
               ) %>%
  {. ->> df.model }   #here is save

# double check missing values of all numeric variables
NAs <- sapply(df.model, function(x) sum(is.na(x)))
NAs

table(df.model$payment_type, df.model$if_tip)
table(df.model$RatecodeID, df.model$if_tip)

fivenum(df.model$total_amount)
fivenum(df.model$trip_distance)
fivenum(df.model$timediff)
fivenum(df.model$speed)
fivenum(df.model$tip_amount)
fivenum(df.model$tip_percentage)

boxplot(df.model$tip_percentage)

boxplot(df.model$total_amount)
hist(df.model$total_amount)

ggplot(df.model, aes(x=total_amount)) + geom_histogram(bins = 50)
ggplot(df.model, aes(x=trip_distance)) + geom_histogram()
ggplot(df.model, aes(x=timediff)) + geom_histogram(bins = 100)
ggplot(df.model, aes(x=tip_amount)) + geom_histogram(bins = 50)
ggplot(df.model, aes(x=speed)) + geom_histogram(bins=100)

# Data comes from the cleaned dataset
boxplot(df.model$total_amount ~ df.model$RatecodeID, 
        col="orange", main="Distribution of Total Amount", ylab="Total Amount", xlab="Rate Code ID") 

boxplot(df.model$total_amount ~ df.model$if_airport, 
        col="orange", main="Distribution of Total Amount", ylab="Total Amount", xlab="If airport trip") 

boxplot(df.model$tip_percentage ~ df.model$if_airport, 
        col="orange", main="Distribution of Tip Percentage", ylab="Tip Percentage", xlab="If airport trip") 

boxplot(df.model$tip_percentage ~ df.model$RatecodeID, 
        col="orange", main="Distribution of Tip Percentage", ylab="Tip Percentage", xlab="RateCode ID") 

boxplot(df.model$tip_amount ~ df.model$if_airport, 
        col="orange", main="Distribution of Tip Amount", ylab="Tip Amount", xlab="If airport trip") 

boxplot(df.model$speed ~ df.model$if_airport, 
        col="orange", main="Distribution of Speed", ylab="Speed", xlab="If airport trip") 

boxplot(df.model$total_amount ~ df.model$VendorID, 
        col="orange", main="Distribution of Total Amount", ylab="Total Amount", xlab="Vendor ID") 

boxplot(df.model$speed ~ df.model$VendorID, 
        col="orange", main="Distribution of Speed", ylab="Speed", xlab="Vendor ID") 

boxplot(df.model$tip_amount ~ df.model$VendorID, 
        col="orange", main="Distribution of Tip Amount", ylab="Tip Amount", xlab="Vendor ID") 

boxplot(df.model$tip_amount ~ df.model$pickup_month, 
        col="orange", main="Distribution of Tip Amount", ylab="Tip Amount", xlab="Month") 

boxplot(df.model$speed ~ df.model$RatecodeID, 
        col="orange", main="Distribution of Speed", ylab="Speed", xlab="Rate Code ID") 

boxplot(df.model$total_amount ~ df.model$pickup_wday, 
        col="orange", main="Distribution of Total Amount", ylab="Total Amount", xlab="Week of Day") 

boxplot(df.model$speed ~ df.model$pickup_wday, 
        col="orange", main="Distribution of Speed", ylab="Speed", xlab="Week of Day") 

boxplot(df.model$total_amount ~ df.model$pickup_month, 
        col="orange", main="Distribution of Total Amount", ylab="Total Amount", xlab="Month") 

boxplot(df.model$speed ~ df.model$pickup_month, 
        col="orange", main="Distribution of Speed", ylab="Speed", xlab="Month") 

# aggregate data
df.model %>% 
  group_by(pickup_day) %>% 
  summarize(avg_amount = mean(total_amount, na.rm=TRUE), sum_amount = sum(total_amount, na.rm=TRUE), n=n()) %>%
  {. ->> data_by_day } %>%   #here is save
  tbl_df %>% 
  print(n = Inf)

dim(data_by_day)
names(data_by_day)

# plot daily volume of orders
ggplot(data=data_by_day, aes(x=pickup_day, y=n, group=1)) +
  geom_line(color="black")+
  geom_point(color="grey")+
  labs(title="Trip Volume overtime",
            x ="Pickup day", y = "Trip Volume")

ggplot(data=data_by_day, aes(x=pickup_day, y=avg_amount, group=1)) +
  geom_line(color="black")+
  geom_point(color="grey")+
  labs(title="Average Trip Amount",
       x ="Pickup day", y = "Trip Amount")

ggplot(data=data_by_day, aes(x=pickup_day, y=sum_amount, group=1)) +
  geom_line(color="black")+
  geom_point(color="grey")+
  labs(title="Sum of Trip Amount by day",
       x ="Pickup day", y = "Sum of Trip Amount")

# read the new data in 
covid <- read.csv("data-tdakA.csv")
dim(covid)
names(covid)
str(covid)

covid$date_covid <- as.POSIXlt(strptime(covid$date_of_interest, format ="%m/%d/%Y"))
covid$day_covid <- covid$date_covid$yday

data_by_day <- data_by_day %>% left_join(covid, by = c("pickup_day" = "day_covid"))

# create heatmap or correlation maps
data_by_day_corr <- subset(data_by_day, select = -c(pickup_day, date_of_interest, date_covid, INCOMPLETE, BASELINE))
data_by_day_corr_nona <- na.omit(data_by_day_corr)

head(data_by_day_corr_nona)
tail(data_by_day_corr_nona)

#heatmap(as.matrix(data_by_day_corr_nona))

corrplot(cor(data_by_day_corr_nona),method="shade")

# Dual y axis plot of Trip volume and new cases
p <- ggplot(data_by_day, aes(x = pickup_day))
p <- p + geom_line(aes(y = Cases, colour = "New confirmed Cases"))

# adding the another dimension data, transformed to match roughly the range of the first one
p <- p + geom_line(aes(y = n/3, colour = "Trip Volume"))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*3, name = "Trip Volume per day"))

# modifying colours and theme options
p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "New confirmed cases in NYC",
              x = "Date and time",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.8, 0.9))
p

# Dual y axis plot of sum of trip amounts and new cases
p <- ggplot(data_by_day, aes(x = pickup_day))
p <- p + geom_line(aes(y = Cases, colour = "New confirmed Cases"))

# adding the another dimension data, transformed to match roughly the range of the first one
p <- p + geom_line(aes(y = sum_amount/70, colour = "Sum of trip amount per day"))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*70, name = "Sum of trip amount per day"))

# modifying colours and theme options
p <- p + scale_colour_manual(values = c("green", "grey"))
p <- p + labs(y = "New confirmed cases in NYC",
              x = "Date and time",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.8, 0.9))
p

# apply the models
names(df.model)
dim(df.model)
str(df.model)

# change categorical data into factors
df.model$VendorID <- as.factor(df.model$VendorID)
df.model$RatecodeID <- as.factor(df.model$RatecodeID)
df.model$payment_type <- as.factor(df.model$payment_type)
df.model$pickup_month <- as.factor(df.model$pickup_month)
df.model$pickup_wday <- as.factor(df.model$pickup_wday)
df.model$pickup_hour <- as.factor(df.model$pickup_hour)
#df.model$if_tip <- as.factor(df.model$if_tip)
df.model$if_airport <- as.factor(df.model$if_airport)

df.model$pickup_day <- NULL
df.model$tip_amount <- NULL
df.model$tip_percentage <- NULL

df.model$payment_type <- NULL

# convert factors into dummies
# dummy_cols(df.model, remove_first_dummy = TRUE,remove_selected_columns = TRUE)

# train-test split
## 70% of the sample size
smp_size <- floor(0.70 * nrow(df.model))

## set the seed to make your partition reproducible
set.seed(123654)
train_ind <- sample(seq_len(nrow(df.model)), size = smp_size)

train <- df.model[train_ind, ]
test <- df.model[-train_ind, ]

########################## Linear regression model to predict tip percentage ##########################
lr <- lm(tip_percentage ~., data = train)
summary(lr)

# Other useful functions
coefficients(lr) # model coefficients
confint(lr, level=0.95) # CIs for model parameters
fitted(lr) # predicted values
plot(residuals(lr)) # residuals
anova(lr) # anova table
vcov(lr) # covariance matrix for model parameters
#influence(lr) # regression diagnostics

# show the first few records of actual FARE, predicted values, and residuals 
tr.res <- data.frame(train$tip_percentage, lr$fitted.values, lr$residuals)
head(tr.res)

# draw a histogram for residuals of train data
hist(lr$residuals,xlab="Residuals for train data")

# apply the regression model to predict validation set
pred <- predict(lr, newdata = test)
vl.res <- data.frame(test$tip_percentage, pred, residuals = test$tip_percentage - pred)
head(vl.res)

# draw a histogram for residuals of validation data
hist(test$tip_percentage - pred,xlab="Residuals for test data")

# compute accuracy on training set
accuracy(lr$fitted.values, train$tip_percentage)

# compute accuracy on prediction set
accuracy(pred, test$tip_percentage)

########################## Xgboosting Modeling ##########################
library(xgboost) # for xgboost
library(xgboostExplainer)

# convert dataframe into a matrix
df.matrix <- data.matrix(df.model)
df.matrix

# get the numb 70/30 training test split
numberOfTrainingSamples <- round(dim(df.model)[1] * .7)

# training data
train_data <- df.matrix[1:numberOfTrainingSamples,]
dim(train_data)

train_features <- train_data[,c(1:18,20)]
train_labels <- train_data[,19]

# testing data
test_data <- df.matrix[-(1:numberOfTrainingSamples),]
dim(test_data)

test_features <- test_data[,c(1:18,20)]
test_labels <- test_data[,19]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_features, label= train_labels)
dtest <- xgb.DMatrix(data = test_features, label= test_labels)

# train a model using our training data
model <- xgboost(data = dtrain, # the data   
                 nround = 2, # max number of boosting iterations
                 objective = "binary:logistic")  # the objective function

# generate predictions for our held-out testing data
pred <- predict(model, dtest)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))

# and transform them in a 0 1 variable, you can choose the value to get 1
pred_bin <-  as.numeric(pred > 0.5)

confusionMatrix(factor(pred_bin),factor(test_labels))

# model visualiazation
# plot them features! what's contributing most to our model?
xgb.plot.multi.trees(feature_names = names(df.matrix), 
                     model = model)

# get information on how important each feature is
importance_matrix <- xgb.importance(names(df.matrix), model = model)

# and plot it!
xgb.plot.importance(importance_matrix)

# model explainer
explainer = buildExplainer(model, dtrain, type="binary", base_score = 0.5, trees_idx = NULL)
pred.breakdown = explainPredictions(model, explainer, dtest)
cat('Breakdown Complete','\n')
weights = rowSums(pred.breakdown)
pred.xgb = 1/(1+exp(-weights))
cat(max(pred-pred.xgb),'\n')
idx_to_get = as.integer(6) #20
test_data[idx_to_get,-19]
showWaterfall(model, explainer, dtest, data.matrix(test_data[,-19]) ,idx_to_get, type = "binary")

##################### decision tree modeling #######################
library(rpart)
library(rpart.plot)

dt <- rpart(if_tip~., data = train, method = "class")
dt

rpart.plot(dt)

pred <- predict(object=dt,test[-19],type="class")
t <- table(test$if_tip,pred)
confusionMatrix(t)






















