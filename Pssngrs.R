## ----------------------------------------------------------------------------------------------------------
if (!require(broom)) install.packages('broom')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(caret)) install.packages('caret')
if (!require(MASS)) install.packages('MASS')
if (!require(ROCR)) install.packages('ROCR')
if (!require(readr)) install.packages('readr')
library(broom)
library(tidyverse)
library(caret)
library(MASS)
library(ROCR)
library(readr)


## ----------------------------------------------------------------------------------------------------------
url_train <- "https://raw.githubusercontent.com/guillepena/Passenger-Satisfaction/master/train.csv"
train_data <- read.csv(url_train,header=TRUE)
url_test <- "https://raw.githubusercontent.com/guillepena/Passenger-Satisfaction/master/test.csv"
test_data <- read.csv(url_test,header=TRUE)


## ----------------------------------------------------------------------------------------------------------
table(train_data$satisfaction)


## ----------------------------------------------------------------------------------------------------------
train_data$Inflight.wifi.service = as.factor(train_data$Inflight.wifi.service)
train_data$Departure.Arrival.time.convenient = as.factor(train_data$Departure.Arrival.time.convenient)
train_data$Ease.of.Online.booking = as.factor(train_data$Ease.of.Online.booking) 
train_data$Gate.location = as.factor(train_data$Gate.location)
train_data$Food.and.drink = as.factor(train_data$Food.and.drink)
train_data$Online.boarding = as.factor(train_data$Online.boarding)
train_data$Seat.comfort = as.factor(train_data$Seat.comfort)
train_data$Inflight.entertainment = as.factor(train_data$Inflight.entertainment)
train_data$On.board.service = as.factor(train_data$On.board.service)
train_data$Leg.room.service = as.factor(train_data$Leg.room.service)
train_data$Baggage.handling = as.factor(train_data$Baggage.handling)
train_data$Checkin.service = as.factor(train_data$Checkin.service)
train_data$Inflight.service = as.factor(train_data$Inflight.service)
train_data$Cleanliness = as.factor(train_data$Cleanliness)
train_data$satisfaction = as.factor(train_data$satisfaction)
str(train_data)


## ----------------------------------------------------------------------------------------------------------
test_data$Inflight.wifi.service = as.factor(test_data$Inflight.wifi.service)
test_data$Departure.Arrival.time.convenient = as.factor(test_data$Departure.Arrival.time.convenient)
test_data$Ease.of.Online.booking = as.factor(test_data$Ease.of.Online.booking) 
test_data$Gate.location = as.factor(test_data$Gate.location)
test_data$Food.and.drink = as.factor(test_data$Food.and.drink)
test_data$Online.boarding = as.factor(test_data$Online.boarding)
test_data$Seat.comfort = as.factor(test_data$Seat.comfort)
test_data$Inflight.entertainment = as.factor(test_data$Inflight.entertainment)
test_data$On.board.service = as.factor(test_data$On.board.service)
test_data$Leg.room.service = as.factor(test_data$Leg.room.service)
test_data$Baggage.handling = as.factor(test_data$Baggage.handling)
test_data$Checkin.service = as.factor(test_data$Checkin.service)
test_data$Inflight.service = as.factor(test_data$Inflight.service)
test_data$Cleanliness = as.factor(test_data$Cleanliness)
test_data$satisfaction = as.factor(test_data$satisfaction)
str(test_data)


## ----------------------------------------------------------------------------------------------------------
train_data_copy = train_data
test_data_copy = test_data


## ----------------------------------------------------------------------------------------------------------
NA_position_train <- which(is.na(train_data_copy$Arrival.Delay.in.Minutes))
train_data_copy$Arrival.Delay.in.Minutes[NA_position_train] = mean(train_data_copy$Arrival.Delay.in.Minutes, na.rm = TRUE)
NA_position_test <- which(is.na(test_data_copy$Arrival.Delay.in.Minutes))
test_data_copy$Arrival.Delay.in.Minutes[NA_position_test] = mean(test_data_copy$Arrival.Delay.in.Minutes, na.rm = TRUE)


## ----------------------------------------------------------------------------------------------------------
est_mod <- glm(satisfaction ~ Gender + Customer.Type + Age + 
                 Type.of.Travel + Class + Flight.Distance + Inflight.wifi.service + 
                 Departure.Arrival.time.convenient + Ease.of.Online.booking + 
                 Gate.location + Food.and.drink + Online.boarding + Seat.comfort +
                 Inflight.entertainment + On.board.service + Leg.room.service +
                 Baggage.handling + Checkin.service + Inflight.service +
                 Cleanliness + Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes , data = train_data_copy, family = "binomial")

summary(est_mod)


## ----------------------------------------------------------------------------------------------------------
est_mod_1 <- glm(satisfaction ~ Customer.Type + Age + Type.of.Travel + Class + 
                  Departure.Arrival.time.convenient + Ease.of.Online.booking + 
                  Online.boarding  +
                  Baggage.handling + Checkin.service + Inflight.service +
                  Cleanliness + Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes , data = train_data_copy, family = "binomial")

summary(est_mod_1)


## ----------------------------------------------------------------------------------------------------------
predict <- predict(est_mod_1, type = 'response' , newdata=test_data_copy)

summary(predict)


## ----------------------------------------------------------------------------------------------------------
ROCRpred <- prediction(predict, test_data_copy$satisfaction)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1),text.adj = c(-0.2,1.7))


## ----------------------------------------------------------------------------------------------------------
AUC <- as.numeric(performance(ROCRpred, "auc")@y.values)
AUC


## ----------------------------------------------------------------------------------------------------------
cutoff <- seq(0,1,.1)
accuracy <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(as.numeric(predict) > x,"satisfied", "neutral or dissatisfied")
    mean(y_hat == test_data_copy$satisfaction) })

plot(cutoff,accuracy)

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff


## ----------------------------------------------------------------------------------------------------------
y_hat <- ifelse(as.numeric(predict) > best_cutoff,"satisfied", "neutral or dissatisfied")
cm <- confusionMatrix(data = as.factor(y_hat), reference = test_data_copy$satisfaction)

cm$overall["Accuracy"]

cm$byClass[c("F1","Sensitivity","Specificity","Prevalence")]



## ----------------------------------------------------------------------------------------------------------

if (!require(rpart)) install.packages('rpart')
library(rpart)


## ----------------------------------------------------------------------------------------------------------
tree <- rpart(satisfaction ~ Gender + Customer.Type + Age + 
               Type.of.Travel + Class + Flight.Distance + Inflight.wifi.service + 
               Departure.Arrival.time.convenient + Ease.of.Online.booking + 
               Gate.location + Food.and.drink + Online.boarding + Seat.comfort +
               Inflight.entertainment + On.board.service + Leg.room.service +
               Baggage.handling + Checkin.service + Inflight.service +
               Cleanliness + Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes , 
             data = train_data_copy, method = 'class', minbucket=25)


## ----------------------------------------------------------------------------------------------------------
varImp(tree)


## ----------------------------------------------------------------------------------------------------------
tree1 <- rpart(satisfaction ~  Age + Type.of.Travel + Class + Inflight.wifi.service + 
                Ease.of.Online.booking + Online.boarding + Seat.comfort +
               Inflight.entertainment + On.board.service + Leg.room.service +
               Baggage.handling + Checkin.service + Inflight.service +
               Cleanliness + Arrival.Delay.in.Minutes,
               data = train_data_copy, method = 'class', minbucket=25)

plot(tree1, margin = 0.1)
text(tree1, cex = 0.4)


## ----------------------------------------------------------------------------------------------------------
predict_cart <- predict(tree1, newdata = test_data_copy, class="tree")


## ----------------------------------------------------------------------------------------------------------
#Plotting the ROC Curve
pred <- prediction(predict_cart[,2], test_data_copy$satisfaction)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1),text.adj = c(-0.2,1.7))


## ----------------------------------------------------------------------------------------------------------
########################### Optimization Part
# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01))


## ----------------------------------------------------------------------------------------------------------
train_rpart <- train(satisfaction ~  Age + Type.of.Travel + Class + Inflight.wifi.service + 
        Ease.of.Online.booking + Online.boarding + Seat.comfort +
        Inflight.entertainment + On.board.service + Leg.room.service +
        Baggage.handling + Checkin.service + Inflight.service +
        Cleanliness + Arrival.Delay.in.Minutes, 
      data = train_data_copy, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

plot(train_rpart)


## ----------------------------------------------------------------------------------------------------------
y_hat_cart <- predict(train_rpart,test_data_copy)
cm_cart <- confusionMatrix(data = as.factor(y_hat_cart), reference = test_data_copy$satisfaction)

cm_cart$overall["Accuracy"]

cm_cart$byClass[c("F1","Sensitivity","Specificity","Prevalence")]


## ----------------------------------------------------------------------------------------------------------
cm$byClass["F1"]


## ----------------------------------------------------------------------------------------------------------
cm_cart$byClass["F1"]

