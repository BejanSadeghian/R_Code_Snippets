library(MASS)
load(file='rrdata.RData')

##PART A
#Cross Validation Method
#Create the test and train dataset
train = rrdata[1:40,]
test = rrdata[41:51,]

#Multiple Linear Regression
regress.ols = lm(crime ~ poverty + single, data = train)
predict.ols = predict(regress.ols,test)
mean((predict.ols-test$crime)^2) #MSE

#Robust Regression
regress.robust = rlm(crime ~ poverty + single, data = train)
predict.rob = predict(regress.robust, test)
mean((predict.rob-test$crime)^2)

#If using the entire dataset as the training dataset then MSE is below...
#Multiple Linear Regression
regress.ols2 = lm(crime ~ poverty + single, data = rrdata)
predict.ols2 = predict(regress.ols2,rrdata)
mean((predict.ols2-rrdata$crime)^2) #MSE

#Robust Regression
regress.robust2 = rlm(crime ~ poverty + single, data = rrdata)
predict.rob2 = predict(regress.robust2, rrdata)
mean((predict.rob2-rrdata$crime)^2)
par(mfrow = c(2,2))

plot(rrdata$crime, predict.ols2, xlab='Actual Crime', ylab='MLR Predict', main='All Data - Multiple Linear Regression')
plot(rrdata$crime, predict.rob2, xlab='Actual Crime', ylab='Robust Predict', main='All Data - Robust Linear Regression (Huber Loss)')

summary(regress.ols)
summary(regress.robust)
summary(regress.ols2)
summary(regress.robust2)
##PART B
#Residual Plot from the MLR method
delta = abs(predict.ols2-rrdata$crime)
#scatter = plot(rrdata$crime, delta, xlab='Real Crime Value', ylab='abs(Predicted-Actual)')

#Identify the outliers' index
#outliers = identify(x=rrdata$crime, y=delta)

#Remove the outliers from the data and reset the rownames
rrdataNoOutlier = rrdata[-outliers,]
rrdataNoOutlier = na.omit(rrdataNoOutlier)
rownames(rrdataNoOutlier) = seq(length=nrow(rrdataNoOutlier))

#Rerun Regressions, this time with cross validation
train = rrdataNoOutlier[1:38,]
test = rrdataNoOutlier[39:47,]

#Multiple Linear Regression
regress.ols = lm(crime ~ poverty + single, data = train)
predict.ols = predict(regress.ols,test)
mean((predict.ols-test$crime)^2) #MSE

#Robust Regression
regress.robust = rlm(crime ~ poverty + single, data = train)
predict.rob = predict(regress.robust, test)
mean((predict.rob-test$crime)^2)

#Rerun Regressions, this time using the entire dataset as a training set and test set
#Multiple Linear Regression
regress.ols2 = lm(crime ~ poverty + single, data = rrdataNoOutlier)
predict.ols2 = predict(regress.ols2,rrdataNoOutlier)
mean((predict.ols2-rrdataNoOutlier$crime)^2) #MSE

#Robust Regression
regress.robust2 = rlm(crime ~ poverty + single, data = rrdataNoOutlier)
predict.rob2 = predict(regress.robust2, rrdataNoOutlier)
mean((predict.rob2-rrdataNoOutlier$crime)^2)

plot(rrdataNoOutlier$crime, predict.ols2, xlab='Actual Crime', ylab='MLR Predict', main='Outlier Removed - Multiple Linear Regression')
plot(rrdataNoOutlier$crime, predict.rob2, xlab='Actual Crime', ylab='Robust Predict', main='Outlier Removed - Robust Linear Regression (Huber Loss)')


summary(regress.robust)
summary(regress.ols)
