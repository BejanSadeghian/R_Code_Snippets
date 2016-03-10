##############################
##Advanced Predictive Modeling
##Homework 4 Question 2
##
##Bejan Lee Sadeghian
##November 3rd 2015
##############################
set.seed(1)
setwd("C:/Users/beins_000/Dropbox/Grad School/2015 Fall/Advanced Predictive Modeling/Homework 4")
library(glmnet)
library(ROCR)
library(ggplot2)

raw_train_std = read.csv('diabetes_train-std.csv', header=TRUE)
raw_train_log = read.csv('diabetes_train-log.csv', header=TRUE)
raw_test_log = read.csv('diabetes_test-log.csv', header=TRUE)
raw_test_std = read.csv('diabetes_test-std.csv', header=TRUE)

##Part A
test.accuracy = list()
train.accuracy = list()
# i) Standardize the columns so there is a mean 0 and unit variance
#Extract test set
test.x = as.matrix(raw_test_std[,-9])
test.y = raw_test_std[,9]

train.x = as.matrix(raw_train_std[,-9])
train.y = raw_train_std[,9]
cross.val = cv.glmnet(train.x, train.y, nfolds=10)
min.lambda = cross.val$lambda.min

ridge.model = glmnet(train.x, train.y, family='binomial', alpha=0, lambda=min.lambda, standardize = FALSE)
predict.train = predict(ridge.model, newx=train.x, type='class')
er.train.std = mean(predict.train != as.factor(train.y))
predict.test = predict(ridge.model, newx=test.x, type='class')
er.test.std = mean(predict.test != as.factor(test.y))

#Confusion Matrix 
cm.train = confusionMatrix(predict.train, train.y)
cm.test = confusionMatrix(predict.test, test.y)

test.accuracy[1] = er.test.std
train.accuracy[1] = er.train.std

#For part B/C later
predict.cont = predict(ridge.model, newx=test.x, type='response')
curve_pred = prediction(predict.cont, as.factor(test.y))
roc.perf.std = performance(curve_pred, 'tpr','fpr')
auc.std = attr(performance(curve_pred, "auc"), "y.values")[[1]]
lift.perf.std = performance(curve_pred, "lift", "rpp")


# ii) Log transformation
#Extract test set
test.x = as.matrix(raw_test_log[,-9])
test.y = raw_test_log[,9]

train.x = as.matrix(raw_train_log[,-9])
train.y = raw_train_log[,9]
cross.val = cv.glmnet(train.x, train.y, nfolds=10)
min.lambda = cross.val$lambda.min

ridge.model = glmnet(train.x, train.y, family='binomial', alpha=0, lambda=min.lambda, standardize = FALSE)
predict.train = predict(ridge.model, newx=train.x, type='class')
er.train.std = mean(predict.train != as.factor(train.y))
predict.test = predict(ridge.model, newx=test.x, type='class')
er.test.std = mean(predict.test != as.factor(test.y))

test.accuracy[2] = er.test.std
train.accuracy[2] = er.train.std

#Confusion Matrix 
cm.train = confusionMatrix(predict.train, train.y)
cm.test = confusionMatrix(predict.test, test.y)


#Create accuracy table
accuracy.table = data.frame('Data Set Transform'=c('Center & Scaled', 'Log Transform'), 'Train' = unlist(train.accuracy), 'Test' = unlist(test.accuracy))

##Part B
#Generate ROC
predict.cont = predict(ridge.model, newx=test.x, type='response')
curve_pred = prediction(predict.cont, as.factor(test.y))
roc.perf.log = performance(curve_pred, 'tpr','fpr')
auc.log = attr(performance(curve_pred, "auc"), "y.values")[[1]]
lift.perf.log = performance(curve_pred, "lift", "rpp")

tpr.points.std = attr(roc.perf.std, 'y.values')[[1]]
fpr.points.std = attr(roc.perf.std, 'x.values')[[1]]

tpr.points.log = attr(roc.perf.log, 'y.values')[[1]]
fpr.points.log = attr(roc.perf.log, 'x.values')[[1]]

df.std = data.frame('TPR'=tpr.points.std, 'FPR' = fpr.points.std)
df.log = data.frame('TPR'=tpr.points.log, 'FPR' = fpr.points.log)

ROC = ggplot() + geom_line(data=df.std, aes(y=TPR, x=FPR, colour='Standardized')) + geom_line(data=df.log, aes(y=TPR, x=FPR, colour='Log Transform')) + ggtitle("Receiver Operator Characteristic Curve") + theme(legend.title=element_blank())
#auc = attr(performance(pred, "auc"), "y.values")[[1]]

rpp.points.std = attr(lift.perf.std, 'x.values')[[1]]
lift.points.std = attr(lift.perf.std, 'y.values')[[1]]

rpp.points.log = attr(lift.perf.log, 'x.values')[[1]]
lift.points.log = attr(lift.perf.log, 'y.values')[[1]]

df.std = data.frame('Lift'=lift.points.std, 'RPP' = rpp.points.std)
df.log = data.frame('Lift'=lift.points.log, 'RPP' = rpp.points.log)

lift = ggplot()+ geom_line(data=df.std, aes(y=Lift, x=RPP, colour='Standardized')) + geom_line(data=df.log, aes(y=Lift, x=RPP, colour='Log Transform')) + ggtitle("Lift Chart") + theme(legend.title=element_blank())
