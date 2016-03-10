library(ggplot2)
library(nlme)

rawdata = read.csv('oxboys.csv')


#Creating a Function to average a list (lapply isnt working)
averageList <- function(X){
  counter = 0
  sum = 0
  for(i in X){
    sum = i + sum
    counter = counter + 1
  }
  return (sum/counter)
}

##Part A
fit = lm(height ~ year, data= rawdata)

plot.df = rawdata[,c('year','height')]
plot(plot.df, main='Height vs Year')
abline(fit, col='red')


##Part B

ggplot(rawdata, aes(x=year, y=height, group=id)) + stat_smooth(method='lm') + ggtitle('Regression Line for Each Individual')


##Part C / Part D

for(N in c(3,7)){
  print(paste0('Train/Test Seperated by ', N-1, ' years.'))
  train = subset(rawdata, year < N)
  test = subset(rawdata, year >= N)
  
  #Global Model
  g.fit = lm(height ~ year, data = train)
  g.predict = predict(g.fit, newdata=test)
  g.MSE = mean((g.predict - test$height)^2)
  g.RMSE = sqrt(mean((g.predict - test$height)^2))
  print(paste0(g.RMSE, ' ::::: Global Model RMSE'))
  print(paste0(g.MSE, ' ::::: Global Model MSE'))
  
  #Local Model
  l.RMSE = list()
  l.MSE = list()
  for(i in unique(train$id)){
    unique.train = subset(train, id == i)
    unique.test = subset(test, id == i)
    l.fit = lm(height~ year, data=unique.train)
    l.predict = predict(l.fit, newdata = unique.test)
    l.RMSE[i] = sqrt(mean((l.predict - unique.test$height)^2))
    l.MSE[i] = mean((l.predict - unique.test$height)^2)
  }
  
  print(paste0(averageList(l.RMSE),' ::::: Local Model RMSE Average'))
  print(paste0(averageList(l.MSE),' ::::: Local Model MSE Average'))

  #Multilevel Model
  
  mlm.fit = lme(height~year, data = train, random=list(id=pdDiag(~year)))
  mlm.predict = predict(mlm.fit, newdata = test, level=1, asList=TRUE)
  
  mlm.RMSE = list()
  mlm.MSE = list()
  for(i in unique(train$id)){
    temp.test = subset(test, id == i)
    mlm.RMSE[i] = sqrt(mean((as.data.frame(mlm.predict[i]) - temp.test[['height']])^2))
    mlm.MSE[i] = mean((as.data.frame(mlm.predict[i]) - temp.test[['height']])^2)
  }
  
  print(paste0(averageList(mlm.RMSE), ' ::::: Multilevel Model RMSE Average'))
  print(paste0(averageList(mlm.MSE), ' ::::: Multilevel Model MSE Average'))
  if(N == 3){
    print('----------------------------------------------------')
  }
}


