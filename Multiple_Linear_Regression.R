par(mfrow = c(1,2))

load(file='BostonDerived.rData')
rawdata = Boston
Bostrain = Boston[1:300,]
Bostest = Boston[301:506,]
rawdata$medv.bin = cut(rawdata$medv,10) #Cont var Broken down into ten bins
saveplot = boxplot(rawdata$lstat, main = 'LSTAT')
saveplot1 = boxplot(rawdata$medv, main = 'MEDV')

outliers = list()
inc = 1
for(i in rawdata$lstat){
  if(i %in% saveplot$out){
    print(i)
    outliers = append(outliers, inc)
  }
  inc = inc + 1
}

inc = 1
for(i in rawdata$medv){
  if(i %in% saveplot1$out && !(i %in% outliers)){
    print(i)
    outliers = append(outliers, inc)
  }
  inc = inc + 1
}

plot(rawdata$medv, rawdata$lstat, col=ifelse(rownames(rawdata) %in% outliers, 'red', 'black'))
saveplot$stats[c(1,5),]
saveplot1$stats[c(1,5),]


names(Bostrain)

regress.bos = lm('medvDerived ~ lstat + rm + chas + indus + tax + rad + black', data=Bostrain)

train.predict = predict.lm(regress.bos, Bostrain)
mean((train.predict-Bostrain$medvDerived)^2)

test.predict = predict.lm(regress.bos, Bostest)
mean((test.predict-Bostest$medvDerived)^2)

summary = summary(regress.bos)
mean(summary$residuals^2)
train.predict$residual.scale
