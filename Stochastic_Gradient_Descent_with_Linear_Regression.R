## load the data
train = read.csv('bostonderived_train.csv')
test = read.csv('bostonderived_test.csv')
#Extract the columns of interest
train = train[,c('lstat','rm','chas','indus','tax','rad','black','medvDerived')]
test = test[,c('lstat','rm','chas','indus','tax','rad','black','medvDerived')]



getRMSE <- function(pred, actual) {
  return (sqrt(mean((pred-actual)^2)))
}

addIntercept <- function(mat) {
  ## add intercept to the matrix
  allones= rep(1, nrow(mat))
  return(cbind(Intercept=allones, mat))
}

predictSamples <- function(beta, mat) {
    ## TODO: compute the predicted value using matrix multiplication
    ## Note that for a single row of mat, pred = sum_i (beta_i * feature_i)
    predicted = list()
    for (i in seq(nrow(mat))){
      predicted[i] = beta %*% mat[i,]
    }
    return (as.double(predicted)) #predicted
}

MAX_EPOCH = 100

sgd <- function(learn.rate, train, test, epoch=MAX_EPOCH) {

  ## convert the train and test to matrix format
  train.mat = as.matrix(train) 
  test.mat = as.matrix(test)

  ## TODO: get the number of rows in the train matrix (done)
  N = nrow(train.mat)
  ## TODO: get the number of dimensions (columns) in the train matrix (done)
  d = ncol(train.mat)

  ## standardize the columns of both matrices (done)
  train.temp = train.mat
  for (j in 1:d){
    train.mat[, j] = ((train.temp[, j] - mean(train.temp[, j]))/sd(train.temp[, j]))
    test.mat[, j] = ((test.mat[, j] - mean(train.temp[, j]))/sd(train.temp[, j]))
  }
  
  ## add a feature to represent the intercept
  tmat <- addIntercept(train.mat[, -d])
  testmat <- addIntercept(test.mat[, -d])

  ## initialize all the coefficients to be 0.5
  beta = rep(0.5,d)
  j = 1
  mse.df <- NULL
  # predict training residuals
  pred_train =predictSamples(beta, tmat)
  pred_test = predictSamples(beta, testmat)
  tMse = getRMSE(pred_train, train.mat[,'medvDerived'])
  testMSE = getRMSE(pred_test, test.mat[,'medvDerived'])
  mse.df <- rbind(mse.df, data.frame(epoch=j, train=tMse, test=testMSE))

  while(j < MAX_EPOCH){  
    j=j+1;
    # for each row in the training data
    for (n in seq(1:N)){
      ##TODO: update beta according to slide #6 in APA-reg2 
      beta = beta + (learn.rate * (train.mat[n,'medvDerived'] - (beta %*% tmat[n,])) * tmat[n,])
    }
    pred_train = predictSamples(beta, tmat)
    pred_test = predictSamples(beta, testmat)
    tMse = getRMSE(pred_train, train.mat[,'medvDerived'])
    testMSE = getRMSE(pred_test, test.mat[,'medvDerived'])
    mse.df <- rbind(mse.df, data.frame(epoch=j, train=tMse, test=testMSE))
  } 
  return(mse.df)
}

## learning rate 1
l1.df <- sgd(0.0025, train, test)
plot.l1 = l1.df[,c('epoch','test')]
plot(plot.l1, main='Learning Rate = 0.0025') ##### 4 Epochs
## learning rate 2
l2.df <- sgd(0.01, train, test)
plot.l2 = l2.df[,c('epoch','test')]
plot(plot.l2, main='Learning Rate = 0.01') ##### 2 Epochs
## learning rate 3
l3.df <- sgd(0.095, train, test)
plot.l3 = l3.df[,c('epoch','test')]
plot(plot.l3, main='Learning Rate = 0.095') ##### 1 Epochs

## TODO: fit an MLR model to it
fit <- lm(medvDerived ~ ., data=train)

## calculate RMSE for LM model
lm.mse = getRMSE(predict(fit, test), test$medvDerived)
lm.train.mse = getRMSE(predict(fit,train), train$medvDerived)
