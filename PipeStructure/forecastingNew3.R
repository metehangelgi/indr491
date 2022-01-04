# Title     : TODO
# Objective : TODO
# Created by: metehangelgi
# Created on: 4.01.2021
library(e1071)
library("dplyr")
library(readr)
library(forecast)
library(glmnet)
library(factoextra)
library(cluster)
library(tsintermittent)


is.rankdeficient <- function(xregg) {
  constant_columns <- apply(xregg, 2, is.constant)
  if (any(constant_columns)) {
    xregg <- xregg[, -which(constant_columns)[1]]
  }
  sv <- svd(na.omit(cbind(rep(1, NROW(xregg)), xregg)))$d
  min(sv)/sum(sv) < .Machine$double.eps
}

writeCSV <- function (final_data,numofSample){
  #ABCtype,SBCtype,clustering will be added later
  output00 <- c("forecast/new", numofSample,".csv")
  output002 <- paste(output00, collapse="")
  write.csv(final_data,output002, row.names = FALSE)
}

args = commandArgs(trailingOnly=TRUE)
numofSample=as.character(args[1])
inputy <- c("featureCreation/new", numofSample,"Y.csv")
inputy2 <- paste(inputy, collapse="")
inputx <- c("featureCreation/new", numofSample,".csv")
inputx2 <- paste(inputx, collapse="")
#inputLasso<-c("featureSelection/new", numofSample,".csv")
#inputLasso2 <- paste(inputLasso, collapse="")
inputElastic<-c("featureSelection/newElastic", numofSample,".csv")
inputElastic2 <- paste(inputElastic, collapse="")
ydata <- read_csv(inputy2)
xdata <- read_csv(inputx2)
#lasso_coefs <- read_csv(inputLasso2)
elastic_coefs <- read_csv(inputElastic2)

#clusteredInput <- c("clustering/new", numofSample,".csv")
#clusteredInput2 <- paste(clusteredInput, collapse="")
#clusteredData <- read_csv(clusteredInput2)


# exclude brandid,gender,size
patterns <- c("brand_ID_.*","size_.*","gender_.*")
any_matching = Reduce(`|`, lapply(patterns, grepl, colnames(xdata)))
resultX = colnames(xdata)[! any_matching]
drop <- resultX
xdata=xdata[,(names(xdata) %in% drop)]

prodIDs=xdata[['product_id']]
ux <- unique(prodIDs)
forecastingGroupsErrorsWhole <- NULL
for (prodIDIndex in 1:length(ux))
  {

  #lasso_coef <- filter(lasso_coefs, product_id == ux[prodIDIndex])[, -1] ## lasso
  elastic_coef <- filter(elastic_coefs, product_id == ux[prodIDIndex])[, -1] ## elastic
  prod_x <- filter(xdata, product_id == ux[prodIDIndex])[, -1] ## product x data
  prod_y <- filter(ydata, product_id == ux[prodIDIndex])[, -1] ## product y data
  #clusteredDataGivenID <- filter(clusteredData, product_id == ux[prodIDIndex])
  # df = cbind(prod_y, prod_x)
  #prod_y_train <- head(prod_y, round(nrow(prod_y) * 0.8))
  prod_y_train <- head(prod_y, nrow(prod_y)-7)
  h <- nrow(prod_y) - nrow(prod_y_train)
  prod_y_test <- tail(prod_y, h)

  #prod_x_train <- head(prod_x, round(nrow(prod_x) * 0.8))
  prod_x_train <- head(prod_x, nrow(prod_x)-7)
  h <- nrow(prod_x) - nrow(prod_x_train)
  prod_x_test <- tail(prod_x, h)

  methods <- list()
  maseErrors <- vector()
  maeErrors <- vector()
  errors <- list()
  predicts <- list()
  i <- 1
  regressors <- which(elastic_coef!= 0) # if elastic net is desired
  #rank deficient sıkıntısı var çözemedim, şimdilik bu şekilde handle ettim
  rankTest=is.rankdeficient(data.matrix(prod_x[,regressors], rownames.force = NA))
  #dynamic
  if (!(rankTest)){

    ARIMAmodelNotfound <- tryCatch(
      auto.arima((prod_y[,"sales"]), xreg=data.matrix(prod_x[,regressors], rownames.force = NA)),
      error=function(e) e
         )

    if(!inherits(ARIMAmodelNotfound, "error")){
      dyano.fit <- auto.arima((prod_y[,"sales"]),
                              xreg=data.matrix(prod_x[,regressors], rownames.force = NA))
      RegArimaErr <- cbind("Regression Errors" = residuals(dyano.fit, type="regression"),
          "ARIMA errors" = residuals(dyano.fit, type="innovation"))

      #checkresiduals(dyano.fit)  #ne ise yarıyor

      #dynamic
      dyno.forecast <- dyano.fit %>% forecast::forecast(xreg = as.matrix(prod_x_test[,regressors]),h=7)
      dyno.predict <-as.data.frame(dyno.forecast)[["Point Forecast"]]
      predicts[[i]] <- dyno.predict
      dyno.err <- measures(as.matrix(prod_y_test$sales), dyno.predict, as.matrix(prod_y_test$sales), benchmark = "naive")
      dyno.errorFrame <- t(as.data.frame(dyno.err))
      errors[[i]] <- dyno.errorFrame
      #dyno.MaseError<-(as.data.frame(dyno.errorFrame))$MASE[[1]]
      dyno.MaseError<-(as.data.frame(dyno.errorFrame))$MASE[[1]]
      maseErrors <- c(maseErrors, dyno.MaseError)
      dyno.MaeError<-(as.data.frame(dyno.errorFrame))$MAE[[1]]
      maeErrors <- c(maseErrors, dyno.MaeError)
      methods[[i]] <- "dyno"
      i <- i + 1
      #print("dyno done")
      #dyno.predict <- accuracy(dyno.forecast, prod_y_test$sales)
    }
  }

  #ets
  methods[[i]] <- "ets"
  ets.fit<- ets(as.vector(prod_y_train$sales))
  ets.forecast <- forecast(ets.fit,h=7)
  ets.predict<-as.data.frame(ets.forecast)[["Point Forecast"]]
  predicts[[i]] <- ets.predict
  ets.err <- measures(as.matrix(prod_y_test$sales), ets.predict, as.matrix(prod_y_test$sales), benchmark = "naive")
  ets.errorFrame <- t(as.data.frame(ets.err))
  errors[[i]] <- ets.errorFrame
  #ets.MaseError<-(as.data.frame(ets.errorFrame))$MASE[[1]]
  ets.MaseError<-(as.data.frame(ets.errorFrame))$MASE[[1]]
  maseErrors <- c(maseErrors, ets.MaseError)
  ets.MaeError<-(as.data.frame(ets.errorFrame))$MAE[[1]]
  maeErrors <- c(maseErrors, ets.MaeError)
  i <- i + 1
  #print("ets done")

  #crosten
  methods[[i]] <- "crost"
  crost.fit <- crost(prod_y_train[["sales"]], h=7, w=NULL, type="sba", outplot = TRUE)
  crost.predict = as.data.frame(crost.fit$components$c.out)$Demand
  predicts[[i]] <- crost.predict
  crost.err <- measures(as.matrix(prod_y_test$sales), crost.predict, as.matrix(prod_y_test$sales), benchmark = "naive")
  crost.errorFrame <- t(as.data.frame(crost.err))
  errors[[i]] <- crost.errorFrame
  #crost.MaseError<-(as.data.frame(crost.errorFrame))$MASE[[1]]
  crost.MaseError<-(as.data.frame(crost.errorFrame))$MASE[[1]]
  maseErrors <- c(maseErrors, crost.MaseError)
  crost.MaeError<-(as.data.frame(crost.errorFrame))$MAE[[1]]
  maeErrors <- c(maseErrors, crost.MaeError)
  i <- i + 1
  #print("crost done")


  #tsb
  methods[[i]] <- "tsb"
  tsb.fitt <- tsb(prod_y_train[["sales"]], h=7, init = "naive")
  tsb.predicted = tsb.fitt$frc.out
  predicts[[i]] <- tsb.predicted
  tsb.err2 <- measures(as.matrix(prod_y_test$sales), tsb.predicted, as.matrix(prod_y$sales), benchmark = "naive")
  tsb.errorFrame <- t(as.data.frame(tsb.err2))
  errors[[i]] <- tsb.errorFrame
  #crost.MaseError<-(as.data.frame(crost.errorFrame))$MASE[[1]]
  tsb.MaseError<-(as.data.frame(tsb.errorFrame))$MASE[[1]]
  maseErrors <- c(maseErrors, tsb.MaseError)
  tsb.MaeError<-(as.data.frame(tsb.errorFrame))$MAE[[1]]
  maeErrors <- c(maseErrors, tsb.MaeError)
  i <- i + 1
  #print("tsb done")

  # I didn't get what we are calculating here
  #if((as.data.frame(err2)$err2)[9] < 1){
  #  counter = counter + 1
  #}
  #print(counter)

  #svm
  methods[[i]] <- "svm"
  regressors <- which(elastic_coef!= 0) # if elastic net is desired
  new_data_train <- cbind(prod_y_train, prod_x_train[, regressors])
  new_data_test <- cbind(prod_y_test, prod_x_test[, regressors])
  tuneResult <- tune(svm, sales ~ .,  data = new_data_train,
                        ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
  svm.predicted = predict(tuneResult$best.model, new_data_test)
  predicts[[i]] <- svm.predicted
  svm.err2 <- measures(as.matrix(prod_y_test$sales), svm.predicted, as.matrix(prod_y$sales), benchmark = "naive")
  svm.errorFrame <- t(as.data.frame(svm.err2))
  errors[[i]] <- svm.errorFrame
  #crost.MaseError<-(as.data.frame(crost.errorFrame))$MASE[[1]]
  svm.MaseError<-(as.data.frame(svm.errorFrame))$MASE[[1]]
  maseErrors <- c(maseErrors, svm.MaseError)
  svm.MaeError<-(as.data.frame(svm.errorFrame))$MAE[[1]]
  maeErrors <- c(maseErrors, svm.MaeError)
  #print("svm done")
  i <- i + 1

  #combined Methods
  iterateMethods <- methods
  iteratePredicts <- predicts

  # combination 2 methods
  for (index in c(1:(length(iterateMethods)-1))){
    for (index2 in c((index+1):length(iterateMethods))){
      method1 <- iterateMethods[index]
      method2 <- iterateMethods[index2]
      predict1 <- iteratePredicts[index]
      predict2 <- iteratePredicts[index2]
      methods[[i]] <- paste(c(method1,method2),sep = ",")
      # direct combination
      #print(method1)
      #print(c(predict1[[1]]))
      #print(method2)
      #print(c(predict2[[1]]))
      combined.predict=(c(predict1[[1]]) + c(predict2[[1]]))/2
      predicts[[i]] <- combined.predict
      combined.err <- measures(as.matrix(prod_y_test$sales), combined.predict, as.matrix(prod_y_test$sales), benchmark = "naive")
      combined.errorFrame <- t(as.data.frame(combined.err))
      errors[[i]] <- combined.errorFrame
      combined.MaseError<-(as.data.frame(combined.errorFrame))$MASE[[1]]
      maseErrors <- c(maseErrors, combined.MaseError)
      combined.MaeError<-(as.data.frame(combined.errorFrame))$MAE[[1]]
      maeErrors <- c(maseErrors, combined.MaeError)
      i <- i + 1
    }
  }

  # combination 3 methods
  indices <- list(c(1,2,3),c(1,2,4),c(1,2,5),c(1,3,4),c(1,3,5),c(1,4,5),c(2,3,4),c(2,3,5),c(2,4,5),c(3,4,5))
  for (indices2 in indices){
    method1 <- iterateMethods[indices2[1]]
    method2 <- iterateMethods[indices2[2]]
    method3 <- iterateMethods[indices2[3]]
    predict1 <- iteratePredicts[indices2[1]]
    predict2 <- iteratePredicts[indices2[2]]
    predict3 <- iteratePredicts[indices2[3]]
    methods[[i]] <- paste(c(method1,method2,method3),sep = ",")
    # direct combination
    combined.predict=(c(predict1[[1]]) + c(predict2[[1]]) + c(predict3[[1]]))/3
    predicts[[i]] <- combined.predict
    combined.err <- measures(as.matrix(prod_y_test$sales), combined.predict, as.matrix(prod_y_test$sales), benchmark = "naive")
    combined.errorFrame <- t(as.data.frame(combined.err))
    errors[[i]] <- combined.errorFrame
    combined.MaseError<-(as.data.frame(combined.errorFrame))$MASE[[1]]
    maseErrors <- c(maseErrors, combined.MaseError)
    combined.MaeError<-(as.data.frame(combined.errorFrame))$MAE[[1]]
    maeErrors <- c(maseErrors, combined.MaeError)
    i <- i + 1
  }

    # combination 4 methods
  indices <- list(c(1,2,3,4),c(1,2,3,5),c(1,2,4,5),c(1,3,4,5),c(2,3,4,5))
  for (indices2 in indices){
    method1 <- iterateMethods[indices2[1]]
    method2 <- iterateMethods[indices2[2]]
    method3 <- iterateMethods[indices2[3]]
    method4 <- iterateMethods[indices2[4]]
    predict1 <- iteratePredicts[indices2[1]]
    predict2 <- iteratePredicts[indices2[2]]
    predict3 <- iteratePredicts[indices2[3]]
    predict4 <- iteratePredicts[indices2[4]]
    methods[[i]] <- paste(c(method1,method2,method3,method4),sep = ",")
    # direct combination
    combined.predict=(c(predict1[[1]]) + c(predict2[[1]]) + c(predict3[[1]]) + c(predict4[[1]]))/4
    predicts[[i]] <- combined.predict
    combined.err <- measures(as.matrix(prod_y_test$sales), combined.predict, as.matrix(prod_y_test$sales), benchmark = "naive")
    combined.errorFrame <- t(as.data.frame(combined.err))
    errors[[i]] <- combined.errorFrame
    combined.MaseError<-(as.data.frame(combined.errorFrame))$MASE[[1]]
    maseErrors <- c(maseErrors, combined.MaseError)
    combined.MaeError<-(as.data.frame(combined.errorFrame))$MAE[[1]]
    maeErrors <- c(maseErrors, combined.MaeError)
    i <- i + 1
  }


  method1 <- iterateMethods[1]
  method2 <- iterateMethods[2]
  method3 <- iterateMethods[3]
  method4 <- iterateMethods[4]
  method5 <- iterateMethods[5]
  predict1 <- iteratePredicts[1]
  predict2 <- iteratePredicts[2]
  predict3 <- iteratePredicts[3]
  predict4 <- iteratePredicts[4]
  predict5 <- iteratePredicts[5]
  methods[[i]] <- paste(c(method1,method2,method3,method4,method5),sep = ",")
  # direct combination
  combined.predict=(c(predict1[[1]]) + c(predict2[[1]]) + c(predict3[[1]]) + c(predict4[[1]]) + c(predict5[[1]]))/5
  predicts[[i]] <- combined.predict
  combined.err <- measures(as.matrix(prod_y_test$sales), combined.predict, as.matrix(prod_y_test$sales), benchmark = "naive")
  combined.errorFrame <- t(as.data.frame(combined.err))
  errors[[i]] <- combined.errorFrame
  combined.MaseError<-(as.data.frame(combined.errorFrame))$MASE[[1]]
  maseErrors <- c(maseErrors, combined.MaseError)
  combined.MaeError<-(as.data.frame(combined.errorFrame))$MAE[[1]]
  maeErrors <- c(maseErrors, combined.MaeError)
  #i <- i + 1

  # using mae
  #minMae <- min(maeErrors)
  #minIndex <- which.min(maeErrors)
  #minErr <- errors[[minIndex]]
  #minMethod <- methods[[minIndex]]

  #using mase
  minMase <- min(maseErrors)
  if (!is.finite(minMase)){
    # if mase does not work use mae
    minIndex <- which.min(maeErrors)
    minErr <- errors[[minIndex]]
    minMethod <- methods[[minIndex]]
  } else {
    minIndex <- which.min(maseErrors)
    minErr <- errors[[minIndex]]
    minMethod <- methods[[minIndex]]
  }
  #minIndex <- which.min(maseErrors)
  #minErr <- errors[[minIndex]]
  #minMethod <- methods[[minIndex]]


  ForecastAndID <- cbind(product_id = ux[prodIDIndex],
                         forecastingGroup = paste(minMethod, collapse=","),
                         minErr)

  if (!is.null(forecastingGroupsErrorsWhole)){
    forecastingGroupsErrorsWhole <- rbind(forecastingGroupsErrorsWhole, ForecastAndID)
  } else {
    forecastingGroupsErrorsWhole <- ForecastAndID
  }

}

writeCSV(forecastingGroupsErrorsWhole,numofSample)


