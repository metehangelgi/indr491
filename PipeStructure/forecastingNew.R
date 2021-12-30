# Title     : TODO
# Objective : TODO
# Created by: metehangelgi
# Created on: 10.12.2021

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
inputLasso<-c("featureSelection/new", numofSample,".csv")
inputLasso2 <- paste(inputLasso, collapse="")
inputElastic<-c("featureSelection/newElastic", numofSample,".csv")
inputElastic2 <- paste(inputElastic, collapse="")
ydata <- read_csv(inputy2)
xdata <- read_csv(inputx2)
lasso_coefs <- read_csv(inputLasso2)
elastic_coefs <- read_csv(inputElastic2)

clusteredInput <- c("clustering/new", numofSample,".csv")
clusteredInput2 <- paste(clusteredInput, collapse="")
clusteredData <- read_csv(clusteredInput2)


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

  lasso_coef <- filter(lasso_coefs, product_id == ux[prodIDIndex])[, -1] ## lasso
  elastic_coef <- filter(elastic_coefs, product_id == ux[prodIDIndex])[, -1] ## elastic
  prod_x <- filter(xdata, product_id == ux[prodIDIndex])[, -1] ## product x data
  prod_y <- filter(ydata, product_id == ux[prodIDIndex])[, -1] ## product y data
  clusteredDataGivenID <- filter(clusteredData, product_id == ux[prodIDIndex])
  # df = cbind(prod_y, prod_x)
  prod_y_train <- head(prod_y, round(nrow(prod_y) * 0.8))
  h <- nrow(prod_y) - nrow(prod_y_train)
  prod_y_test <- tail(prod_y, h)

  prod_x_train <- head(prod_x, round(nrow(prod_x) * 0.8))
  h <- nrow(prod_x) - nrow(prod_x_train)
  prod_x_test <- tail(prod_x, h)

  methods <- list()
  maseError <- vector()
  errors <- list()
  i <- 1
  regressors <- which(elastic_coef!= 0) # if elastic net is desired
  #rank deficient sıkıntısı var çözemedim, şimdilik bu şekilde handle ettim
  rankTest=is.rankdeficient(data.matrix(prod_x[,regressors], rownames.force = NA))

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
      dyno.forecast <- dyano.fit %>% forecast::forecast(xreg = as.matrix(prod_x_test[,regressors]),h=40)
      dyno.predict <-as.data.frame(dyno.forecast)[["Point Forecast"]]
      print("1")
      dyno.err <- measures(as.matrix(prod_y_test$sales), dyno.predict, as.matrix(prod_y_test$sales), benchmark = "naive")
      dyno.errorFrame <- t(as.data.frame(dyno.err))
      errors[[i]] <- dyno.errorFrame
      dyno.MaseError<-(as.data.frame(dyno.errorFrame))$MASE[[1]]
      maseError <- c(maseError,dyno.MaseError)
      methods[[i]] <- "dyno"
      i <- i + 1
      #dyno.predict <- accuracy(dyno.forecast, prod_y_test$sales)
    }
  }

  #ets
  methods[[i]] <- "ets"
  ets.fit<- ets(as.vector(prod_y_train$sales))
  ets.forecast <- forecast(ets.fit,length(prod_y_test$sales))
  ets.predict<-as.data.frame(ets.forecast)[["Point Forecast"]]
  print("2")
  ets.err <- measures(as.matrix(prod_y_test$sales), ets.predict, as.matrix(prod_y_test$sales), benchmark = "naive")
  ets.errorFrame <- t(as.data.frame(ets.err))
  errors[[i]] <- ets.errorFrame
  ets.MaseError<-(as.data.frame(ets.errorFrame))$MASE[[1]]
  maseError <- c(maseError,ets.MaseError)
  i <- i + 1

  #crosten
  methods[[i]] <- "crost"
  crost.fit <- crost(prod_y_train[["sales"]], h=40, w=NULL, type="sba", outplot = TRUE)
  crost.predict = as.data.frame(crost.fit$components$c.out)$Demand
  print("3")
  crost.err <- measures(as.matrix(prod_y_test$sales), crost.predict, as.matrix(prod_y_test$sales), benchmark = "naive")
  crost.errorFrame <- t(as.data.frame(crost.err))
  errors[[i]] <- crost.errorFrame
  crost.MaseError<-(as.data.frame(crost.errorFrame))$MASE[[1]]
  maseError <- c(maseError,crost.MaseError)
  i <- i + 1

  # dyno - ets
  if (!(rankTest)){
    methods[[i]] <-  "dyno,ets"
    forecastOutputs <- list()
    forecastOutputs[[1]] <- dyno.predict
    forecastOutputs[[2]] <- ets.predict
    dyno.ets.combined=Reduce(`+`, forecastOutputs)/length(2)
    print("4")
    dyno.ets.err <- measures(as.matrix(prod_y_test$sales), dyno.ets.combined, as.matrix(prod_y_test$sales), benchmark = "naive")
    dyno.ets.errorFrame <- t(as.data.frame(dyno.ets.err))
    errors[[i]] <- dyno.ets.errorFrame
    dyno.ets.MaseError<-(as.data.frame(dyno.ets.errorFrame))$MASE[[1]]
    maseError <- c(maseError,dyno.ets.MaseError)
    i <- i + 1
  }


  # dyno - crost
  if (!(rankTest)){
    methods[[i]] <-  "dyno,crost"
    forecastOutputs <- list()
    forecastOutputs[[1]] <- dyno.predict
    forecastOutputs[[2]] <- crost.predict
    dyno.crost.combined=Reduce(`+`, forecastOutputs)/length(2)
    print("5")
    dyno.crost.err <- measures(as.matrix(prod_y_test$sales), dyno.crost.combined, as.matrix(prod_y_test$sales), benchmark = "naive")
    dyno.crost.errorFrame <- t(as.data.frame(dyno.crost.err))
    errors[[i]] <- dyno.crost.errorFrame
    dyno.crost.MaseError<-(as.data.frame(dyno.crost.errorFrame))$MASE[[1]]
    maseError <- c(maseError,dyno.crost.MaseError)
    i <- i + 1
  }


  # ets - crost
  methods[[i]] <-  "ets,crost"
  forecastOutputs <- list()
  forecastOutputs[[1]] <- ets.predict
  forecastOutputs[[2]] <- crost.predict
  ets.crost.combined=Reduce(`+`, forecastOutputs)/length(2)
  print("6")
  ets.crost.err <- measures(as.matrix(prod_y_test$sales), ets.crost.combined, as.matrix(prod_y_test$sales), benchmark = "naive")
  ets.crost.errorFrame <- t(as.data.frame(ets.crost.err))
  errors[[i]] <- ets.crost.errorFrame
  ets.crost.MaseError<-(as.data.frame(ets.crost.errorFrame))$MASE[[1]]
  maseError <- c(maseError,ets.crost.MaseError)
  i <- i + 1

  # dyno - ets - crost
  if (!(rankTest)){
    methods[[i]] <-  "dyno,ets,crost"
    forecastOutputs <- list()
    forecastOutputs[[1]] <- ets.predict
    forecastOutputs[[2]] <- crost.predict
    dyno.ets.crost.combined=Reduce(`+`, forecastOutputs)/length(2)
    print("7")
    dyno.ets.crost.err <- measures(as.matrix(prod_y_test$sales), dyno.ets.crost.combined, as.matrix(prod_y_test$sales), benchmark = "naive")
    dyno.ets.crost.errorFrame <- t(as.data.frame(dyno.ets.crost.err))
    errors[[i]] <- dyno.ets.crost.errorFrame
    dyno.ets.crost.MaseError<-(as.data.frame(dyno.ets.crost.errorFrame))$MASE[[1]]
    maseError <- c(maseError,dyno.ets.crost.MaseError)
    i <- i + 1
  }

  minMase <- min(maseError)
  minIndex <- which.min(maseError)
  minErr <- errors[[minIndex]]
  minMethod <- methods[[minIndex]]
  ForecastAndID <- cbind(product_id = ux[prodIDIndex],cluster=clusteredDataGivenID$cluster[[1]],
                            ABCtype=clusteredDataGivenID$ABCtype[[1]],SBCtype=clusteredDataGivenID$SBCtype[[1]],
                            forecastingGroup = minMethod,minErr)
  #print(ForecastAndID)
  #ForecastAndID <- cbind(ForecastAndIDPre,forecastingGroup = minMethod)
  if (!is.null(forecastingGroupsErrorsWhole)){
    forecastingGroupsErrorsWhole <- rbind(forecastingGroupsErrorsWhole, ForecastAndID)
  } else {
    forecastingGroupsErrorsWhole <- ForecastAndID
  }

}

writeCSV(forecastingGroupsErrorsWhole,numofSample)


