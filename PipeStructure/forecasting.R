# Title     : TODO
# Objective : TODO
# Created by: metehangelgi
# Created on: 28.12.2021

library(factoextra)
library(tsintermittent)
library("dplyr")
library(readr)
library(forecast)
library(glmnet)

is.rankdeficient <- function(xregg) {
  constant_columns <- apply(xregg, 2, is.constant)
  if (any(constant_columns)) {
    xregg <- xregg[, -which(constant_columns)[1]]
  }
  sv <- svd(na.omit(cbind(rep(1, NROW(xregg)), xregg)))$d
  min(sv)/sum(sv) < .Machine$double.eps
}

writeCSV <- function (final_data,numofSample,ABCtype,SBCtype,clustering){
  output00 <- c("forecasting/new", numofSample,ABCtype,"_",SBCtype,clustering,".csv")
  output002 <- paste(output00, collapse="")
  write.csv(final_data,output002, row.names = FALSE)
}

forecast.dynamicReg <- function(elastic_coef,prod_x,prod_y,prod_x_test,prod_y_test){

  regressors <- which(elastic_coef!= 0) # if elastic net is desired

  #rank deficient sıkıntısı var çözemedim, şimdilik bu şekilde handle ettim
  rankTest=is.rankdeficient(data.matrix(prod_x[,regressors], rownames.force = NA))
  #print(rankTest)
  if (rankTest) return()
  print('hata2')

  dyano.fit <- auto.arima(prod_y[,"sales"],
                          xreg=data.matrix(prod_x[,regressors], rownames.force = NA))
  print('hata3')
  cbind("Regression Errors" = residuals(dyano.fit, type="regression"),
        "ARIMA errors" = residuals(dyano.fit, type="innovation")) %>%
    autoplot(facets=TRUE)
  print('hata4')
  checkresiduals(dyano.fit)
  print('hata5')
  dyano.fit %>% forecast(xreg = as.matrix(prod_x_test[,regressors]),h=40) %>% autoplot(ylab = "Sales")
  print('hata6')
  dyno.forecast <- dyano.fit %>% forecast(xreg = as.matrix(prod_x_test[,regressors]),h=40)
  print('hat73')
  dynamic.forecast <- as.data.frame(dyno.forecast)
  print(dynamic.forecast)
}

forecast.ets <- function (prod_y_train,prod_y_test){

  ets.fit<- ets(as.vector(prod_y_train$sales))
  # hata var çözülmeli
  ets.forecast <- as.data.frame(forecast(ets.fit,length(prod_y_test$sales)))
  print(as.matrix(prod_y_test$sales))
  #print(ets.forecast)
  #print(ets.errors)

  #ets.forecast <- ets(as.vector(prod_y_train$sales))
  #ets.errors <- accuracy(ets.forecast, as.matrix(prod_y_test$sales))

  return(ets.forecast)
}

forecast.crosten <- function (prod_y_train,prod_y_test){
  fitt <- crost(prod_y_train[["sales"]], h=40, w=NULL, type="sba", outplot = TRUE)
  predicted = as.data.frame(fitt$components$c.out)$Demand

  err2 <- measures(as.matrix(prod_y_test$sales), predicted, as.matrix(prod_y_test$sales), benchmark = "naive")
  print(predicted)
}

doForecasting <- function(xdata,ydata,clusteredDataID,clusterID){
  forecastingGroups = list(c('ets'),c('dynamicReg'),c('crosten'),c('dynamicReg','ets'),
                           c('ets','crosten'),c('dynamicReg','crosten'),c('dynamicReg','ets','crosten'))

  prodIDs=xdata[['product_id']]
  ux <- unique(prodIDs)
  forecastErrors=list()
  for (prodIDIndex in 1:length(ux)) {
    print(prodIDIndex)
    print(ux[prodIDIndex])

    lasso_coef <- filter(lasso_coefs, product_id == ux[prodIDIndex])[, -1] ## lasso
    elastic_coef <- filter(elastic_coefs, product_id == ux[prodIDIndex])[, -1] ## elastic
    prod_x <- filter(xdata, product_id == ux[prodIDIndex])[, -1] ## product x data
    prod_y <- filter(ydata, product_id == ux[prodIDIndex])[, -1] ## product y data
    df = cbind(prod_y, prod_x)

    prod_y_train <- head(prod_y, round(nrow(prod_y) * 0.8))
    h <- nrow(prod_y) - nrow(prod_y_train)
    prod_y_test <- tail(prod_y, h)

    prod_x_train <- head(prod_x, round(nrow(prod_x) * 0.8))
    h <- nrow(prod_x) - nrow(prod_x_train)
    prod_x_test <- tail(prod_x, h)

    bestForecastPairs=list("nothing",100000)
    for (foreIndex in c(1:length(forecastingGroups))){
      forecastingGroup <- forecastingGroups[[foreIndex]]
      forecastOutputs=list()
      for (j in c(1:length(forecastingGroup))){
        forecastMethod <- forecastingGroup[[j]]
        if (forecastMethod == "ets"){
          forecastOutputs[[j]]<-forecast.ets(prod_y_train,prod_y_test)
        } else if (forecastMethod == "crosten"){
          forecastOutputs[[j]]<-forecast.crosten(prod_y_train,prod_y_test)
        } else if (forecastMethod == "dynamicReg"){
          forecastOutputs[[j]]<-forecast.dynamicReg(elastic_coef,prod_x,prod_y,prod_x_test,prod_y_test)
        }
      }
      forecastOutputs=Reduce(`+`, myListOfVector)/length(forecastingGroup)
      err2 <- measures(as.matrix(prod_y_test$sales), predicted, as.matrix(prod_y_test$sales), benchmark = "naive")
      # err burdan mase çekilip compare edilecek

      if (err2<bestForecastPairs[[2]]){
        bestForecastPairs<-list(forecastingGroup,err2)
      }
    }
    #forecast.crosten(prod_y_train,prod_y_test)
    #forecast.dynamicReg(elastic_coef,prod_x,prod_y,prod_x_test,prod_y_test)
    forecastErrors<-c(forecastErrors,bestForecastPairs)
  }
  ForecastsAndIDS<-list(product_id=ux,MaseError=forecastErrors[[1]],forecastPairs=forecastErrors[[2]])
  return (as.data.frame(ForecastsAndIDS))
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

# exclude brandid,gender,size
patterns <- c("brand_ID_.*","size_.*","gender_.*")
any_matching = Reduce(`|`, lapply(patterns, grepl, colnames(xdata)))
resultX = colnames(xdata)[! any_matching]
drop <- resultX
xdata=xdata[,(names(xdata) %in% drop)]

categorized_dataCategories <- list(c("A","Smooth"),c("A","Lumpy"),c("A","Intermittent"),c("A","Erratic"),
                                   c("B","Smooth"),c("B","Lumpy"),c("B","Intermittent"),c("B","Erratic"),
                                   c("C","Smooth"),c("C","Lumpy"),c("C","Intermittent"),c("C","Erratic"))

for (i in c(1:length(categorized_dataCategories))){
  ABCtype<-categorized_dataCategories[[i]][[1]]
  SBCtype<-categorized_dataCategories[[i]][[2]]
  inputCategorize<-c("clustering/new", numofSample,ABCtype,"_",SBCtype,".csv")
  inputCategorize2 <- paste(inputCategorize, collapse="")
  clusteredData <- read_csv(inputCategorize2)
  clusteredDataIDs<- clusteredData[["product_id"]]
  if (identical(clusteredData[[1]], character(0))) next

  clusterNum=max(clusteredData$cluster)

  if (clusterNum==0){
    for (clusteredDataID in clusteredDataIDs){
      xdataFiltered <- subset(xdata, product_id %in% clusteredDataIDs)
      ydataFiltered <- subset(ydata, product_id %in% clusteredDataIDs)
      output=doForecasting(xdataFiltered,ydataFiltered,clusteredDataID,0)
      writeCSV(output,numofSample,ABCtype,SBCtype,0)
    }
  } else {
    for (clusterIndex in c(1:length(clusterNum))){
      #clusteredDataFiltered <- clusteredData %>% filter(cluster==clusterIndex)
      clusteredDataFiltered <- subset(clusteredData, cluster == clusterIndex)
      clusteredDataFilteredIDs<- clusteredDataFiltered[["product_id"]]
      xdataFiltered2 <- subset(xdata, product_id %in% clusteredDataFilteredIDs)
      ydataFiltered2 <- subset(ydata, product_id %in% clusteredDataFilteredIDs)
      for (clusteredDataFilteredID in clusteredDataFilteredIDs){
        output=doForecasting(xdataFiltered2,ydataFiltered2,clusteredDataFilteredID,clusterIndex)
        writeCSV(output,numofSample,ABCtype,SBCtype,clusterIndex)
      }
    }
  }
}

