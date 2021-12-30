# Title     : TODO
# Objective : TODO
# Created by: metehangelgi
# Created on: 28.12.2021

#library(factoextra)
#library(tsintermittent)
#library(dplyr)
#library(readr)
#library(forecast)
#library(glmnet)

library(dplyr)
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

writeCSV <- function (final_data,numofSample,ABCtype,SBCtype,clustering){
  output00 <- c("forecast/new", numofSample,ABCtype,"_",SBCtype,clustering,".csv")
  output002 <- paste(output00, collapse="")
  write.csv(final_data,output002, row.names = FALSE)
}

forecast.dynamicReg <- function(elastic_coef,prod_x,prod_y,prod_x_train,prod_x_test,prod_y_train,prod_y_test){
  regressors <- which(elastic_coef!= 0) # if elastic net is desired

  rankTest<-is.rankdeficient(data.matrix(prod_x_train[,regressors], rownames.force = NA))
  print(rankTest)
  if (rankTest) return()

  dyano.fit <- auto.arima(prod_y_train[,"sales"],
                          xreg=data.matrix(prod_x_train[,regressors], rownames.force = NA))
  RegArimaErrors <- cbind("Regression Errors" = residuals(dyano.fit, type="regression"),
        "ARIMA errors" = residuals(dyano.fit, type="innovation"))
  #print(RegArimaErrors)
  checkresiduals(dyano.fit) #ne ise yarıyor?
  print("hata aaa")

  prodTest<-as.matrix(prod_x_test[,regressors])
  # hata burası
  #dyano.forecast <- dyano.fit %>% forecast(xreg = prodTest,h=40)
  #dyano.predict <- as.data.frame(dyano.forecast)[["Point Forecast"]]

  #dyno.erors <- accuracy(dyno.forecast, prod_y_test$sales)
  return()
}

forecast.ets <- function (prod_y_train,prod_y_test){

  ets.fit <- ets(as.vector(prod_y_train$sales))
  # istediğimiz gibi bir sonuç mu veriyor emin değilim.
  #print(predict(ets.fit,newdata=prod_y_test$sales)$fitted)
  #ets.forecast <- forecast::forecast(ets.fit,length(prod_y_test$sales))

  # forecast diyince hata veriyor çözemedim hatayı
  #ets.forecast <- ets.fit %>% forecast::forecast(h=length(prod_y_test$sales))
  #predict means veriyor diğeri de çalışmıyor maalesef, şimdilik hata almadığı için bunu kullaıyorum
  # print(predict(ets.fit,h=length(prod_y_test$sales),interval = "prediction"))
  ets.forecast <- predict(ets.fit,h=length(prod_y_test$sales),interval = "prediction")
  ets.predict <- as.data.frame(ets.forecast)[["Point Forecast"]]
  return(ets.predict)
}

forecast.crosten <- function (prod_y_train,prod_y_test){
  fitt <- crost(prod_y_train[["sales"]], h=40, w=NULL, type="sba", outplot = TRUE)
  predicted = as.data.frame(fitt$components$c.out)$Demand
  return(predicted)
}

doForecasting <- function(elastic_coefs,lasso_coefs,xdata,ydata,clusteredDataID,clusterID){
  forecastingGroups = list(c('dynamicReg'),c('crosten'),c('ets'),c('ets','dynamicReg'),
                           c('crosten','ets'),c('crosten','dynamicReg'),c('crosten','ets','dynamicReg'))

  prodIDs=xdata[['product_id']]
  ux <- unique(prodIDs)
  forecastingGroupsWhole=c()
  forecastingGroupsErrorsWhole=NULL

  for (prodIDIndex in 1:length(ux)) {

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

    bestForecastPairs=list("nothing",1/0,0)

    for (foreIndex in c(1:length(forecastingGroups))){
      forecastingGroup <- forecastingGroups[[foreIndex]]
      forecastOutputs=list()
      for (j in c(1:length(forecastingGroup))){
        forecastMethod <- forecastingGroup[[j]]
        dyno.output <- 0
        if (forecastMethod == "ets"){
          forecastOutputs[[j]]<-forecast.ets(prod_y_train,prod_y_test)
        } else if (forecastMethod == "crosten"){
          forecastOutputs[[j]]<-forecast.crosten(prod_y_train,prod_y_test)
        } else if (forecastMethod == "dynamicReg"){
          dyno.output<-forecast.dynamicReg(elastic_coef,prod_x,prod_y,prod_x_train,prod_x_test,prod_y_train,prod_y_test)
          forecastOutputs[[j]] <- dyno.output
        }
      }
      if (is.null(dyno.output)) next
      forecastOutputs2=Reduce(`+`, forecastOutputs)/length(forecastingGroup)
      err2 <- measures(as.matrix(prod_y_test$sales), forecastOutputs2, as.matrix(prod_y_test$sales), benchmark = "naive")
      errorFrame <- t(as.data.frame(err2))
      rownames(errorFrame) <- c(ux[prodIDIndex])
      MaseError<-(as.data.frame(errorFrame))$MASE[[1]]
      # err burdan mase çekilip compare edilecek
      if (MaseError<bestForecastPairs[[2]]){
        bestForecastPairs<-list(paste(forecastingGroup,collapse=","),MaseError,errorFrame)
      }
    }
    if (bestForecastPairs[[3]] == 0){
      #sometimes it does not give anything, for these situations give last forecast as optimal solution
      bestForecastPairs<-list(paste(forecastingGroup,collapse=","),MaseError,errorFrame)
    }
    forecastingGroupsWhole=c(forecastingGroupsWhole,bestForecastPairs[[1]])

    if (is.null(forecastingGroupsErrorsWhole)) {
      forecastingGroupsErrorsWhole<-bestForecastPairs[[3]]
    } else {
      forecastingGroupsErrorsWhole <- rbind(forecastingGroupsErrorsWhole, bestForecastPairs[[3]])
    }

  }
  ForecastsAndIDS <- cbind(forecastingGroupsErrorsWhole, forecastingGroup = forecastingGroupsWhole)
  #ForecastsAndIDS<-list(product_id=ux,Errors=forecastingGroupsErrorsWhole,forecastPairs=forecastingGroupsWhole)
  return (ForecastsAndIDS)
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
    xdataFiltered <- subset(xdata, product_id %in% clusteredDataIDs)
    ydataFiltered <- subset(ydata, product_id %in% clusteredDataIDs)
    output=doForecasting(elastic_coefs,lasso_coefs, xdataFiltered,ydataFiltered,0)
    writeCSV(output,numofSample,ABCtype,SBCtype,0)
  } else {
    for (clusterIndex in c(1:length(clusterNum))){
      #clusteredDataFiltered <- clusteredData %>% filter(cluster==clusterIndex)
      clusteredDataFiltered <- subset(clusteredData, cluster == clusterIndex)
      clusteredDataFilteredIDs <- clusteredDataFiltered[["product_id"]]
      xdataFiltered2 <- subset(xdata, product_id %in% clusteredDataFilteredIDs)
      ydataFiltered2 <- subset(ydata, product_id %in% clusteredDataFilteredIDs)

      output=doForecasting(elastic_coefs,lasso_coefs,xdataFiltered2,ydataFiltered2,clusterIndex)
      writeCSV(output,numofSample,ABCtype,SBCtype,clusterIndex)

    }
  }
}

