# Title     : TODO
# Objective : TODO
# Created by: metehangelgi
# Created on: 29.12.2021

library("dplyr")
library(readr)
library(forecast)
library(glmnet)
library(factoextra)
library(cluster)
library(readr)
library(dplyr)
library(forecast)
library(glmnet)
library(tsintermittent)

args = commandArgs(trailingOnly=TRUE)
numofSample=as.character(args[1])
inputy <- c("featureCreation/new", numofSample,"Y.csv")
inputy2 <- paste(inputy, collapse="")
inputx <- c("featureCreation/new", numofSample,".csv")
inputx2 <- paste(inputx, collapse="")
ydata <- read_csv(inputy2)
xdata <- read_csv(inputx2)

prodIDs=xdata[['product_id']]
ux <- unique(prodIDs)

for (prodIDIndex in 1:length(ux))
  {

  prod_x <- filter(xdata, product_id == ux[prodIDIndex])[, -1] ## product x data
  prod_y <- filter(ydata, product_id == ux[prodIDIndex])[, -1] ## product y data
  df = cbind(prod_y, prod_x)

  prod_y_train <- head(prod_y, round(nrow(prod_y) * 0.8))
  h <- nrow(prod_y) - nrow(prod_y_train)
  prod_y_test <- tail(prod_y, h)

  prod_x_train <- head(prod_x, round(nrow(prod_x) * 0.8))
  h <- nrow(prod_x) - nrow(prod_x_train)
  prod_x_test <- tail(prod_x, h)

  fitt <- crost(prod_y_train[["sales"]], h=40, w=NULL, type="sba", outplot = TRUE)
  predicted = as.data.frame(fitt$components$c.out)$Demand
  err2 <- measures(as.matrix(prod_y_test$sales), predicted, as.matrix(prod_y_test$sales), benchmark = "naive")
  print(err2)
  print(predicted)
}