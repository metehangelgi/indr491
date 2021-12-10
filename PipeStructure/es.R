# Title     : TODO
# Objective : TODO
# Created by: metehangelgi
# Created on: 10.12.2021

#exponential Smoothing

library("dplyr")
library(readr)
library(forecast)
library(glmnet)

args = commandArgs(trailingOnly=TRUE)
numofSample=as.character(args[1])
inputy <- c("featureCreation/new", numofSample,"Y.csv")
inputy2 <- paste(inputy, collapse="")
inputx <- c("featureCreation/new", numofSample,".csv")
inputx2 <- paste(inputx, collapse="")
ydata <- read_csv(inputy2)
xdata <- read_csv(inputx2)

ux <- unique(prodIDs)
for (prodIDIndex in 1:length(ux))
  {

  prod_x <- filter(xdata, product_id == ux[prodIDIndex])[, -1] ## product x data
  prod_y <- filter(ydata, product_id == ux[prodIDIndex])[, -1] ## product y data
  # df = cbind(prod_y, prod_x)
  prod_y_train <- head(prod_y, round(nrow(prod_y) * 0.8))
  h <- nrow(prod_y) - nrow(prod_y_train)
  prod_y_test <- tail(prod_y, h)

  prod_x_train <- head(prod_x, round(nrow(prod_x) * 0.8))
  h <- nrow(prod_x) - nrow(prod_x_train)
  prod_x_test <- tail(prod_x, h)

  ses.forecast <- ses(as.vector(prod_y_train$sales),h=nrow(prod_y_test),damped=TRUE, seasonal="multiplicative")
  ses.erors <- accuracy(ses.forecast, prod_y_test$sales)
}