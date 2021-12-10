# Title     : TODO
# Objective : TODO
# Created by: metehangelgi
# Created on: 10.12.2021

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
inputLasso<-c("featureSelection/new", numofSample,".csv")
inputLasso2 <- paste(inputLasso, collapse="")
inputElastic<-c("featureSelection/newElastic", numofSample,".csv")
inputElastic2 <- paste(inputElastic, collapse="")
ydata <- read_csv(inputy2)
xdata <- read_csv(inputx2)
lasso_coefs <- read_csv(inputLasso2)
elastic_coefs <- read_csv(inputElastic2)

ux <- unique(prodIDs)
for (prodIDIndex in 1:length(ux))
  {
  lasso_coef <- filter(lasso_coefs, product_id == lasso_coefs[prodIDIndex])[, -1] ## lasso
  elastic_coef <- filter(elastic_coef, product_id == elastic_coef[prodIDIndex])[, -1] ## elastic
  prod_x <- filter(xdata, product_id == ux[prodIDIndex])[, -1] ## product x data
  prod_y <- filter(ydata, product_id == ux[prodIDIndex])[, -1] ## product y data
  # df = cbind(prod_y, prod_x)
  prod_y_train <- head(prod_y, round(nrow(prod_y) * 0.8))
  h <- nrow(prod_y) - nrow(prod_y_train)
  prod_y_test <- tail(prod_y, h)

  prod_x_train <- head(prod_x, round(nrow(prod_x) * 0.8))
  h <- nrow(prod_x) - nrow(prod_x_train)
  prod_x_test <- tail(prod_x, h)

  #prod_feature <- lasso_coef[-1] ## list of elemnts to include
  rod_feature <- lasso_coef ## list of elemnts to include
  #regressors <- which(prod_feature!= 0) ## indices of non-zero elements

  #regressors <- which(elastic_coef[-1]!= 0) # if elastic net is desired
  regressors <- which(elastic_coef!= 0) # if elastic net is desired

  dyano.fit <- auto.arima(prod_y[,"sales"],
                          xreg=data.matrix(prod_x[,regressors], rownames.force = NA))

  cbind("Regression Errors" = residuals(dyano.fit, type="regression"),
        "ARIMA errors" = residuals(dyano.fit, type="innovation")) %>%
    autoplot(facets=TRUE)
  checkresiduals(dyano.fit)

  dyano.fit %>% forecast(xreg = as.matrix(prod_x_test[,regressors]),h=40) %>% autoplot(ylab = "Sales")
  dyno.forecast <- dyano.fit %>% forecast(xreg = as.matrix(prod_x_test[,regressors]),h=40)

  dyno.erors <- accuracy(dyno.forecast, prod_y_test$sales)

}