# Title     : TODO
# Objective : TODO
# Created by: metehangelgi
# Created on: 7.12.2021

# Libraries
library("dplyr")
library(readr)
library(forecast)
library(glmnet)

Lasso <- function (prod_y_train,prod_y_test,prod_x_train,prod_x_test){
   glmfit <- cv.glmnet(as.matrix(prod_x_train), y=prod_y_train$sales,
                       type.measure = "mse",family= "gaussian", alpha=1)
   glmfit.prediction <-predict(glmfit, s = glmfit$lambda.1se, newx = as.matrix(prod_x_test))


   glmfit.rmse <- mean((glmfit.prediction - as.matrix(prod_y_test))^2)

   #plot(glmfit)

   list.of.fits <- list()
   for (i in 0:10) {
      fit.name <- paste0("alpha", i/10)
      list.of.fits[[fit.name]] <-
      cv.glmnet(as.matrix(prod_x_train), y=prod_y_train$sales,
                type.measure = "mse",family= "gaussian", alpha=i/10)
   }

   results <- data.frame()
   for (i in 0:10) {
      fit.name <- paste0("alpha", i/10)

      ## Use each model to predict 'y' given the Testing dataset
      predicted <-
         predict(list.of.fits[[fit.name]],
                  s=list.of.fits[[fit.name]]$lambda.1se, newx=as.matrix(prod_x_test))

      ## Calculate the Mean Squared Error...
      mse <- mean((as.matrix(prod_y_test) - predicted)^2)

      ## Store the results
      temp <- data.frame(alpha=i/10, mse=mse, fit.name=fit.name)
      results <- rbind(results, temp)
   }

   #plot(list.of.fits[["alpha0.2"]])

   elastic_coef <-  coef(list.of.fits[["alpha0.2"]],s = list.of.fits[["alpha0.9"]]$lambda.min)
   lasso_coef <- coef(glmfit, s = glmfit$lambda.1se)

   # Bana bir ürün için LassoColumnValues olan bi vector döndür
   numCols <- ncol(prod_x_train)
   LassoColumnValues <- rep(NA, numCols) # NA yerine lasso feature değerleri gibi

   return(LassoColumnValues)
}
FeatureSelection <- function(xdata,ydata,prodIDs) {
   #mat<-matrix(list(), nrow=length(prodIDs), ncol=ncol(xdata))
   mat <- matrix(0, length(prodIDs), ncol(xdata))
   for (prodIDIndex in 1)
   {

      prod_x <- filter(xdata, product_id == prodIDs[prodIDIndex])[, -1] ## product x data
      prod_y <- filter(ydata, product_id == prodIDs[prodIDIndex])[, -1] ## product y data
      df = cbind(prod_y, prod_x)
      prod_y_train <- head(prod_y, round(nrow(prod_y) * 0.8))

      h <- nrow(prod_y) - nrow(prod_y_train)
      prod_y_test <- tail(prod_y, h)


      prod_x_train <- head(prod_x, round(nrow(prod_x) * 0.8))
      h <- nrow(prod_x) - nrow(prod_x_train)
      prod_x_test <- tail(prod_x, h)
      #lassoProd<-Lasso(prod_y_train,prod_y_test,prod_x_train,prod_x_test)
      lassoProd<-integer(ncol(xdata)) # bunu commentle üsttekini aç

      for (i in 1:ncol(xdata)){
         mat[[prodIDIndex,i]]<-lassoProd[i]
      }

   }
   frame <- as.data.frame(mat,columns=colnames(xdata))
   colnames(frame) <- colnames(xdata)
   #drop <- c("product_id")
   #frame = frame[,!(names(frame) %in% drop)]
   return(frame)
}

# Read data
ydata <- read_csv("featureCreation/new10Y.csv")
xdata <- read_csv("featureCreation/new10.csv")
#pid <- read_csv("featureCreation/new10PID.csv")
#xdata <- data.frame(xdataPre)
#ydata <- data.frame(ydataPre)
prodIDs=xdata[['product_id']]
LassoOutput=FeatureSelection(xdata,ydata,prodIDs)
write.csv(LassoOutput,"featureSelection/new10.csv", row.names = FALSE) # parametric yapamadım bakmak gerek




