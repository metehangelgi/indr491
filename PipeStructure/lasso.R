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

   #glmfit <- cv.glmnet(as.matrix(prod_x_train), y=prod_y_train$sales,
   #                    type.measure = "mse",family= "gaussian", alpha=1)
   #glmfit.prediction <-predict(glmfit, s = glmfit$lambda.1se, newx = as.matrix(prod_x_test))
   
   #glmfit.rmse <- mean((glmfit.prediction - as.matrix(prod_y_test))^2)

   #plot(glmfit)

   list.of.fits <- list()
   for (i in 0:10) {
      fit.name <- paste0("alpha", i/10)

      # try catch
      ElasticError <- tryCatch(
      cv.glmnet(as.matrix(prod_x_train), y=prod_y_train$sales,
                type.measure = "mse",family= "gaussian", alpha=i/10))
      if(inherits(ElasticError, "error")){
         print(prod_y_train$sales)
      }
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

   elastic_coef <-  coef(list.of.fits[["alpha0.7"]],s = list.of.fits[["alpha0.9"]]$lambda.min)
   #lasso_coef <- coef(glmfit, s = glmfit$lambda.1se)
   #print(lasso_coef) # lasso outputları güzel değil
   #print(elastic_coef) # elasticte bişiler var

   # Bana bir ürün için LassoColumnValues olan bi vector döndür
   #numCols <- ncol(prod_x_train)
   #LassoColumnValues <- rep(NA, numCols) # NA yerine lasso feature değerleri gibi
   #newList <- list("Lasso" = lasso_coef, "Elastic" = elastic_coef)
   return(elastic_coef)
}
FeatureSelection <- function(xdata,ydata,prodIDs) {
   #mat<-matrix(list(), nrow=length(prodIDs), ncol=ncol(xdata))
   ux <- unique(prodIDs)
   mat <- matrix(0, length(ux), ncol(xdata)-1)
   matElastic <- matrix(0, length(ux), ncol(xdata)-1)
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

      #newListOutputs<-Lasso(prod_y_train,prod_y_test,prod_x_train,prod_x_test)

      #lassoProd <-newListOutputs$Lasso
      #lassoProd <-lassoProd[-1] #no idea first column?
      #elasticProd<-newListOutputs$Elastic
      #print(prod_y_train$sales)
      elasticProd<-Lasso(prod_y_train,prod_y_test,prod_x_train,prod_x_test)
      elasticProd<-elasticProd[-1] #no idea first column?

      #for (i in 1:(ncol(xdata)-1)){
      #   if (lassoProd[i]!=0){
      #      lassoProd[i]<-1
      #   }
      #   mat[[prodIDIndex,i]]<-lassoProd[i]
      #}

      for (i in 1:(ncol(xdata)-1)){
         if (elasticProd[i]!=0){
            elasticProd[i]<-1
         }
         matElastic[[prodIDIndex,i]]<-elasticProd[i]
      }

   }

   #frame <- as.data.frame(mat)
   #colnames(frame) <- colnames(xdata)[-1]
   #frame["product_id"]=ux
   #frame2 <- frame %>% select("product_id", everything())

   frameElastic <- as.data.frame(matElastic)
   colnames(frameElastic) <- colnames(xdata)[-1]
   frameElastic["product_id"]=ux
   frameElastic2 <- frameElastic %>% select("product_id", everything())
   #rownames(frame) <- ux
   #drop <- c("product_id")
   #frame = frame[,!(names(frame) %in% drop)]
   #newList <- list("Lasso" = frame2, "Elastic" = frameElastic2)
   return(frameElastic2)
}

# Read data
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
numofSample=as.character(args[1])
inputy <- c("featureCreation/new", numofSample,"Y.csv")
inputy2 <- paste(inputy, collapse="")
inputx <- c("featureCreation/new", numofSample,".csv")
inputx2 <- paste(inputx, collapse="")
ydata <- read_csv(inputy2)
xdata <- read_csv(inputx2)
#pid <- read_csv("featureCreation/new10PID.csv")
#xdata <- data.frame(xdataPre)
#ydata <- data.frame(ydataPre)

# exclude brandid,gender,size
patterns <- c("size_.*","gender_.*")
any_matching = Reduce(`|`, lapply(patterns, grepl, colnames(xdata)))
resultX = colnames(xdata)[! any_matching]
drop <- resultX
xdata=xdata[,(names(xdata) %in% drop)]
prodIDs=xdata[['product_id']]
#SelectionOutputs=FeatureSelection(xdata,ydata,prodIDs)
#LassoOutput<-SelectionOutputs$Lasso
#ElasticOutput<-SelectionOutputs$Elastic
ElasticOutput=FeatureSelection(xdata,ydata,prodIDs)

#output <- c("featureSelection/new", numofSample,".csv")
output0 <- c("featureSelection/newElastic", numofSample,".csv")
#output2 <- paste(output, collapse="")
output02 <- paste(output0, collapse="")
#write.csv(LassoOutput,output2, row.names = FALSE)
write.csv(ElasticOutput,output02, row.names = FALSE)





