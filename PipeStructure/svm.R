library(tsintermittent)
library(readr)
library(dplyr)
library(e1071)

args = commandArgs(trailingOnly=TRUE)
numofSample=as.character(args[1])
inputy <- c("featureCreation/new100Y.csv")
inputy2 <- paste(inputy, collapse="")
inputx <- c("featureCreation/new100.csv")
inputx2 <- paste(inputx, collapse="")
inputLasso<-c("featureSelection/new100.csv")
inputLasso2 <- paste(inputLasso, collapse="")
inputElastic<-c("featureSelection/newElastic100.csv")
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

prodIDs=xdata[['product_id']]
ux <- unique(prodIDs)

for (prodIDIndex in 1:length(ux))
{
  print(prodIDIndex)
  #lasso_coef <- filter(lasso_coefs, product_id == ux[prodIDIndex])[, -1] ## lasso
  elastic_coef <- filter(elastic_coefs, product_id == ux[4])[, -1] ## elastic
  prod_x <- filter(xdata, product_id == ux[4])[, -1] ## product x data
  prod_y <- filter(ydata, product_id == ux[4])[, -1] ## product y data
  # df = cbind(prod_y, prod_x)
  prod_y_train <- head(prod_y, round(nrow(prod_y) * 0.8))
  h <- nrow(prod_y) - nrow(prod_y_train)
  prod_y_test <- tail(prod_y, h)
  
  prod_x_train <- head(prod_x, round(nrow(prod_x) * 0.8))
  h <- nrow(prod_x) - nrow(prod_x_train)
  prod_x_test <- tail(prod_x, h)
  #prod_feature <- lasso_coef[-1] ## list of elemnts to include
  #rod_feature <- lasso_coef ## list of elemnts to include
  #regressors <- which(prod_feature!= 0) ## indices of non-zero elements
  #regressors <- which(elastic_coef[-1]!= 0) # if elastic net is desired
  
  regressors <- which(elastic_coef!= 0) # if elastic net is desired
  
  new_data_train <- cbind(prod_y_train, prod_x_train[, regressors])
  new_data_test <- cbind(prod_y_test, prod_x_test[, regressors])
  
  tuneResult <- tune(svm, sales ~ .,  data = new_data_train,
                        ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
  )
  print(tuneResult)
  # Draw the tuning graph
  #plot(tuneResult)
  
  prediction = predict(tuneResult$best.model, new_data_test)
  
  err2 <- measures(as.matrix(prod_y_test$sales), prediction, as.matrix(prod_y$sales), benchmark = "naive")
  
  print(err2)
  
  
}