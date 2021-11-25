# Libraries
library("dplyr")
library(readr)
library(forecast)
library(glmnet)

# Read data


ydata <- read_csv("A-LassoY.csv")
features <- read_csv("A-Lasso45.csv")
xdata<- read_csv("A-LassoX.csv") 



id = 999786225237277
#2014207854611875 iyi ürün from dynamic_regression
#1011252778676425
# 4570778245309593
#4570778245309593
prod_x <- filter(xdata, product_id == id)[, -1] ## product x data
prod_y <- filter(ydata, product_id == id)[, -1] ## product y data

df = cbind(prod_y, prod_x)


prod_y_train <- head(prod_y, round(nrow(prod_y) * 0.8))
h <- nrow(prod_y) - nrow(prod_y_train)
prod_y_test <- tail(prod_y, h)



prod_x_train <- head(prod_x, round(nrow(prod_x) * 0.8))
h <- nrow(prod_x) - nrow(prod_x_train)
prod_x_test <- tail(prod_x, h)
# autoplot(prod_y_train$sales) + ylab("Sales") + xlab("Day index")
# Arima

arima_fit <- auto.arima(prod_y_train)
checkresiduals(arima_fit) 
arima_fit %>% forecast(h=nrow(prod_y_test)) %>% autoplot(ylab = "Sales")
arima_forecast <- arima_fit %>% forecast(h=nrow(prod_y_test))

arima_erors <- accuracy(arima_forecast, prod_y_test$sales)



#Lasso regression

glmfit <- cv.glmnet(as.matrix(prod_x_train), y=prod_y_train$sales,
                    type.measure = "mse",family= "gaussian", alpha=1)
glmfit.prediction <-predict(glmfit, s = glmfit$lambda.1se, newx = as.matrix(prod_x_test))


glmfit.rmse <- mean((glmfit.prediction - as.matrix(prod_y_test))^2)

plot(glmfit)

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

plot(list.of.fits[["alpha0.2"]])

elastic_coef <-  coef(list.of.fits[["alpha0.2"]],s = list.of.fits[["alpha0.9"]]$lambda.min)
lasso_coef <- coef(glmfit, s = glmfit$lambda.1se)

#Dyamic regression

prod_feature <- lasso_coef[-1] ## list of elemnts to include
#regressors <- which(prod_feature!= 0) ## indices of non-zero elements

regressors <- which(elastic_coef[-1]!= 0) # if elastic net is desired

dyano.fit <- auto.arima(prod_y[,"sales"],
                  xreg=data.matrix(prod_x[,regressors], rownames.force = NA))


cbind("Regression Errors" = residuals(dyano.fit, type="regression"),
      "ARIMA errors" = residuals(dyano.fit, type="innovation")) %>%
  autoplot(facets=TRUE)

checkresiduals(dyano.fit)



dyano.fit %>% forecast(xreg = as.matrix(prod_x_test[,regressors]),h=40) %>% autoplot(ylab = "Sales")
dyno.forecast <- dyano.fit %>% forecast(xreg = as.matrix(prod_x_test[,regressors]),h=40)

dyno.erors <- accuracy(dyno.forecast, prod_y_test$sales)

# Simple exponential smothing

ses.forecast <- ses(as.vector(prod_y_train$sales),h=nrow(prod_y_test),damped=TRUE, seasonal="multiplicative")
ses.erors <- accuracy(ses.forecast, prod_y_test$sales)


# Holt's trend method

holt.forecast <- holt(as.vector(prod_y_train$sales),h=nrow(prod_y_test))
holt.errors <- accuracy(holt.forecast, prod_y_test$sales)

# ets

ets.forecast <- ets(as.vector(prod_y_train$sales))
ets.errors <- accuracy(ets.forecast, as.matrix(prod_y_test$sales))

summary(ets.forecast)

