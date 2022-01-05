#This file defines an ensemble model
#It is heavily commented
#If "MODIFY" clause is observer, it is safe to modify

#Libraries
library(readr)#To read data from csv
# Training libraries
library(forecast)#Common forecasting functionality
library(caret) #Uniform modelling interface

require(gbm)
library(e1071)
library("dplyr")
library(glmnet) #For elastic net -> regularization algorithm
library(randomForest)
library(rpart)
# Miscellenaus
library(ggplot2)
#Set seed for reproducibility
set.seed(1)

# Read data here
# Change data path for x and y acordingly
x_path <- "dataX.csv" #MODIFY
y_path <- "dataY.csv" #MODIFY
  
  
ydata <- read_csv(y_path)
xdata<- read_csv(x_path)
df <- data.frame(cbind(ydata, xdata))


# FOR LOOP entry point, for simplicity I focused on single product
# However, since following code will depend on id, this can be easilty enveloped in a foor loop
id <- ydata$product_id[300]

df_prod <- df[df$product_id == id,] ## product y data
data_size <- nrow(df_prod)

# Partition data for training, blending and testing purposes
# Assuming that all data is of same size
train_size <- 120 #MODIFY
blend_size <- 50  #MODIFY
test <- 30        #MODIFY

train_size + blend_size + test == data_size #Ensure using all data

#Set to preferred value
testing_horizon <- 7 #MODIFY

# Some preprocessing, since they are not handled in initial preprocessing step
df_prod <- df_prod[,names(df_prod)[names(df_prod) != "product_id"]] #drop product_id

# Use to remove linear dependence
comboInfo <- findLinearCombos(df_prod)
df_prod <- df_prod[, -comboInfo$remove]

# Remove highly correlated features
# This part can be removed, if a feature selection algorithm is provided
# However since I dont implement such a thing act acordingly
#MODIFY
descrCor <-  cor(df_prod)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
df_prod <- df_prod[,-highlyCorDescr]



# Parition data
ensembleData <- df_prod[1:train_size,] # Will be used to train individual models
blenderData <- df_prod[(train_size+1):(train_size+blend_size),] # Will be used to train ensembling model
testingData <- df_prod[(train_size+blend_size+1):data_size,] # Will be used to test overall model performance

labelName <- "sales" # Name of the predictor variable
# Modify acordingly to capture irrelevant features such as product id
predictors <- names(ensembleData)[names(ensembleData) != c(labelName)] #MODIFY

# Define  training control, one for caret, another for non-caret usage for individual model training
number_of_splits <- 7 #MODİFY -> number of train,test pairs

myControl <- trainControl(method = "timeslice",
                           initialWindow = train_size - number_of_splits*testing_horizon,
                           horizon = testing_horizon,
                           skip = testing_horizon -1,
                           fixedWindow = FALSE)

idx <- createTimeSlices(1:train_size, initialWindow = train_size - number_of_splits*testing_horizon,
                        horizon = testing_horizon,
                        skip = testing_horizon -1,
                        fixedWindow = FALSE)
trainSlices <- idx[[1]]
testSlices <- idx[[2]]

# Additional controller to ensemble the models
#MODIFY acordingly
controller <- trainControl(method = "timeslice",
                           initialWindow = blend_size - 1,
                           horizon = 1,
                           skip = 0,
                           fixedWindow = FALSE)

test_model <- train(sales ~ .,data = blenderData, method='gbm', trControl=controller)
preds <- predict(object=test_model, testingData[,predictors])
#err <- measures(as.matrix(testingData$sales), test_model, as.matrix(testingData$sales), benchmark = "naive")

#This defines an ensamble model defined by method_list, and combined using ensembler_method
#List of methods avaliavle in caret to ensemble
model_list <- c("gbm", "rpart", "rf", "glm") #MODIFY
#Ensembling method, trained on blend data
ensembling_method <- "rf" #MODIFY

for(model in model_list){
  model_fit <- train(ensembleData[,predictors], ensembleData[,labelName], method=model, trControl=myControl)
  blenderData[model] <- predict(object=model_fit, blenderData[,predictors])
  testingData[model] <- predict(object=model_fit, testingData[,predictors])
}



#Train and include non-avaliable methods to data

# Elastic net
elastic.fit <- train(ensembleData[,predictors], ensembleData[,labelName], method="glmnet", trControl=myControl)
elastic.fit$finalModel$lambdaOpt
elastic_coef <- coef(elastic.fit$finalModel, s =  elastic.fit$finalModel$lambdaOpt)
regressors <- which(elastic_coef[-1]!= 0) # -1 to drop intercept term

#In addition lets add elastic net as a forecasting algorithm
model <- "elastic"
blenderData[model] <- predict(object=elastic.fit, blenderData[,predictors])
testingData[model] <- predict(object=elastic.fit, testingData[,predictors])

#In a similar form train models using ensembleData, and record forecasts on blenderData and testingData
# ARIMA
model = "ARIMA"
model_fit <- auto.arima(ensembleData[,labelName])
blenderData[model] <- predict(object=model_fit, blenderData[,predictors])
testingData[model] <- predict(object=model_fit, testingData[,predictors])


#checkresiduals(model_fit) If arima erro plot is desired  

# Dynamic regression
model = "Dynamic"

ncol(data.matrix(blenderData[,predictors][, regressors]))
ncol(ensembleData[,predictors][, regressors])

model_fit <- auto.arima(ensembleData[,labelName],
                        xreg=data.matrix(ensembleData[,predictors][, regressors]))
blenderData[model] <- predict(object=model_fit, data.matrix(blenderData[,predictors][, regressors]))
testingData[model] <- predict(object=model_fit, testingData[,predictors][, regressors])


#Train ensebling algorithm
predictors <- model_list
final_blender_model <- train(blenderData[,predictors], blenderData[,labelName], method=ensembling_method, trControl=controller)





#Test on testing data

# Mase function if necessary
# Mase function
computeMASE <- function(forecast,train,test,period){
  
  # forecast - forecasted values
  # train - data used for forecasting .. used to find scaling factor
  # test - actual data used for finding MASE.. same length as forecast
  # period - in case of seasonal data.. if not, use 1
  
  forecast <- as.vector(forecast)
  train <- as.vector(train)
  test <- as.vector(test)
  
  n <- length(train)
  scalingFactor <- sum(abs(train[(period+1):n] - train[1:(n-period)])) / (n-period)
  
  et <- abs(test-forecast)
  qt <- et/scalingFactor
  meanMASE <- mean(qt)
  return(meanMASE)
}


preds <- predict(object=final_blender_model, testingData[,predictors])
training_sales_data <- c(ensembleData$sales,blenderData$sales)
computeMASE(preds,training_sales_data,testingData$sales,test)



