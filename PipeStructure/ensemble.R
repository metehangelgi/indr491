#This file defines an ensemble model
#It is heavily commented
#If "MODIFY" clause is observer, it is safe to modify



#Libraries
library(readr)#To read data from csv

# Training libraries
library(forecast)#Common forecasting functionality
library(caret) #Uniform modelling interface
require(xgboost)

library(kernlab)
library(gbm)
library(e1071)
library("dplyr")
library(glmnet) #For elastic net -> regularization algorithm
library(randomForest)
library(rpart)
library(tsintermittent)
library("greybox")

# Miscellenaus
library(ggplot2)

#for the paralelization issues
require(doParallel)
c1<- makePSOCKcluster(12)
registerDoParallel(c1)


#Set seed for reproducibility
set.seed(10)


# Read data here
args = commandArgs(trailingOnly=TRUE)
numofSample=as.character(args[1])
inputy <- c("featureCreation/new", numofSample,"Y.csv")
inputy2 <- paste(inputy, collapse="")
inputx <- c("featureCreation/new", numofSample,".csv")
inputx2 <- paste(inputx, collapse="")
ydata <- read_csv(inputy2)
xdata <- read_csv(inputx2)

inputElastic<-c("featureSelection/new", numofSample,".csv")
inputElastic2 <- paste(inputElastic, collapse="")
elastic_coefs <- read_csv(inputElastic2)

inputSBC<-c("dataCategorization/new", numofSample,"SBC.csv")
inputSBC2 <- paste(inputSBC, collapse="")
dataCategorizes <- read_csv(inputSBC2)

#drop categoricals
patterns <- c("brand_ID_.*","size_.*","gender_.*")
any_matching = Reduce(`|`, lapply(patterns, grepl, colnames(xdata)))
resultX = colnames(xdata)[! any_matching]
drop <- resultX
xdata<-xdata[,(names(xdata) %in% drop)]

#manupilation on the data
xdata <- subset(xdata, select = -c(product_id))

df <- cbind(ydata, xdata)


# FOR LOOP entry point, for simplicity I focused on single product
# However, since following code will depend on id, this can be easilty enveloped in a foor loop
ids <- unique(ydata$product_id)

#for writing results to the csv file
writeCSV <- function (final_data,numofSample){
  #ABCtype,SBCtype,clustering will be added later
  output <- c("forecast/new", numofSample,".csv")
  output2 <- paste(output, collapse="")
  write.csv(final_data,output2, row.names = FALSE)
}

#finding the indexes of the first n max elements
whichpart <- function(x, n) {
  nx <- length(x)
  p <- nx-n
  xp <- sort(x, partial=p)[p]
  which(x > xp)
}

is.rankdeficient <- function(xregg) {
  constant_columns <- apply(xregg, 2, is.constant)
  if (any(constant_columns)) {
    xregg <- xregg[, -which(constant_columns)[1]]
  }
  sv <- svd(na.omit(cbind(rep(1, NROW(xregg)), xregg)))$d
  min(sv)/sum(sv) < .Machine$double.eps
}



results <- NULL
for(id in c(1:length(ids))){ # should use this one(stuck in infinity loop?)
# for(id in 1:3){ # worked sample for me
  print(id)
  df_prod <- df[df$product_id == ids[id],] ## product y data
  df_prod <- subset(df_prod, select = -c(product_id))
  elastic_coef <- filter(elastic_coefs, product_id == ids[id])[, -1]
  regressors <- which((elastic_coef)[-1]!= 0)

  df_prod<-cbind(subset(df_prod, select = c(sales)), (df_prod[-1])[,regressors])

  #categorize of prod
  dataCategorizes.selected <- dataCategorizes[dataCategorizes$product_id == ids[id],] ##
  categorizationName <- dataCategorizes.selected$demand_cate[1]

  # Partition data for training, blending and testing purposes
  # Assuming that all data is of same size
  train_size <- 150 #MODIFY
  blend_size <- 40  #MODIFY
  test <- 27        #MODIFY

  #train_size + blend_size + test == data_size #Ensure using all data
  #Set to preferred value
  testing_horizon <- 1 #MODIFY


  # Use to remove linear dependence
  comboInfo <- findLinearCombos(subset(df_prod, select = -c(sales)))
  df_prod <- df_prod[, -comboInfo$remove]

  # descrCor <-  cor(subset(df_prod, select = -c(sales)))
  # highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
  # df_prod <- cbind(subset(df_prod, select = c(sales)), subset(df_prod, select = -c(sales))[,-highlyCorDescr])


  # Parition data
  ensembleData <- df_prod[1:train_size,] # Will be used to train individual models
  blenderData <- df_prod[(train_size+1- blend_size):train_size ,] # Will be used to train ensembling model
  testingData <- df_prod[(train_size+1):177,] # Will be used to test overall model performance

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


  #This defines an ensamble model defined by method_list, and combined using ensembler_method
  #List of methods avaliavle in caret to ensemble
  #Ensembling method, trained on blend data

  usedMethods <- c("xgbTree", "gbm", "rf","svmRadial", "svmLinear", "svmPoly")
  ensembling_method <- "rf" #MODIFY


  for(model in usedMethods){
    print(model)
    model_fit <- train(ensembleData[,predictors], ensembleData[,labelName], method=model, trControl=myControl, verbose = FALSE, metric = "MAE")
    #ensembleData[model] <- predict(object=model_fit, ensembleData[,predictors])
    blenderData[model] <- predict(object=model_fit, blenderData[,predictors])
    testingData[model] <- predict(object=model_fit, testingData[,predictors])
  }



  #Train and include non-avaliable methods to data

  # Elastic net
  elastic.fit <- train(ensembleData[,predictors], ensembleData[,labelName], method="glmnet", trControl=myControl)
  elastic_coef <- coef(elastic.fit$finalModel, s =  elastic.fit$finalModel$lambdaOpt)


  #In addition lets add elastic net as a forecasting algorithm
  model <- "glmnet"
  usedMethods <- c(usedMethods, model)
  #ensembleData[model] <- predict(object=model_fit, ensembleData[,predictors])
  blenderData[model] <- predict(object=elastic.fit, blenderData[,predictors])
  testingData[model] <- predict(object=elastic.fit, testingData[,predictors])


  #ETS
  model = "ETS"
  usedMethods <- c(usedMethods, model)
  model_fit <- ets(as.vector(ensembleData[,labelName]))
  blenderData[model] <- as.data.frame(forecast(object=model_fit, h=blend_size))[["Point Forecast"]]
  testingData[model] <- as.data.frame(forecast(object=model_fit, h=test))[["Point Forecast"]]

  #Naive forecast
  model <- "naive"
  usedMethods <- c(usedMethods, model)
  blenderData[model] <- c(ensembleData[nrow(ensembleData),labelName],blenderData[-nrow(blenderData),labelName])
  testingData[model] <- c(blenderData[nrow(blenderData),labelName],testingData[-nrow(testingData),labelName])

  #Croston's
  model = "Croston"
  usedMethods <- c(usedMethods, model)
  blenderData[model] <- as.data.frame(crost(ensembleData[,labelName], h=blend_size,
                                            w=NULL, cost='mae', init.opt = TRUE)$components$c.out)$Demand
  testingData[model] <- as.data.frame(crost(ensembleData[,labelName], h=test,
                                            w=NULL, cost='mae', init.opt = TRUE)$components$c.out)$Demand

  #SBA
  model = "SBA"
  usedMethods <- c(usedMethods, model)
  blenderData[model] <- as.data.frame(crost(ensembleData[,labelName], h=blend_size, type = "sba",
                                            w=NULL, cost='mae', init.opt = TRUE)$components$c.out)$Demand
  testingData[model] <- as.data.frame(crost(ensembleData[,labelName], h=test, type = "sba",
                                            w=NULL, cost='mae', init.opt = TRUE)$components$c.out)$Demand

  #TSB
  model = "TSB"
  usedMethods <- c(usedMethods, model)
  blenderData[model] <- tsb(ensembleData[,labelName], h=blend_size, init.opt = TRUE)$frc.out
  testingData[model] <- tsb(ensembleData[,labelName], h=test, init.opt = TRUE)$frc.out

  if(!is.rankdeficient(ensembleData[,predictors])){
    #Dynamic regression
    model = "Dynamic" # MODIFY
    usedMethods <- c(usedMethods, model)

    ncol(data.matrix(blenderData[,predictors]))
    ncol(ensembleData[,predictors])

    model_fit <- auto.arima(ensembleData[,labelName],
                            xreg = data.matrix(ensembleData[,predictors])) #MODIFY
    blenderData[model] <- predict(object=model_fit, newxreg = data.matrix(blenderData[,predictors]))$pred
    testingData[model] <- predict(object=model_fit, newxreg = data.matrix(testingData[,predictors]))$pred
  }

  mases <- list()
  for(mod in usedMethods){
    ms <- measures(testingData$sales, testingData[[mod]], testingData$sales, benchmark = "naive")
    mases <- c(mases, as.data.frame(ms)$ms[9])
  }

  #Train ensebling algorithm
  final_blender_model <- train(blenderData[,usedMethods], blenderData[,labelName], method=ensembling_method, trControl=controller)
  #ml_only_blend <- train(blenderData[,ml_usedMethods], blenderData[,labelName], method=ensembling_method, trControl=controller)


  preds <- predict(object=final_blender_model, newdata = testingData[,usedMethods])
  ms <- measures(testingData$sales, preds, testingData$sales, benchmark = "naive")
  usedMethods <- c(usedMethods, "ensembled")
  mases <- c(mases, as.data.frame(ms)$ms[9])

  purity <- as.data.frame(final_blender_model$finalModel$importance)
  best_ones <- whichpart(purity$IncNodePurity, 4)
  names_of_bests <- rownames(purity)[best_ones]

  best <- usedMethods[which.min(mases)]

  if(best == "ensembled"){
    ForecastAndID <- cbind(product_id = ids[id],
                           categorization = categorizationName,
                           forecastingType = paste(names_of_bests, collapse = ","),
                           t(as.data.frame(measures(testingData$sales, preds, testingData$sales, benchmark = "naive"))))
  }else{
    ForecastAndID <- cbind(product_id = ids[id],
                           categorization = categorizationName,
                           forecastingType = paste(best),
                           t(as.data.frame(measures(testingData$sales, testingData[[best]], testingData$sales, benchmark = "naive"))))
  }


  if (!is.null(results)){
    results <- rbind(results, ForecastAndID)
  } else {
    results <- ForecastAndID
  }

}

stopCluster(c1)

writeCSV(results, numofSample)

