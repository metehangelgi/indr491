#This file defines an ensemble model
#It is heavily commented
#If "MODIFY" clause is observer, it is safe to modify


libraries <- c("readr","forecast", "tsintermittent", "caret","kernlab","gbm","e1071","dplyr","glmnet","randomForest","rpart", "greybox", "ggplot2","doParallel")

lapply(libraries, require, character.only = TRUE)

# if (!require("readr", quietly = TRUE))
#   install.packages("readr")

#Libraries
# library(readr)#To read data from csv
# # Training libraries
# library("RSNNS")
# library(forecast)#Common forecasting functionality
# library(caret) #Uniform modelling interface
# 
# library(kernlab)
# library(gbm)
# library(e1071)
# library("dplyr")
# library(glmnet) #For elastic net -> regularization algorithm
# library(randomForest)
# library(rpart)
# library("greybox")
# 
# # Miscellenaus
# library(ggplot2)
#Set seed for reproducibility
set.seed(1)

# Read data here
# Change data path for x and y acordingly

inputy <- c("new100Y.csv")
inputy2 <- paste(inputy, collapse="")
inputx <- c("new100.csv")
inputx2 <- paste(inputx, collapse="")
inputElastic<-c("newElastic100.csv")
inputElastic2 <- paste(inputElastic, collapse="")
ydata <- read_csv(inputy2)
xdata <- read_csv(inputx2)
elastic_coefs <- read_csv(inputElastic2)

inputSBC<-c("new100SBC.csv")
inputSBC2 <- paste(inputSBC, collapse="")
dataCategorizes <- read_csv(inputSBC2)

inputPandemic <- c("usd_and_pandemic.csv")
inputPandemic2 <- paste(inputPandemic, collapse="")
pandemic <- read_csv(inputPandemic2)


c1<- makePSOCKcluster(12)
registerDoParallel(c1)
# FOR LOOP entry point, for simplicity I focused on single product
# However, since following code will depend on id, this can be easilty enveloped in a foor loop
patterns <- c("brand_ID_.","size_.","gender_.*")
any_matching = Reduce(`|`, lapply(patterns, grepl, colnames(xdata)))
resultX = colnames(xdata)[! any_matching]
drop <- resultX
xdata<-xdata[,(names(xdata) %in% drop)]

xdata <- subset(xdata, select = -c(product_id))

df <- cbind(ydata, xdata)

ids <- unique(ydata$product_id)

writeCSV <- function (final_data,numofSample){
  #ABCtype,SBCtype,clustering will be added later
  output00 <- c("newResults.csv")
  output002 <- paste(output00, collapse="")
  write.csv(final_data,output002, row.names = FALSE)
}

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

for(id in 2:length(ids)){
  print(id)
  df_prod <- df[df$product_id == ids[id],] ## product y data
  df_prod <- subset(df_prod, select = -c(product_id))
  elastic_coef <- filter(elastic_coefs, product_id == ids[id])[, -1]
  regressors <- which((elastic_coef)[-1]!= 0)
  
  df_prod<-cbind(subset(df_prod, select = c(sales)), (df_prod[-1])[,regressors])
  
  df_prod$`USD/TL` <- pandemic$`USD/TL`
  df_prod$Cases <- pandemic$Cases
    
  #categorize of prod
  dataCategorizes.selected <- dataCategorizes[dataCategorizes$product_id == ids[id],] ##
  categorizationName <- dataCategorizes.selected$demand_cate[1]
  
  # Partition data for training, blending and testing purposes
  # Assuming that all data is of same size
  train_size <- 150 #MODIFY
  blend_size <- 0  #MODIFY
  test <- 27        #MODIFY
  
  #train_size + blend_size + test == data_size #Ensure using all data
  
  #Set to preferred value
  testing_horizon <- 1 #MODIFY
  
  # Use to remove linear dependence
  comboInfo <- findLinearCombos(subset(df_prod, select = -c(sales)))
  df_prod <- df_prod[, -comboInfo$remove]
  
  # Remove highly correlated features
  # This part can be removed, if a feature selection algorithm is provided
  # However since I dont implement such a thing act acordingly
  #MODIFY
  # descrCor <-  cor(df_prod)
  # highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
  # df_prod <- df_prod[,-highlyCorDescr]
  
  
  # Parition data
  ensembleData <- df_prod[1:train_size,] # Will be used to train individual models
  testingData <- df_prod[(train_size+1):177,] # Will be used to test overall model performance
  
  labelName <- "sales" # Name of the predictor variable
  # Modify acordingly to capture irrelevant features such as product id
  predictors <- names(ensembleData)[names(ensembleData) != c(labelName)] #MODIFY
  
  
  # Define  training control, one for caret, another for non-caret usage for individual model training
  number_of_splits <- 7 #MODÄ°FY -> number of train,test pairs
  
  
  myControl <- trainControl(method = "timeslice",
                            initialWindow = train_size - number_of_splits*testing_horizon,
                            horizon = testing_horizon,
                            skip = testing_horizon -1,
                            fixedWindow = FALSE)
  
  
  
  #Ensembling method, trained on blend data
  usedMethods <- c("xgbTree", "gbm", "rf","svmRadial", "svmLinear", "svmPoly")
  #ensembling_method <- "rf" #MODIFY
  for(model in usedMethods){
    print(model)
    model_fit <- train(ensembleData[,predictors], ensembleData[,labelName], method=model, trControl=myControl, verbose = FALSE)
    #ensembleData[model] <- predict(object=model_fit, ensembleData[,predictors])
    #blenderData[model] <- predict(object=model_fit, blenderData[,predictors])
    testingData[model] <- predict(object=model_fit, testingData[,predictors])
  }
  
  #Train and include non-avaliable methods to data
  
  #Elastic net
  elastic.fit <- train(ensembleData[,predictors], ensembleData[,labelName], method="glmnet", trControl=myControl)
  elastic_coef <- coef(elastic.fit$finalModel, s =  elastic.fit$finalModel$lambdaOpt)
  regressors <- which(elastic_coef[-1]!= 0) # -1 to drop intercept term
  
  
  #In addition lets add elastic net as a forecasting algorithm
  model <- "glmnet"
  usedMethods <- c(usedMethods, model)
  #ensembleData[model] <- predict(object=model_fit, ensembleData[,predictors])
  #blenderData[model] <- predict(object=elastic.fit, blenderData[,predictors])
  testingData[model] <- predict(object=elastic.fit, testingData[,predictors])
  
  
  #ETS
  model = "ETS"
  usedMethods <- c(usedMethods, model)
  model_fit <- ets(as.vector(ensembleData[,labelName]))
  #blenderData[model] <- as.data.frame(forecast(object=model_fit, h=blend_size))[["Point Forecast"]]
  testingData[model] <- as.data.frame(forecast(object=model_fit, h=test))[["Point Forecast"]]
  
  #Naive forecast
  model <- "naive"
  usedMethods <- c(usedMethods, model)
  #blenderData[model] <- c(ensembleData[nrow(ensembleData),labelName],blenderData[-nrow(blenderData),labelName])
  testingData[model] <- c(ensembleData[nrow(ensembleData),labelName],testingData[-nrow(testingData),labelName])
  
  #Croston's
  model = "Croston"
  usedMethods <- c(usedMethods, model)
  #blenderData[model] <- as.data.frame(crost(ensembleData[,labelName], h=blend_size,
   #                                         w=NULL, cost='mae', init.opt = TRUE)$components$c.out)$Demand
  testingData[model] <- as.data.frame(crost(ensembleData[,labelName], h=test,
                                            w=NULL, cost='mae', init.opt = TRUE)$components$c.out)$Demand
  
  #SBA
  model = "SBA"
  usedMethods <- c(usedMethods, model)
  testingData[model] <- as.data.frame(crost(ensembleData[,labelName], h=test, type = "sba",
                                            w=NULL, cost='mae', init.opt = TRUE)$components$c.out)$Demand
  
  #TSB
  model = "TSB"
  usedMethods <- c(usedMethods, model)
  #blenderData[model] <- tsb(ensembleData[,labelName], h=blend_size, init.opt = TRUE)$frc.out
  testingData[model] <- tsb(ensembleData[,labelName], h=test, init.opt = TRUE)$frc.out
  
  write.csv(testingData$mean,"resxgb.csv", row.names = FALSE)
  


  for (mymodel in usedMethods) {
    ms <- measures(testingData$sales, testingData[[mymodel]], testingData$sales, benchmark = "naive")
    ForecastAndID <- cbind(product_id = ids[id],
                           categorization = categorizationName,
                           forecastingGroup = paste(mymodel, collapse=","),
                           t(as.data.frame(ms)))
    
    if (!is.null(results)){
      results <- rbind(results, ForecastAndID)
    } else {
      results <- ForecastAndID
    }
  }
  
}

stopCluster(c1)


writeCSV(results, 100)
