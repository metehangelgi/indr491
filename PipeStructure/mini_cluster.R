
library(factoextra)
library(cluster)
library(readr)
library(dplyr)
library(forecast)
library(glmnet)
library(tsintermittent)

#folder names given manually to work on
args = commandArgs(trailingOnly=TRUE)
numofSample=as.character(args[1])
inputy <- c("featureCreation/new300Y.csv")
inputy2 <- paste(inputy, collapse="")
inputx <- c("featureCreation/new300.csv")
inputx2 <- paste(inputx, collapse="")
inputElastic<-c("featureSelection/newElastic300.csv")
inputElastic2 <- paste(inputElastic, collapse="")
elastic_data <- read_csv(inputElastic2)
ydata <- read_csv(inputy2)
xdata <- read_csv(inputx2)

#merge back the categorical features that dropped in the feature selection process
patterns <- c("product_id","brand_ID_.*","size_.*","gender_.*")
any_matching = Reduce(`|`, lapply(patterns, grepl, colnames(xdata)))
resultX = colnames(xdata)[any_matching]
elastic_with_categoric <- unique(left_join(elastic_data, xdata[,(names(xdata) %in% resultX)], by= 'product_id'))


#comment out below lines to make categorization with only categorical data
categories <- elastic_with_categoric[,(names(elastic_with_categoric) %in% resultX)]
categories <- categories[, colSums(categories != 0) > 0]


#categorization based on ABC and SBC features of the products

input_abc <- c("dataCategorization/new300ABC.csv")
input_abc2 <- paste(input_abc, collapse="")
input_sbc <- c("dataCategorization/newNASBC.csv")
input_sbc2 <- paste(input_sbc, collapse="")

abc_data <- read_csv(input_abc2)
sbc_data <- read_csv(input_sbc2)

#####These two lines is used for the alignment of the SBC and ABC data
#####after implementation of the new version(discrimination of the products whose training sales lower than 15)
#####this alignment can be removed
prodIDs=sbc_data[['id']]
abc_data <-subset(abc_data, product_id %in% prodIDs)


####empty vectors first 12 categories
A_Smooth <- vector()
A_Lumpy <- vector()
A_Intermittent <- vector()
A_Erratic <- vector()
B_Smooth <- vector()
B_Lumpy <- vector()
B_Intermittent <- vector()
B_Erratic <- vector()
C_Smooth <- vector()
C_Lumpy <- vector()
C_Intermittent <- vector()
C_Erratic <- vector()

###fill the vectors (can be optimized)
for(idd in 1:length(prodIDs)){
  prod_ABC <- filter(abc_data, product_id == prodIDs[idd])[, -1]
  prod_SBC <- filter(sbc_data, id == prodIDs[idd])[, -1]
  
  if(grepl('Smooth', prod_SBC[["demand_cate"]]) && grepl('A', prod_ABC[["ABCGroup"]])){
    A_Smooth <- c(A_Smooth, prodIDs[idd])
  }else if(prod_SBC[["demand_cate"]] == "Lumpy" && prod_ABC[["ABCGroup"]] == "A"){
    A_Lumpy <- c(A_Lumpy, prodIDs[idd])
  }else if(prod_SBC[["demand_cate"]] == "Intermittent" && prod_ABC[["ABCGroup"]] == "A"){
    A_Intermittent <- c(A_Intermittent, prodIDs[idd])
  }else if(prod_SBC[["demand_cate"]] == "Erratic" && prod_ABC[["ABCGroup"]] == "A"){
    A_Erratic <- c(A_Erratic, prodIDs[idd])
  }else if(prod_SBC[["demand_cate"]] == "Smooth" && prod_ABC[["ABCGroup"]] == "B"){
    B_Smooth <- c(B_Smooth, prodIDs[idd])
  }else if(prod_SBC[["demand_cate"]] == "Lumpy" && prod_ABC[["ABCGroup"]] == "B"){
    B_Lumpy <- c(B_Lumpy, prodIDs[idd])
  }else if(prod_SBC[["demand_cate"]] == "Intermittent" && prod_ABC[["ABCGroup"]] == "B"){
    B_Intermittent <- c(B_Intermittent, prodIDs[idd])
  }else if(prod_SBC[["demand_cate"]] == "Erratic" && prod_ABC[["ABCGroup"]] == "B"){
    B_Erratic <- c(B_Erratic, prodIDs[idd])
  }else if(prod_SBC[["demand_cate"]] == "Smooth" && prod_ABC[["ABCGroup"]] == "C"){
    C_Smooth <- c(C_Smooth, prodIDs[idd])
  }else if(prod_SBC[["demand_cate"]] == "Lumpy" && prod_ABC[["ABCGroup"]] == "C"){
    C_Lumpy <- c(C_Lumpy, prodIDs[idd])
  }else if(prod_SBC[["demand_cate"]] == "Intermittent" && prod_ABC[["ABCGroup"]] == "C"){
    C_Intermittent <- c(C_Intermittent, prodIDs[idd])
  }else if(prod_SBC[["demand_cate"]] == "Erratic" && prod_ABC[["ABCGroup"]] == "C"){
    C_Erratic <- c(C_Erratic, prodIDs[idd])
  }
}

######Selection of which category will be clustered (change B_Intermittent)
sbset <- subset(categories, product_id %in% B_Intermittent)
sbset <- sbset[, colSums(sbset != 0) > 0]
###dropping product_id's for clustering purposes
sbset2 <- subset(sbset, select = -c(product_id))

####elbow method to decide optimal number of clusters (for B_Intermittent it is 2)
####elbow method can be researched
####if the number of product in any category(A_Lumpy, B_Intermittent vs.) is very low,
####and there is no meaningful cluster numbers, then go directly to model training part 
fviz_nbclust(sbset2, kmeans, method = "wss", k.max = 10)

set.seed(5)

#perform k-means clustering with optimal number of clusters
km <- kmeans(sbset2, centers = 2, nstart = 25)

#view results
km

#merge back to cluster numbers to categoric data to work on different clusters
final_data <- cbind(sbset, cluster = km$cluster)

#drop categoric data
patterns <- c("brand_ID_.*","size_.*","gender_.*")
any_matching = Reduce(`|`, lapply(patterns, grepl, colnames(xdata)))
resultX = colnames(xdata)[! any_matching]
drop <- resultX
xdata=xdata[,(names(xdata) %in% drop)]

####product_id's of the selected cluster(change finaldata$cluster for different clusters)
####if elbow method produces a bad result directly assign ux like in the example

ux = final_data[which(final_data$cluster == 1), ][["product_id"]]
#ux = A_Lumpy


#ets model below uncomment to use it

# for (prodIDIndex in 1:length(ux))
# {
#   print(prodIDIndex)
#   prod_x <- filter(xdata, product_id == ux[prodIDIndex])[, -1] ## product x data
#   prod_y <- filter(ydata, product_id == ux[prodIDIndex])[, -1] ## product y data
#   # df = cbind(prod_y, prod_x)
#   prod_y_train <- head(prod_y, round(nrow(prod_y) * 0.8))
#   h <- nrow(prod_y) - nrow(prod_y_train)
#   prod_y_test <- tail(prod_y, h)
# 
#   prod_x_train <- head(prod_x, round(nrow(prod_x) * 0.8))
#   h <- nrow(prod_x) - nrow(prod_x_train)
#   prod_x_test <- tail(prod_x, h)
# 
#   ets.fit<- ets(as.vector(prod_y_train$sales))
# 
#   ets.forecast <- forecast(ets.fit,length(prod_y_test$sales))
#   ets.errors <- accuracy(ets.forecast, as.matrix(prod_y_test$sales))
#   #print(ets.forecast)
#   print(ets.errors)
#   #print(summary(ets.forecast))
# }

######SYNTETOS-BOYLAN APPROXIMATION IN THE FOR LOOP
for (prodIDIndex in 1:length(ux))
{
  print(prodIDIndex)
  prod_x <- filter(xdata, product_id == ux[4])[, -1] ## product x data
  prod_y <- filter(ydata, product_id == ux[4])[, -1] ## product y data
  # df = cbind(prod_y, prod_x)
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
  
}


###DYNAMIC REGRESSION

# is.rankdeficient <- function(xregg) {
#   constant_columns <- apply(xregg, 2, is.constant)
#   if (any(constant_columns)) {
#     xregg <- xregg[, -which(constant_columns)[1]]
#   }
#   sv <- svd(na.omit(cbind(rep(1, NROW(xregg)), xregg)))$d
#   min(sv)/sum(sv) < .Machine$double.eps
# }
# 
# for (prodIDIndex in 1:length(ux))
# {
#   print(prodIDIndex)
#   #lasso_coef <- filter(lasso_coefs, product_id == ux[prodIDIndex])[, -1] ## lasso
#   elastic_coef <- filter(elastic_data, product_id == ux[prodIDIndex])[, -1] ## elastic
#   prod_x <- filter(xdata, product_id == ux[prodIDIndex])[, -1] ## product x data
#   prod_y <- filter(ydata, product_id == ux[prodIDIndex])[, -1] ## product y data
#   # df = cbind(prod_y, prod_x)
#   prod_y_train <- head(prod_y, round(nrow(prod_y) * 0.8))
#   h <- nrow(prod_y) - nrow(prod_y_train)
#   prod_y_test <- tail(prod_y, h)
# 
#   prod_x_train <- head(prod_x, round(nrow(prod_x) * 0.8))
#   h <- nrow(prod_x) - nrow(prod_x_train)
#   prod_x_test <- tail(prod_x, h)
#   #prod_feature <- lasso_coef[-1] ## list of elemnts to include
#   #rod_feature <- lasso_coef ## list of elemnts to include
#   #regressors <- which(prod_feature!= 0) ## indices of non-zero elements
#   #regressors <- which(elastic_coef[-1]!= 0) # if elastic net is desired
# 
#   regressors <- which(elastic_coef!= 0) # if elastic net is desired
# 
# 
#   #rank deficient sıkıntısı var çözemedim, şimdilik bu şekilde handle ettim
#   prod_x_train <- prod_x_train[,regressors][, colSums(prod_x_train[,regressors] != 0) > 0]
# 
#   dyano.fit <- auto.arima(prod_y_train[,"sales"],
#                           xreg=data.matrix(prod_x_train, rownames.force = NA))
# 
#   cbind("Regression Errors" = residuals(dyano.fit, type="regression"),
#         "ARIMA errors" = residuals(dyano.fit, type="innovation")) %>%
#     autoplot(facets=TRUE)
#   checkresiduals(dyano.fit)
# 
#   dyano.fit %>% forecast(xreg = as.matrix(prod_x_test[,regressors]),h=40) %>% autoplot(ylab = "Sales")
#   dyno.forecast <- dyano.fit %>% forecast(xreg = as.matrix(prod_x_test[,regressors]),h=40)
# 
#   dyno.erors <- accuracy(dyno.forecast, prod_y_test$sales)
# 
#   print(dyno.forecast)
#   print(dyno.erors)
# 
# }










