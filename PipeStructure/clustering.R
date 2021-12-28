# Title     : TODO
# Objective : TODO
# Created by: metehangelgi
# Created on: 28.12.2021

library(factoextra)
library(cluster)
library(readr)
library(dplyr)
library(forecast)
library(glmnet)
library(tsintermittent)

writeCSV <- function (final_data,numofSample,ABCtype,SBCtype){
  output00 <- c("clustering/new", numofSample,ABCtype,"_",SBCtype,".csv")
  output002 <- paste(output00, collapse="")
  write.csv(final_data,output002, row.names = FALSE)
}

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

inputCategorization <- c("dataCategorization/new", numofSample,"Combined.csv")
inputCategorization2 <- paste(inputCategorization, collapse="")
categorized_data <- read_csv(inputCategorization2)

#merge back the categorical features that dropped in the feature selection process
patterns <- c("product_id","brand_ID_.*","size_.*","gender_.*")
any_matching = Reduce(`|`, lapply(patterns, grepl, colnames(xdata)))
resultX = colnames(xdata)[any_matching]
elastic_with_categoric <- unique(left_join(elastic_coefs, xdata[,(names(xdata) %in% resultX)], by= 'product_id'))

#comment out below lines to make categorization with only categorical data
categories <- elastic_with_categoric[,(names(elastic_with_categoric) %in% resultX)]
categories <- categories[, colSums(categories != 0) > 0]


categorized_dataIDs <- list(
  A_Smooth = vector(),
  A_Lumpy = vector(),
  A_Intermittent = vector(),
  A_Erratic = vector(),
  B_Smooth = vector(),
  B_Lumpy = vector(),
  B_Intermittent = vector(),
  B_Erratic = vector(),
  C_Smooth = vector(),
  C_Lumpy = vector(),
  C_Intermittent = vector(),
  C_Erratic = vector()
    )

categorized_dataCategories <- list(c("A","Smooth"),c("A","Lumpy"),c("A","Intermittent"),c("A","Erratic"),
                                   c("B","Smooth"),c("B","Lumpy"),c("B","Intermittent"),c("B","Erratic"),
                                   c("C","Smooth"),c("C","Lumpy"),c("C","Intermittent"),c("C","Erratic"))

for (a in c(1:length(categorized_dataCategories))){
  ABCtype<-categorized_dataCategories[[a]][[1]]
  SBCtype<-categorized_dataCategories[[a]][[2]]
  filtered <- categorized_data %>%
    filter(demand_cate==SBCtype & ABCGroup==ABCtype)
  AAprodIDs=filtered[['product_id']]
  categorized_dataIDs[[a]]=AAprodIDs
}

for (i in c(1:length(categorized_dataIDs))){
  ######Selection of which category will be clustered (change B_Intermittent)
  ABCtype<-categorized_dataCategories[[i]][[1]]
  SBCtype<-categorized_dataCategories[[i]][[2]]
  if(length(categorized_dataIDs[[i]])==0) {
    final_data=NULL
    writeCSV(final_data,numofSample,ABCtype,SBCtype)
    next
  }


  sbset <- subset(categories, product_id %in% categorized_dataIDs[[i]])
  sbset <- sbset[, colSums(sbset != 0) > 0]
  ###dropping product_id's for clustering purposes
  #print(sbset$product_id)
  sbset2 <- subset(sbset, select = -c(product_id))

  possibleError <- tryCatch(
    fviz_nbclust(sbset2, kmeans, method = "wss", k.max = 10),
    error=function(e) e
  )

  if(inherits(possibleError, "error")) {
    final_dataPre <- cbind(sbset, cluster = 0)
    writeCSV(final_dataPre,numofSample,ABCtype,SBCtype)
    next
  }

  ####elbow method to decide optimal number of clusters (for B_Intermittent it is 2)
  ####elbow method can be researched
  ####if the number of product in any category(A_Lumpy, B_Intermittent vs.) is very low,
  ####and there is no meaningful cluster numbers, then go directly to model training part
  fviz_nbclust(sbset2, kmeans, method = "wss", k.max = 10)
  set.seed(5)
  #perform k-means clustering with optimal number of clusters
  km <- kmeans(sbset2, centers = 2, nstart = 25)

  #view results
  #km
  #merge back to cluster numbers to categoric data to work on different clusters
  final_data <- cbind(sbset, cluster = km$cluster)
  writeCSV(final_data,numofSample,ABCtype,SBCtype)
}




