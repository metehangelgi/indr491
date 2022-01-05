library(tsintermittent)
library(readr)
library(dplyr)


args = commandArgs(trailingOnly=TRUE)
numofSample=as.character(args[1])
inputy <- c("featureCreation/new100Y.csv")
inputy2 <- paste(inputy, collapse="")
inputx <- c("featureCreation/new100.csv")
inputx2 <- paste(inputx, collapse="")
ydata <- read_csv(inputy2)
xdata <- read_csv(inputx2)

prodIDs=xdata[['product_id']]
ux <- unique(prodIDs)

counter = 0

for (prodIDIndex in 1:length(ux))
{
  prod_y <- filter(ydata, product_id == ux[prodIDIndex])[, -1] ## product y data
  prod_y_train <- head(prod_y, round(nrow(prod_y) * 0.8))
  h <- nrow(prod_y) - nrow(prod_y_train)
  prod_y_test <- tail(prod_y, h)
  
  fitt <- tsb(prod_y_train[["sales"]], h=40, init = "naive")
  predicted = fitt$frc.out
  err2 <- measures(as.matrix(prod_y_test$sales), predicted, as.matrix(prod_y$sales), benchmark = "naive")
  #print((as.data.frame(err2)$err2)[9])
  if((as.data.frame(err2)$err2)[9] < 1){
    counter = counter + 1
  }
  print(counter)
}
