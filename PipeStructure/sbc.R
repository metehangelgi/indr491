# Title     : TODO
# Objective : TODO
# Created by: metehangelgi
# Created on: 8.12.2021

library(tsintermittent)
library(readr)
library(reshape2)
library(ggplot2)
library("dplyr")

args = commandArgs(trailingOnly=TRUE)
numofSample=as.character(args[1])
inputy <- c("featureCreation/new100Y.csv")
inputy2 <- paste(inputy, collapse="")
ydata <- read_csv(inputy2)
lag <- 30
#numbering for dates to reshape the data to proper format
ydata$dates <- rep(1:(207-lag), 100)

#proper format for data to SBC categorization
new_df <- subset(dcast(ydata, dates ~ product_id, value.var="sales"), select = -c(dates))

#SBC categorization
ts_cate_obj <- idclass(new_df, type = "SBC", outplot = "none")


ts_categorization <- data.frame(product_id = row.names(t(new_df)), cv2 = ts_cate_obj$cv2,
                                 p = ts_cate_obj$p) %>%
  mutate(demand_cate = case_when(p < 1.32 & cv2 < 0.49 ~ "Smooth",
                                 p >= 1.32 & cv2 < 0.49 ~ "Intermittent",
                                 p < 1.32 & cv2 >= 0.49 ~ "Erratic",
                                 p >= 1.32 & cv2 >= 0.49 ~ "Lumpy"))
#ts_categorization = subset(ts_categorization, select = -c(p,cv2) )
#output <- c("dataCategorization/new", numofSample,"SBC",".csv")
#output2 <- paste(output, collapse="")
#write.csv(ts_categorization,output2, row.names = FALSE)


p <- ggplot(ts_categorization, aes(x = p, y = cv2, color= '#c50a2c'), xlim=c(0.7,2), ylim=c(0,6)) +
geom_point(size = 3, colour= '#c50a2c')


# a data frame with all the annotation info
annotation <- data.frame(
  x = c(1,1.75, 1, 1.75),
  y = c(2.5,2.5,0,0),
  label = c("ERRATIC", "LUMPY", "SMOOTH", "INTERMITTENT")
)

p +
  # horizontal
  geom_hline(yintercept=0.49, color="black", size=1) +
  # vertical
  geom_vline(xintercept=1.32, color="black", size=1) +

  #geom_text(data=annotation, aes(x=x, y=y, label=label),
  #           color="orange",
  #           size=7 , angle=0, fontface="bold" )


  geom_text(data=annotation, aes( x=x, y=y, label=label),
              color="black",
              size=5 , angle=45) +
  
  scale_x_continuous(limits = c(0.9,2)) +
  scale_y_continuous(limits = c(0,3)) +
  theme_void()
  
  


