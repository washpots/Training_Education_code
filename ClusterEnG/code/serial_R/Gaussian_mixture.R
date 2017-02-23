#!/usr/bin/env Rscript

#Gaussian mixture models
#Mohith Manjunath
#2015-2016

if (!require("mclust")) {
        install.packages("mclust") 
}
library(mclust)
options(stringsAsFactors = FALSE)

#input arguments
args <- commandArgs(TRUE)
num <- as.integer(args[1])

#import data and specify number of clusters (from user)
numClusters <- num
mydata <- read.csv("/education/input/input_data.csv")
xData <- mydata[, 1]
yData <- mydata[, 2]

#cluster and write to csv file
fit = Mclust(mydata, G = numClusters)
output <- data.frame(xdata = xData, ydata = yData, category = fit$classification)
write.csv <- write.table(output, file = "/education/output/output_GM.csv", sep = ",", row.names = FALSE, col.names = TRUE)
