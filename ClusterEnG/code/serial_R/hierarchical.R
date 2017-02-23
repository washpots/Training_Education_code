#!/usr/bin/env Rscript

#Hierarchical clustering
#Mohith Manjunath
#2015-2016

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
fit <- hclust(dist(mydata), method = "complete")
dendroClus <- cutree(fit, numClusters)
output <- data.frame(xdata = xData, ydata = yData, category = dendroClus)
write.csv <- write.table(output, file="/education/output/output_hierarchical.csv", sep = ",", row.names = FALSE, col.names = TRUE)