#!/usr/bin/env Rscript

#Affinity propagation
#Mohith Manjunath
#2015-2016

if (!require("apcluster")) {
        install.packages("apcluster")
}
library(apcluster)
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
if (!is.na(num)) {
  fit <- apclusterK(negDistMat(r=2), mydata, K = numClusters)
} else {
  fit <- apcluster(negDistMat(r=2), mydata, q=0)
}
output <- data.frame(xdata = xData, ydata = yData, category = labels(fit, type="enum"))
write.csv <- write.table(output, file = "/education/output/output_AP.csv", sep = ",", row.names = FALSE, col.names = TRUE)
