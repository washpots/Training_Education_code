#!/usr/bin/env Rscript

#Spectral clustering
#Mohith Manjunath
#2015-2016

if (!require("kernlab")) {
        install.packages("kernlab")
}
library(kernlab)
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
fit <- specc(data.matrix(mydata), centers = numClusters)
output <- data.frame(xdata = xData, ydata = yData, category = fit[1:nrow(mydata)])
write.csv <- write.table(output, file = "/education/output/output_SC.csv", sep = ",", row.names = FALSE, col.names = TRUE)
