#!/usr/bin/env Rscript

#k-medoids clustering
#Mohith Manjunath
#2015-2016

if (!require("cluster")) {
        install.packages("cluster") 
}
library(cluster)
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
fit <- pam(mydata, numClusters)
output <- data.frame(xdata = xData, ydata = yData, category = fit$cluster)
write.csv <- write.table(output, file = "/education/output/output_kmedoids.csv", sep = ",", row.names = FALSE, col.names = TRUE)
