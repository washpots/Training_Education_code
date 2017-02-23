#!/usr/bin/env Rscript

#k-means clustering
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
pca <- prcomp(as.matrix(mydata))
fullData <- pca$x
colnames(fullData)[1:2] <- c("xdata", "ydata")

#cluster and write to csv file
fit <- kmeans(fullData, numClusters)
output <- cbind(fullData, data.frame(category = fit$cluster))
write.csv <- write.table(output, file = "/education/output/output_kmeans.csv", sep = ",", row.names = FALSE, col.names = TRUE)
