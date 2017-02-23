#!/usr/bin/env Rscript

#Mohith Manjunath
#2015-2016

if (!require("cluster")) {
  install.packages("cluster")
}
library(cluster)
if (!require("kernlab")) {
  install.packages("kernlab")
}
library(kernlab)
if (!require("apcluster")) {
  install.packages("apcluster")
}
if (!require("methods")) {
  install.packages("methods")
}
library(apcluster)
library(methods)
if (!require("mclust")) {
  install.packages("mclust") 
}
library(mclust)
if (!require("data.table")) {
  install.packages("data.table") 
}
library(data.table)
options(stringsAsFactors = FALSE)

d <- fread("/education/input/GSE2003_series_matrix_data.txt", header=TRUE, sep="\t", data.table = FALSE);
data <- na.omit(d)
pca <- prcomp(t(as.matrix(data)))
fullData <- pca$x
colnames(fullData)[1:3] <- c("xdata", "ydata", "zdata")
write.table(fullData, file="/education/output/pca_values.csv", sep = ",")

#input arguments
args <- commandArgs(TRUE)
algoName <- args[1]
num <- as.integer(args[2])

numClusters <- num
if (algoName == "kmeans") {
  fit <- kmeans(fullData, numClusters)
  mydata <- as.data.frame(fit$cluster)
}
if (algoName == "kmedoids") {
  fit <- pam(fullData, numClusters)
  mydata <- as.data.frame(fit$cluster)
}
if (algoName == "AP") {
  if (!is.na(num)) {
    fit <- apclusterK(negDistMat(r=2), t(fullData), K = numClusters)
  } else {
    fit <- apcluster(negDistMat(r=2), t(fullData))
  }
  mydata <- as.data.frame(labels(fit, type="enum"))
}
if (algoName == "SC") {
  fit <- specc(data.matrix(fullData), centers = numClusters)
  mydata <- as.data.frame(fit[1:nrow(fullData)])
}
if (algoName == "GM") {
  fit <- Mclust(fullData, G = numClusters)
  mydata <- as.data.frame(fit$classification)
}

colnames(mydata) <- c("category")
colorCode <- as.factor(mydata$category)
write.table(mydata, file="/education/output/pca_cluster.csv", sep = ",", row.names = FALSE)

sample <- as.data.frame(rownames(fullData))
colnames(sample) <- c("sample")
combinedData <- cbind(fullData, mydata, sample)
filename <- paste("/education/output/pca_", algoName, ".csv", sep="")
write.table(combinedData, file=filename, sep = ",", row.names = FALSE)
print(algoName);

#clusplot(fullData, fit1$cluster, main = "k-means", sub = "", color = TRUE, col.p = colorCode, shade = TRUE, lines = 0, xpd = NA)
#par(xpd=TRUE)
#legend("bottomright", legend = levels(colorCode), text.col=seq_along(levels(colorCode)))