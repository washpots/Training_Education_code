#!/usr/bin/env Rscript

#Mohith Manjunath
#2015-2016

library(data.table)
options(stringsAsFactors = FALSE)

d <- fread("/education/input/GSE60-GPL176_series_matrix.txt", header=TRUE, sep="\t", data.table=FALSE);
dd <- na.omit(d)
myvars <- names(dd) %in% c("row.names","ID_REF")
dataGEO <- dd[!myvars]
pcaGEO <- prcomp(t(as.matrix(dataGEO)))
fullData <- pcaGEO$x
colnames(fullData)[1:3] <- c("PC1", "PC2", "PC3")
#write.table(fullData, file="/education/output/pca_values.csv", sep = ",")

# rownames(fullData)[1:9] <- "Breast tumor"
# rownames(fullData)[10:15] <- "CNS tumor"
# rownames(fullData)[16:22] <- "Colon tumor"
# rownames(fullData)[23:30] <- "Leukemia"
# rownames(fullData)[31:38] <- "Melanoma"
# rownames(fullData)[39:47] <- "Non-small cell lung cancer (NSCLC)"
# rownames(fullData)[48:53] <- "Ovarian tumor"
# rownames(fullData)[54:55] <- "Prostate tumor"
# rownames(fullData)[56:63] <- "Renal tumor"
# rownames(fullData)[64] <- "Unknown"
colorCode <- as.factor(rownames(fullData))

#input arguments
args <- commandArgs(TRUE)
algoName <- args[1]
num <- as.integer(args[2])

numClusters <- num
if (algoName == "kmeans") {
  if (!require("cluster")) {
    install.packages("cluster")
  }
  library(cluster)
  fit <- kmeans(fullData, numClusters)
  mydata <- as.data.frame(fit$cluster)
}
if (algoName == "kmedoids") {
  if (!require("cluster")) {
    install.packages("cluster")
  }
  library(cluster)
  fit <- pam(fullData, numClusters)
  mydata <- as.data.frame(fit$cluster)
}
if (algoName == "AP") {
  if (!require("apcluster")) {
    install.packages("apcluster")
  }
  if (!require("methods")) {
    install.packages("methods")
  }
  library(apcluster)
  library(methods)
  fit <- apclusterK(negDistMat(r=2), t(fullData), K = numClusters, details=TRUE)
  mydata <- as.data.frame(labels(fit, type="enum"))
}
if (algoName == "SC") {
  if (!require("kernlab")) {
    install.packages("kernlab")
  }
  library(kernlab)
  fit <- specc(data.matrix(fullData), centers = numClusters)
  mydata <- as.data.frame(fit[1:nrow(fullData)])
}
if (algoName == "GM") {
  if (!require("mclust")) {
    install.packages("mclust") 
  }
  library(mclust)
  fit <- Mclust(fullData, G = numClusters)
  mydata <- as.data.frame(fit$classification)
}
if (algoName == "hierarchical") {
  fit <- hclust(dist(fullData), method = "complete")
  mydata <- as.data.frame(cutree(fit, numClusters))
}

colnames(mydata) <- c("cluster")
sample <- as.data.frame(rownames(fullData))
colnames(sample) <- c("sample")
filename <- paste("/education/output/output_", algoName, "_annot.csv", sep="")
write.table(cbind(sample, mydata), file=filename, sep = ",", row.names = FALSE)

combinedData <- cbind(fullData, mydata, sample)
filename <- paste("/education/output/output_", algoName, "_PC.csv", sep="")
write.table(combinedData, file=filename, sep = ",", row.names = FALSE)
ids <- as.data.frame(0:(nrow(combinedData)-1))
colnames(ids) <- c("ID")
finalData <- cbind(ids, sample, fullData, mydata)
filename <- paste("/education/output/output_", algoName, "_full.csv", sep="")
write.table(finalData, file = filename, sep = ",", row.names = FALSE)
print(algoName);