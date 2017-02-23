#!/usr/bin/env Rscript

#convert csv file to parallel spectral clustering code input format
#Mohith Manjunath
#2016

library(data.table)
options(stringsAsFactors = FALSE)

#input arguments
args <- commandArgs(TRUE)
num <- as.integer(args[1])
cluster.by <- args[2]
is.row.names <- "yes"
sample.file <- args[4]
algoName <- "pkmeans"

setwd("/code/parallel/parallel-kmeans/Image_data")
if (sample.file == "nci60-data") {
  data.orig <- fread("/education/input/files/GSE2003_series_matrix_data.txt", header=TRUE, sep="\t", data.table=FALSE);
} else {
  data.orig <- fread("/education/input/files/GSE60-GPL176_series_matrix.txt", header=TRUE, sep="\t", data.table=FALSE);
}

data.orig.mod <- data.orig

if (is.row.names == "yes") {
  data.orig.mod <- data.orig[, 2:ncol(data.orig)]
  rownames(data.orig.mod) <- data.orig[, 1]
  print("Inside rownames check.")
}
if (cluster.by == "rows") {
  data.orig.mod <- t(data.orig.mod)
}

dd <- na.omit(data.orig.mod)
#myvars <- names(dd) %in% c("row.names","ID_REF")
#dataGEO <- dd[!myvars]
pcaGEO <- prcomp((as.matrix(t(dd))))
fullDataPCA <- pcaGEO$x
fullData <- as.matrix(t(dd))
write.table(cbind(1:nrow(fullData), fullData), "input_data.txt", sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

system(paste("./submit.sh ", num, sep = ""))

#colnames(fullData)[1:3] <- c("PC1", "PC2", "PC3")

if (sample.file == "nci60-data"  & cluster.by == "columns") {
  rownames(fullData)[1:9] <- "Breast tumor"
  rownames(fullData)[10:15] <- "CNS tumor"
  rownames(fullData)[16:22] <- "Colon tumor"
  rownames(fullData)[23:30] <- "Leukemia"
  rownames(fullData)[31:38] <- "Melanoma"
  rownames(fullData)[39:47] <- "Non-small cell lung cancer (NSCLC)"
  rownames(fullData)[48:53] <- "Ovarian tumor"
  rownames(fullData)[54:55] <- "Prostate tumor"
  rownames(fullData)[56:63] <- "Renal tumor"
  rownames(fullData)[64] <- "Unknown"
}
colorCode <- as.factor(rownames(fullData))

mydata <- fread("result.txt", sep = "auto", data.table = FALSE)
colnames(mydata) <- c("cluster")
sample <- as.data.frame(rownames(as.data.frame(fullData)))
colnames(sample) <- c("sample")
filename <- paste("/education/output/output_", algoName, "_annot.csv", sep="")
write.table(cbind(sample, mydata), file=filename, sep = ",", row.names = FALSE)

combinedData <- cbind(fullDataPCA, mydata)
filename <- paste("/education/output/output_", algoName, "_PC.csv", sep="")
write.table(combinedData, file=filename, sep = ",", row.names = FALSE)
ids <- as.data.frame(0:(nrow(combinedData)-1))
colnames(ids) <- c("ID")
finalData <- cbind(ids, sample, fullDataPCA, mydata)
filename <- paste("/education/output/output_", algoName, "_full.csv", sep="")
write.table(finalData, file = filename, sep = ",", row.names = FALSE)
print(algoName);