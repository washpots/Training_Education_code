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
is.row.names <- args[3]
file.name <- args[4]
algoName <- "pspectral_clustering"

setwd("/code/parallel/pspectralclustering/testdata")
d <- fread("/education/input/GSE60-GPL176_series_matrix.txt", header=TRUE, sep="\t", data.table=FALSE);
dd <- na.omit(d)
myvars <- names(dd) %in% c("row.names","ID_REF")
dataGEO <- dd[!myvars]

data.new <- (apply(dataGEO, 1, function(row) {
  numElem <- length(row)
  res <- mapply(function(i, val) {
    paste(paste(i, ":", sep = ""), val, sep = "")
  }, 0:(numElem-1), row)
  res
}))

write.table(data.new, "input_data_psc.txt", sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

system(paste("./submit.sh ", num, sep = ""))

pcaGEO <- prcomp(t(as.matrix(dataGEO)))
fullData <- pcaGEO$x
colnames(fullData)[1:3] <- c("PC1", "PC2", "PC3")
colorCode <- as.factor(rownames(fullData))

mydata <- fread("result.txt", sep = "auto", data.table = FALSE)
colnames(mydata) <- c("cluster")
sample <- as.data.frame(rownames(fullData))
colnames(sample) <- c("sample")
filename <- paste("/education/output/output_", algoName, "_annot.csv", sep="")
write.table(cbind(sample, mydata), file=filename, sep = ",", row.names = FALSE)

combinedData <- cbind(fullData, mydata)
filename <- paste("/education/output/output_", algoName, "_PC.csv", sep="")
write.table(combinedData, file=filename, sep = ",", row.names = FALSE)
ids <- as.data.frame(0:(nrow(combinedData)-1))
colnames(ids) <- c("ID")
finalData <- cbind(ids, sample, fullData, mydata)
filename <- paste("/education/output/output_", algoName, "_full.csv", sep="")
write.table(finalData, file = filename, sep = ",", row.names = FALSE)
print(algoName);