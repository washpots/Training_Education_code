#!/usr/bin/env Rscript

#convert csv file to parallel spectral clustering code input format
#Mohith Manjunath
#2016

setwd("/code/parallel/pspectralclustering/testdata")
library(data.table)
library(pryr)
options(stringsAsFactors = FALSE)

#input arguments
args <- commandArgs(TRUE)
num <- as.integer(args[1])
cluster.by <- args[2]
is.row.names <- "yes"
sample.file <- args[4]
algoName <- "pspectral_clustering"

input.start <- Sys.time()
if (sample.file == "nci60-data") {
  data.orig <- fread("/education/input/files/GSE2003_series_matrix_data.txt", header=TRUE, sep="\t", data.table=FALSE);
} else {
  data.orig <- fread("/education/input/files/GSE60-GPL176_series_matrix.txt", header=TRUE, sep="\t", data.table=FALSE);
}
input.time <- Sys.time() - as.numeric(input.start, units = "secs")
data.orig.mod <- data.orig

if (is.row.names == "yes") {
  data.orig.mod <- data.orig[, 2:ncol(data.orig)]
  rownames(data.orig.mod) <- data.orig[, 1]
  print("Inside rownames check.")
}
if (cluster.by == "rows") {
  data.orig.mod <- t(data.orig.mod)
}
rm(data.orig)

print("Computing principal components...")
dd <- na.omit(data.orig.mod)
#myvars <- names(dd) %in% c("row.names","ID_REF")
#dataGEO <- dd[!myvars]
pca.start <- Sys.time()
pcaGEO <- prcomp((as.matrix(t(dd))))
pca.time <- Sys.time() - as.numeric(pca.start, units = "secs")
fullDataPCA <- pcaGEO$x
rm(pcaGEO)
fullData <- as.matrix(t(dd))

data.new <- t(apply(fullData, 1, function(row) {
  numElem <- length(row)
  res <- mapply(function(i, val) {
    paste(paste(i, ":", sep = ""), val, sep = "")
  }, 0:(numElem-1), row)
  res
}))

write.table(data.new, "input_data_psc.txt", sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

print("Algorithm running...")
algo.start <- Sys.time()
system(paste("./submit.sh ", num, sep = ""))
algo.time <- Sys.time() - as.numeric(algo.start, units = "secs")

#colnames(fullData)[1:3] <- c("PC1", "PC2", "PC3")

if (sample.file == "nci60-data" & cluster.by == "columns") {
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

print("Writing files...")
output.start <- Sys.time()
mydata <- fread("result.txt", sep = "auto", data.table = FALSE)
colnames(mydata) <- c("cluster")
mydata <- mydata + 1
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
file.name <- paste("/education/output/output_", algoName, "_full.csv", sep="")
write.table(finalData, file = file.name, sep = ",", row.names = FALSE)
input.output.time <- Sys.time() - as.numeric(output.start, units = "secs") + as.numeric(input.time, units = "secs")
memory.used <- mem_used()/1e6
benchmark.result <- t(data.frame(c(Time = as.character(Sys.time()), File = file.name, Algorithm = algoName, Cluster_by = cluster.by, IO_Time = input.output.time, PCA_Time = pca.time, Algorithm_Time = algo.time, Memory_used_MB = as.character(memory.used))))
write.table(benchmark.result, file = "/code/benchmark.txt", sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
print(paste("Memory used (MB) = ", memory.used))
print(algoName);