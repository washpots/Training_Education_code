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
is.row.names <- args[3]
file.name <- args[4]
algoName <- "pspectral_clustering"

input.start <- Sys.time()
data.orig <- fread(paste(c("/education/input/files/", file.name), collapse = ""), sep = "auto", data.table = FALSE, na.strings = c("NA", "N/A", "?"))
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
data <- na.omit(data.orig.mod)
pca.start <- Sys.time()
pca <- prcomp(as.matrix(t(data)))
pca.time <- Sys.time() - as.numeric(pca.start, units = "secs")
fullData <- pca$x
rm(pca)
#fullData <- data
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

print("Writing files...")
output.start <- Sys.time()
mydata <- fread("result.txt", sep = "auto", data.table = FALSE)
colnames(mydata) <- c("cluster")
mydata <- mydata + 1
sample <- as.data.frame(rownames(as.data.frame(fullData)))
colnames(sample) <- c("sample")
filename <- paste("/education/output/output_", algoName, "_annot.csv", sep="")
write.table(cbind(sample, mydata), file=filename, sep = ",", row.names = FALSE)

combinedData <- cbind(fullData, mydata)
filename <- paste("/education/output/output_", algoName, "_PC.csv", sep="")
write.table(combinedData, file=filename, sep = ",", row.names = FALSE)
ids <- as.data.frame(0:(nrow(combinedData)-1))
colnames(ids) <- c("ID")
finalData <- cbind(ids, sample, fullData, mydata)
file.name <- paste("/education/output/output_", algoName, "_full.csv", sep="")
write.table(finalData, file = file.name, sep = ",", row.names = FALSE)
input.output.time <- Sys.time() - as.numeric(output.start, units = "secs") + as.numeric(input.time, units = "secs")
memory.used <- mem_used()/1e6
benchmark.result <- t(data.frame(c(Time = as.character(Sys.time()), File = file.name, Algorithm = algoName, Cluster_by = cluster.by, IO_Time = input.output.time, PCA_Time = pca.time, Algorithm_Time = algo.time, Memory_used_MB = as.character(memory.used))))
write.table(benchmark.result, file = "/code/benchmark.txt", sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
print(paste("Memory used (MB) = ", memory.used))
print(algoName);