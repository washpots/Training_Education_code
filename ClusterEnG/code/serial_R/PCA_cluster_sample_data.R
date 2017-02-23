#!/usr/bin/env Rscript

#Mohith Manjunath
#2015-2016

library(data.table)
options(stringsAsFactors = FALSE)

#input arguments
args <- commandArgs(TRUE)
algoName <- args[1]
num <- as.integer(args[2])
cluster.by <- args[3]
is.row.names <- "yes"
sample.file <- args[4]
dbscan.eps <- as.numeric(args[5])
kmeans.rand.start <- as.numeric(args[6])
if (is.na(kmeans.rand.start))
  kmeans.rand.start <- 1
print(paste0("rand start: ", kmeans.rand.start))

#system("cat /proc/meminfo > /code/knoweng_meminfo1.txt")
#system("cat /proc/cpuinfo > /code/knoweng_cpuinfo1.txt")

if (sample.file == "nci60-data") {
  data.orig <- fread("/education/input/files/GSE2003_series_matrix_data.txt", header=TRUE, sep="auto", data.table=FALSE, na.strings = c("NA", "N/A", "?"));
} else {
  data.orig <- fread("/education/input/files/GSE60-GPL176_series_matrix.txt", header=TRUE, sep="auto", data.table=FALSE, na.strings = c("NA", "N/A", "?"));
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
pcaGEO <- prcomp(as.matrix(t(dd)))
fullDataPCA <- pcaGEO$x
fullData <- as.matrix(t(dd))
if (ncol(fullData) == 2) {
  colnames(fullData)[1:2] <- c("PC1", "PC2")
} else {
  colnames(fullData)[1:3] <- c("PC1", "PC2", "PC3")
}

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

numClusters <- num
if (algoName == "kmeans") {
  if (!require("cluster")) {
    install.packages("cluster")
  }
  library(cluster)
  fit <- kmeans(fullData, numClusters, nstart = kmeans.rand.start)
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
  if (!is.na(num)) {
    fit <- apclusterK(negDistMat(r=2), t(fullDataPCA), K = numClusters, details=TRUE)
  }
  else {
    fit <- apcluster(negDistMat(r=2), t(fullDataPCA), details=TRUE)
  }
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
  fit <- Mclust(fullDataPCA, G = numClusters)
  mydata <- as.data.frame(fit$classification)
}
if (algoName == "hierarchical") {
  fit <- hclust(dist(fullData), method = "complete")
  mydata <- as.data.frame(cutree(fit, numClusters))
}
if (algoName == "dbscan") {
  if (!require("dbscan")) {
    install.packages("dbscan") 
  }
  library(dbscan)
  fit <- dbscan(fullData, eps = dbscan.eps, minPts = 2)
  mydata <- as.data.frame(fit$cluster + 1L)
}

if (algoName == "dbscan-est") {
  if (!require("dbscan")) {
    install.packages("dbscan") 
  }
  library(dbscan)
  knn.dist <- kNNdist(fullData, k = 2)
  temp <- sort(as.numeric(knn.dist))
  N <- length(temp)
  png("/education/output/dbscan_knn_plot.png")
  plot(1:N, temp, xlab = "Points sorted by distance", ylab = "2-nearest neighbors distance")
  dev.off()
  Bvec <- c(N, temp[N])
  Avec <- c(1, temp[1])
  i <- 1
  dists <- sapply(temp, function(p){
    BAnorm <- (Avec - Bvec)/sqrt(sum((Bvec - Avec) ^ 2))
    pvec <- c(i, temp[i])
    dist <- sqrt(sum((pvec - (sum(pvec*BAnorm)*(BAnorm))) ^ 2))
    i <<- i + 1
    dist
  })
  eps.est.min <- round(temp[which.max(dists)], digits = 3)
  slope <- predict(smooth.spline(1:N, temp), 1:N, 1)
  sec <- predict(smooth.spline(1:N, slope$y), 1:N, 1)
  ext.point <- which(diff(sign(diff(sec$y)))==-2)+1
  #ext.point2 <- which(diff(sign(diff(sec$y)))==2)+1
  eps.est.max <- round(temp[ext.point[length(ext.point)]], digits = 3)
  #eps.est2 <- round(temp[ext.point2[length(ext.point2)]], digits = 2)
  write.table(paste(eps.est.min, "-", eps.est.max, sep = ""), "/education/output/output_dbscan_eps_est.csv", col.names = F, row.names = F, quote = F)
} else {
colnames(mydata) <- c("cluster")
sample <- as.data.frame(rownames(fullData))
colnames(sample) <- c("sample")
filename <- paste("/education/output/output_", algoName, "_annot.csv", sep="")
write.table(cbind(sample, mydata), file=filename, sep = ",", row.names = FALSE)

combinedData <- cbind(fullDataPCA, mydata, sample)
filename <- paste("/education/output/output_", algoName, "_PC.csv", sep="")
write.table(combinedData, file=filename, sep = ",", row.names = FALSE)
ids <- as.data.frame(0:(nrow(combinedData)-1))
colnames(ids) <- c("ID")
finalData <- cbind(ids, sample, fullDataPCA, mydata)
filename <- paste("/education/output/output_", algoName, "_full.csv", sep="")
write.table(finalData, file = filename, sep = ",", row.names = FALSE)
}
print(algoName);
