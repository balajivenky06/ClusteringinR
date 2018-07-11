#Principal Component Analysis

input <- read.csv("C:\\Users\\hp\\Desktop\\Personal Folder\\ISB CBA\\Data Mining 1\\DMG1 Project\\wine.csv", header = T)
dim(input)

mydata <- scale(input)

pcaObj <- princomp(input, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaObj)

#Plotting
plot(pcaObj)

loadings(pcaObj) #Loading table of principal components

biplot(pcaObj$scores[,1:2],pcaObj$loading[,1:2], col = c("Red","Blue"))

set.seed(0)
#K-Means Clustering on Wine Data

clusterVariability <- matrix(nrow = 7, ncol = 1)
for(i in 1:7) {
  clusterVariability[i] = kmeans(mydata, centers = i, iter.max = 10, nstart = 4)$tot.withinss
}  
plot(1:7, clusterVariability, type = "b", xlab = "No of clusters", ylab ="Variablity")

fit <- kmeans(mydata, centers = 3, iter.max = 10, nstart = 4)
t(fit$centers)

install.packages("fpc")
library(fpc)
plotcluster(mydata, fit$cluster)

#K-Means Clustering with PCA first 2 components
fit1 <- kmeans(pcaObj$scores[,1:2], centers=3, iter.max = 10, nstart = 4)
t(fit1$centers)

plotcluster(mydata, fit1$cluster)
