#Loading Libraries
getwd()

#Loading East West Airline dataset CSV file
input <- read.csv("C:\\Users\\hp\\Desktop\\Personal Folder\\ISB CBA\\Data Mining 1\\DMG1 Project\\EastWestAirlines.csv")

mydata <- input[,2:ncol(input)] #Getting only required columns for analysis

normalize_data <- scale(mydata) #Normalize data
head(normalize_data)
set.seed(0)

d <- dist(normalize_data, method = "euclidean")
#Ward's minimum variance method is a special case of the objective function approach
#where the criterion for choosing the pair of clusters to merge at each step is based
#on the optimal value of an objective function. 
fit <- hclust(d, method = "ward.D") #hclust using Ward.D method linkage
plot(fit)
rect.hclust(fit, k = 3, border = "red")


#Getting Cluster details using Complete method

groups <- cutree(fit, k=3)
groups
membership <- as.matrix(groups)
membership
colnames(membership)[1] <- "Cluseter ID"
input <- cbind(input, membership)
head(input)
options(scipen=99999999)

clust.centroid = function(i, dat, groups) {
  ind = (groups == i)
  colMeans(dat[ind,])
}
sapply(unique(groups), clust.centroid, mydata, groups)



#Random Sampling 95% of records & Repeat the analysis

sample_data <- mydata[sample(nrow(mydata), replace = F, size = 0.95*nrow(mydata)), ]

normalize_sample_data <- scale(sample_data)
d <- dist(normalize_sample_data, method = "euclidean")
fit <- hclust(d, method = "ward.D")
plot(fit)
rect.hclust(fit, k=3, border = "red")


#Applying K-means Clustering using cluster variability Analysis

clusterVariability <- matrix(nrow = 5, ncol = 1)
for(i in 1:5) {
  clusterVariability[i] = kmeans(normalize_data, centers = i, iter.max = 10, nstart = 4)$tot.withinss
}  
plot(1:5, clusterVariability, type = "b", xlab = "No of clusters", ylab ="Variablity")

fit <- kmeans(mydata, centers = 3, iter.max = 10, nstart = 4)
t(fit$centers)

fit$size
