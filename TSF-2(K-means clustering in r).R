# Installing the required R packages
install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("factoextra")

# Attaching the above R packages
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(factoextra)

# Importing the iris dataset
data <- iris

# Checking the length of the data
length(data)

# Viewing the first 6 rows of the iris dataset
head(data)

# Viewing the last 6 rows of the iris dataset
tail(data)

# Structure of the iris dataset
str(data)

# Summary of thr iris dataset
summary(data)

# Checking for missing values
any(is.na(data))

# Creating new dataset for performing K-Means clustering
TSF2 <- data[ , (1:4)]

# Viewing the new dataset
head(TSF2)

# Determining the optimum no of cluters for K-means clustering.
# 1) The Elbow Method
# WSSplot function (Within-Cluster-Sum of Squared errors)
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

# Calling the WSSplot function and displaying the WSSplot for the dataset
wssplot(TSF2)

# 2) The Silhouette Method (Optimum number of clusters for the dataset)
fviz_nbclust(TSF2, kmeans, method='silhouette')

# Applying K-means clustering with no of clusters as 2
KM <- kmeans(TSF2, centers = 2)

# Viewing the K-means clusters size
KM$size

# Viewing the K-means clusters of the dataset
KM$cluster

# Viewing the K-means ckusters centers
KM$centers

# Visualizing the clusters
autoplot(KM, TSF2, frame = TRUE)


