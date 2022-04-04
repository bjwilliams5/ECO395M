library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)

cars = read.csv('data/cars.csv', header=TRUE)

summary(cars)

# Center and scale the data
X = cars[,-(1:9)]
X = scale(X, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

# Run k-means with 6 clusters and 25 starts. We are "cheating" a little because we know there are 6 clusters in the true observations
clust1 = kmeans(X, 6, nstart=25)

# What are the clusters?
clust1$center  # not super helpful
clust1$center[1,]*sigma + mu
clust1$center[2,]*sigma + mu
clust1$center[6,]*sigma + mu


# Which cars are in which clusters?
which(clust1$cluster == 1)
which(clust1$cluster == 2)
which(clust1$cluster == 3)
which(clust1$cluster == 4)
which(clust1$cluster == 6)

# A few plots with cluster membership shown
# qplot is in the ggplot2 library
qplot(Weight, Length, data=cars, color=factor(clust1$cluster))
qplot(Horsepower, CityMPG, data=cars, color=factor(clust1$cluster))

# ggplot 2 equivalent
ggplot(cars) + 
  geom_point(aes(Weight, Length, color=factor(clust1$cluster)))

# Using kmeans++ initialization
clust2 = kmeanspp(X, k=6, nstart=25)

clust2$center[1,]*sigma + mu
clust2$center[2,]*sigma + mu
clust2$center[4,]*sigma + mu

# Which cars are in which clusters?
which(clust2$cluster == 1)
which(clust2$cluster == 2)
which(clust2$cluster == 3)

# Compare versus within-cluster average distances from the first run
clust1$withinss
clust2$withinss
sum(clust1$withinss)
sum(clust2$withinss)
clust1$tot.withinss
clust2$tot.withinss
clust1$betweenss
clust2$betweenss

library(foreach)
X = scale(cars[,10:18]) # cluster on measurables
k_grid = seq(2, 30, by=1)
SSE_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(X, k, nstart=50)
  cluster_k$tot.withinss
}

plot(k_grid, SSE_grid) 

N = nrow(X)
CH_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(X, k, nstart=50)
  W = cluster_k$tot.withinss
  B = cluster_k$betweenss
  CH = (B/W)*((N-k)/(k-1))
  CH 
}

plot(k_grid, CH_grid, las=1)

library(cluster)
cars_gap = clusGap(X, FUN = kmeans, nstart = 50, K.max = 10, B = 50)
plot(cars_gap, cex.main=0.7, cex.axis=0.7, cex.lab=0.7, las=1)

clusGap(X, FUN = kmeans, nstart = 50, K.max = 10, B = 50)
