library(fastDummies)
library(tidyverse)
library(LICORS) 

wine = read.csv("../data/wine.csv")

wine_scaled <- wine %>% 
  select(!(color)) %>% 
  scale(center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(wine_scaled,"scaled:center")
sigma = attr(wine_scaled,"scaled:scale")

distance_between = dist(wine_scaled)

h1 = hclust(distance_between, method='average')

# Cut the tree into 10 clusters
cluster1 = cutree(h1, k=2)
summary(factor(cluster1))

# Plot the dendrogram
plot(h1, cex=0.3)


# Using kmeans++ initialization
clust2 = kmeanspp(wine_scaled, k=2, nstart=25)

table(wine$color, factor(clust2$cluster))


wine_pca = prcomp(wine_scaled, rank=2, scale=TRUE)
summary(wine_pca)

loadings_summary = wine_pca$rotation %>%
  as.data.frame() %>%
  rownames_to_column('characteristic')

plot(wine_pca)

wine_merge = merge(wine, wine_pca$x, by="row.names")


# principal component regression: predicted engagement
lm1 = lm(color ~ PC1 + PC2, data=wine_merge)
summary(lm1)

# gross ratings points
lm2 = lm(GRP ~ PC1 + PC2 + PC3, data=shows)
summary(lm2)

# Conclusion: we can predict engagement and ratings
# with PCA summaries of the pilot survey.
# probably too much variance to regress on all survey questions!
# since the sample size isn't too large here.
plot(PE ~ fitted(lm1), data=shows)
plot(GRP ~ fitted(lm2), data=shows)