library(mvtnorm)
library(ggplot2)
mu1 = c(-1, 0)
mu2 = c(1, 0)
sigma1 = diag(0.4^2, 2)
sigma2 = diag(0.4^2, 2)


x1 = rmvnorm(250, mu1, sigma1)
x2 = rmvnorm(250, mu2, sigma2)
x = rbind(x1, x2)
plot(x)

# Run hierarchical clustering with single (min) linkage
# here min produces counterintuitive results
x_dist = dist(x)
h1 = hclust(x_dist, method='single') #single is minimum distance
c1 = cutree(h1, 2) #cut the hierarchical tree to give 2 clusters

D = data.frame(x, z = c1) #c1 is a cluster indicator (1 or 2)
ggplot(D) + geom_point(aes(x=X1, y=X2, col=factor(z)))

## note this poor result - min is highly susceptible to noise and outliers

# Run hierarchical clustering with complete (max) linkage
h2 = hclust(x_dist, method='complete') #complete is max function
c2 = cutree(h2, 2)
D2 = data.frame(x, z = c2)
ggplot(D2) + geom_point(aes(x=X1, y=X2, col=factor(z)))


# Run hierarchical clustering with average linkage
h3 = hclust(x_dist, method='average')
c3 = cutree(h3, 2)
D3 = data.frame(x, z = c3)
ggplot(D3) + geom_point(aes(x=X1, y=X2, col=factor(z)))

## average doesn't have many downsides until the dataset gets unreasonable large


# But here's a different example where max produces counterintuitive results
set.seed(84958)
mu1 = c(-1, 0)
mu2 = c(1, 0)
sigma1 = diag(0.1^2, 2)
sigma2 = diag(0.45^2, 2)


x1 = rmvnorm(250, mu1, sigma1)
x2 = rmvnorm(250, mu2, sigma2)
x = rbind(x1, x2)
plot(x)

# Run hierarchical clustering with single (min) linkage
x_dist = dist(x)
h1 = hclust(x_dist, method='single')
c1 = cutree(h1, 2)
D = data.frame(x, z = c1)
ggplot(D) + geom_point(aes(x=X1, y=X2, col=factor(z)))

## notice how min looks pretty good here

# Run hierarchical clustering with complete (max) linkage
h2 = hclust(x_dist, method='complete')
c2 = cutree(h2, 2)
D2 = data.frame(x, z = c2)
ggplot(D2) + geom_point(aes(x=X1, y=X2, col=factor(z)))

## now max is getting some counterintuitve results 
## a downside of max is large and obvious clusters can get broken up

# Run hierarchical clustering with average linkage
h3 = hclust(x_dist, method='average')
c3 = cutree(h3, 2)
D3 = data.frame(x, z = c3)
ggplot(D3) + geom_point(aes(x=X1, y=X2, col=factor(z)))

## average can perform like the better of the two in either case

## you could also try centroid
h4 = hclust(x_dist, method='centroid')
c4 = cutree(h4, 2)
D4 = data.frame(x, z = c4)
ggplot(D4) + geom_point(aes(x=X1, y=X2, col=factor(z)))
