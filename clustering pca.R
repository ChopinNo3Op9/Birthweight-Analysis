data(iris)
head(iris)

table(iris$Species)

par(mfrow = c(1,2))
plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species)
plot(iris$Petal.Length, iris$Petal.Width, col = iris$Species)

clustering <- kmeans(iris[,1:4], 2) # if clustering based on all fators, not good
par(mfrow = c(1,2))
plot(iris$Petal.Length, iris$Petal.Width, col = iris$Species)  # real
plot(iris$Petal.Length, iris$Petal.Width, col = clustering$cluster)

# scale the covariates
iris1 <- iris
for (i in 1:4) {
  iris1[,i] <- iris1[,i] - mean(iris1[,i])
  iris1[,i] <- iris1[,i]/sd(iris1[,i])
}

mean(iris1[,3])
sd(iris1[,3])

plot(iris$Petal.Length, iris$Petal.Width, col = clustering$cluster)

clustering <- kmeans(iris1[,1:4], 3)
par(mfrow = c(1,2))
plot(iris$Petal.Length, iris$Petal.Width, col = iris$Species)
plot(iris$Petal.Length, iris$Petal.Width, col = clustering$cluster)  # not good

# princinpal component analysis
# transforming the original variables into a new set of uncorrelated variables (principal components).
res <- prcomp(iris[,1:4])
names(res)

par(mfrow = c(1,1))
plot(res)
summary(res)

# "Elbow rule"
res <- prcomp(iris[,1:4], scale = T)
names(res)

par(mfrow = c(1,1))
plot(res)
summary(res)

res$sdev^2

names(res)
res$rotation

# Let's focus on the first principle component
rotation <- res$rotation[,1]
iris1 <- iris
for (i in 1:4) {
  iris1[,i] <- iris1[,i] - mean(iris1[,i])
  iris1[,i] <- iris1[,i]/sd(iris1[,i])
}

pc1 <- rep(NA, nrow(iris1))
for (i in 1:nrow(iris1)) {
  pc1[i] <- sum(iris1[i,1:4]*rotation)
}

pc2 <- rep(NA, nrow(iris1))
for (i in 1:nrow(iris1)) {
  pc2[i] <- sum(iris1[i,1:4]*rotation)
}

pc <- as.matrix(iris1[,1:4]) %*% res$rotation
head(pc)
head(res$x)

iris1[1,]
-0.521*0.9 - 0.269*1.016 - 0.58*1.336 - 0.565*1.311

head(res$x)

plot(pc1, pc2)

# visualization
library(ggplot2)
library(ggfortify)

plot(pc[,1], pc[,2], col = iris$Species)
autoplot(res, color = c(rep(1, 50), rep(2, 50), rep(3, 50)))

# Run k-means clustering on the dimension-reduced dataset
# Compare the clustering with the species covariate
# k means based on the first two principle components
clustering <- kmeans(cbind(res$x[,1], res$x[,2]), 3, nstart = 100)
autoplot(res, color = clustering$cluster)

# Plot PCA results
autoplot(pc, data = iris, colour = 'Species') +
  ggtitle("PCA Plot")

# Run k-means clustering on the dimension-reduced dataset
# Compare the clustering with the species covariate
# clustering <- kmeans(cbind(res$x[,1], res$x[,2]), 3, nstart = 100)

# Plot k-means clustering results
autoplot(pc, data = iris, colour = factor(clustering$cluster)) +
  ggtitle("K-means Clustering Plot")

pc1 <- res$x[,1]
pc2 <- res$x[,2]
par(mfrow = c(1,2))
plot(pc1, pc2, col = iris$Species)  # real
plot(pc1, pc2, col = clustering$cluster)  # predicted

