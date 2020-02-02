rd <- read.csv(file.choose(),header = T)
str(rd)

scaled.dat <- scale(rd[,-1])
View(scaled.dat)

scaled.dat<- as.data.frame(cbind(rd[,1],scaled.dat))
names(scaled.dat)

set.seed(20)
cluster_output <- kmeans(scaled.dat[,-1], 5)

wss <- (nrow(scaled.dat[,-1])-1)*sum(apply(scaled.dat[,-1],2,var))

for (i in 2:15) wss[i] <- sum(kmeans(scaled.dat[,-1],centers=i)$withinss)
plot(1:15, wss, type='b', xlab='Number of Clusters', ylab='Within groups sum of squares',col='mediumseagreen',pch=12)

str(cluster_output)
cluster_output$centers

cluster_output

names(cluster_output)

library(ggplot2)
