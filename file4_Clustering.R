#association rule-----
library(arules)
library(arulesViz)


#clustering-----
ibrary(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(fpc) 
library(NbClust) # finding the optimal number of clusters
library(amap) 


set.seed(1234)
subject1 = trunc(rnorm(30, mean=60, sd=15))
range(subject1)
subject1
marks = data.frame(subject1)
head(marks)
marks
sort(marks$subject1)
k2 = kmeans(marks, centers=2)
k2
k2$size
k2$iter
cbind(marks, k2$cluster) #which data row in to which cluster
length(marks[k2$cluster==1,])
marks[k2$cluster==2,]
marks[k2$cluster==1,]
k2$centers

k2a = kmeans(marks, centers=c(50,70))
k2a
k2a$centers

#optimal number of clusters in data
#Reduce total within as

iris
dim(iris)
head(iris)
table(iris$Species)

data = iris[-5]
head(data)

km1 = kmeans(data, centers=1)
km1$withinss
km1$tot.withinss

km2 = kmeans(data, centers=2)
km2$tot.withinss
km2$withinss

km3 = kmeans(data, centers=3)
km3$tot.withinss

km4 = kmeans(data, centers=4)
km4$tot.withinss

km1$tot.withinss; km2$tot.withinss; km3$withinss; km4$withinss

library(cluster)

plot(data$sepal.Length,data$sepal.width,col=(1:3))
head(iris)

#plot1
library(cluster)
library(fpc)

data(iris)
data = iris[, -5]
km1 = kmeans(data, centers=3)
plotcluster(data, km1$cluster)

#plot2
#More Complex
clusplot(data, km1$cluster, color=TRUE, shade=TRUE, lables=2, lines=0 )
?clusplot

#plot3
with(iris, pairs(data, col=c(1:3)[km1$cluster]))



 
