# Importing the data set
dataset= read.csv("Mall_Customers.csv")

# creating x that will contain the two var (annual income and spending score)
x <- dataset[4:5]

# while using K means specify no. of clusters
# the "elbow method" will help find no. of clusters
# using the Elbow Method to find optimum no. of clusters
set.seed(6)
wcss <- vector()
for (i in 1:10) wcss[i] <- sum(kmeans(x,i)$withinss)
# plot the graph
plot(1:10, wcss, type="b", main=paste('Clusters of clients',
                                      xlab='no.of clusters',
                                      ylab='wcss'))
# Applying dataset to the mall dataset
set.seed(29)
kmeans <- kmeans(x, 5, iter.max = 300, nstart = 10)

# Visualising the cluster
library(cluster)
clusplot(x, kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = 'Clusters of Clients',
         xlab = 'Annual Income',
         ylab = 'WCSS')
