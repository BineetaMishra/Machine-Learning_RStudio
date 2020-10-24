#importing the dataset
dataset = read.csv("Mall_Customers.csv")
x = dataset[4:5]

# using the dendrogram to find optimal no. of clusters
dendrogram =hclust(dist(x, method = 'euclidean'), method='ward.D')
plot(dendrogram,
     main=paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distance')

#Fitting the hierarchial clustering to the mall dataset
hc =hclust(dist(x, method = 'euclidean'), method='ward.D')
y_hc= cutree(hc, 5)

#Visualizing the clusters
library(cluster)
clusplot(x, y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = 'Clusters of Clients',
         xlab = 'Annual Income',
         ylab = 'WCSS')

##Error in plot.new() : figure margins too large
#par(mar=c(1,1,1,1))
