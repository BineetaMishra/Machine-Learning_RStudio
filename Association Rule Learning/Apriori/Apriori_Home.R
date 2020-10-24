#Apriori 
# Transforming the dataset into a Sparse MAtrix
# Creating Sparse Matrix

install.packages('arules')
library(arules)
dataset= read.csv('Market_Basket_Optimisation.csv', header = FALSE)
dataset = read.transactions('Market_Basket_Optimisation.csv', sep= ',', rm.duplicates =  TRUE)
# in console 1 5 means 5 transaction containing 1 duplicate
summary(dataset)
# frquency plot of the top 10 diff products bought at the store
itemFrequencyPlot(dataset, topN=10)

#Training Apriori on the dataset
rules = apriori(data = dataset, parameter = list(support= 0.003, confidence= 0.2))
# support (3*7)/(7500) = 0.003, 
#Confidence = arbitrary choice, put it then inc or dec accordingly, default= 0.8

# Visualizing the result, sort the rules by the lift
inspect(sort(rules, by = 'lift')[1:10])



