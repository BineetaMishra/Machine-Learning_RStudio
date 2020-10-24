# RANDOM FOREST CLASSIFICATION
# Importing the dataset
dataset= read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

#Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0,1))

#Splitting the dataset into training and test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split= sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set= subset(dataset, split== TRUE)
test_set= subset(dataset, split== FALSE)

#Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

#Fitting classifier to the training set
#install.packages('randomForest')
library(randomForest)
classifier = randomForest(x= training_set[-3], 
                          y= training_set$Purchased,
                          ntree=10)

# Predicting the test set result
y_pred= predict(classifier, test_set[-3])
#y_pred will be in the type/form of probability
# to convert y_pred to vector use "type='class'",
#then it will be 0 or 1

# Making the confusion matrix
cm=table(test_set[, 3], y_pred)

#Visualize the Training set results graphically
#install.packages('ElemStatLearn') #installed manually
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata= grid_set)
plot(set[, -3],
     main = 'Random Forest Classification (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))  ## if error comes put --> par(mar=c(1,1,1,1)) in console
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'yellow', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata= grid_set)
plot(set[, -3],
     main = 'Random Forest Classification(Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

