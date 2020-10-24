dataset = read.csv('Wine.csv')
#dataset = dataset[, 3:5] as all data is needed.

# Splitting the dataset into training and test
#install.packages('caTools')
library(caTools)
set.seed(123)
split= sample.split(dataset$Customer_Segment, SplitRatio = 0.75)
training_set = subset(dataset, split== TRUE)
test_set= subset(dataset, split== FALSE)

#FEATURE Scaling Exclude the last column Customer_segment
training_set[, -14] =scale(training_set[, -14]) 
test_set[, -14] = scale(test_set[, -14])

#Applying PCA
install.packages('caret')
library(caret)
# install.packages('e1071')
library(e1071)
pca = preProcess(x=training_set[-14], method = 'pca', pcaComp = 2)
training_set = predict(pca, training_set)
training_set = training_set[c(2,3,1)]
test_set = predict(pca, test_set)
test_set = test_set[c(2,3,1)]

#Fitting the SVM model
library(e1071)
Classifier= svm(formula= Customer_Segment ~.,
                data= training_set,
                type = 'C-classification',
                kernel = 'linear')

#Predicting the test result
y_pred= predict(Classifier, test_set[-3])

#Making the confusion Matrix
cm = table(test_set[, 3], y_pred)

#Visualising the training set result

library(ElemStatLearn)
set = training_set
x1= seq(min(set[,1]) -1, max(set[, 1]) +1, by=0.01)
x2= seq(min(set[,2]) -1, max(set[, 2]) +1, by=0.01)
grid_set= expand.grid(x1,x2)
colnames(grid_set) = c('PC1','PC2')
y_grid= predict(Classifier, newdata= grid_set)
plot(set[, -3],
     main= 'PCA(Training SET)',
     xlab= 'PC1', ylab = 'PC2',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add=TRUE)
points(grid_set, pch='.', col= ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen', 'tomato')))
points(set, pch= 21, bg= ifelse(set[, 3] == 2, 'blue3',ifelse(set[, 3] == 1, 'green', 'red')))

#Visualising the test set result

library(ElemStatLearn)
set = test_set
x1= seq(min(set[,1]) -1, max(set[, 1]) +1, by=0.01)
x2= seq(min(set[,2]) -1, max(set[, 2]) +1, by=0.01)
grid_set= expand.grid(x1,x2)
colnames(grid_set) = c('PC1','PC2')
y_grid= predict(Classifier, newdata= grid_set)
plot(set[, -3],
     main= 'PCA(TEST SET)',
     xlab= 'PC1', ylab = 'PC2',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add=TRUE)
points(grid_set, pch='.', col= ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen', 'tomato')))
points(set, pch= 21, bg= ifelse(set[, 3] == 2, 'blue3',ifelse(set[, 3] == 1, 'green', 'red')))
