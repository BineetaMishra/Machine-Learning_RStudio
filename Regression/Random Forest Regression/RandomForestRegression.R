#Random Forest Regression
#It is a team of decision trees, each one making some prediction of the dependent variable
#and ultimate prediction of random forest is the avg of the diff prediction
#of all the diff trees of the forest.

#Importing the dataset
dataset = read.csv('Position_Salaries.csv')
#resetting the dataset to only the colmn that is needed
dataset = dataset[2:3]

#Building the regression model
#Fitting Random Forest regression to the dataset
install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor = randomForest(x=dataset[1], y=dataset$Salary,
                         ntree = 10)

#Predicting a new result with Linear Regression
y_pred = predict(regressor, data.frame(Level=6.5))

#Visualise the Random Forest Regression Dataset. 
#Higher Degree... Thus more accuracy
#install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot()+
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             color='red')+
  geom_line(aes(x= x_grid, y=predict(regressor, newdata = data.frame(Level = x_grid))),
            color='Blue')+
  ggtitle('True vs Bluff(Decision Tree)')+
  xlab('Level')+
  ylab('Salary')