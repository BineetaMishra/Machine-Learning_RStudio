# Regression Template
#Importing the dataset
dataset = read.csv('Position_Salaries.csv')
#resetting the dataset to only the colmn that is needed
dataset = dataset[2:3]
#if d dataset is small no need to splt in test & train set
#Feature scaling is not required because a polynomial regression
#model is a multiple regression model with polynomial terms

#Building the regression model
#Fitting Linear regression to the dataset
'Create your regressor here'

#Predicting a new result with Linear Regression
y_pred = predict(lin_reg, data.frame(Level=6.5))

#Fitting Polynomial regression to the dataset
#Adding the polynomial features
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
poly_reg = lm(formula = Salary ~., data=dataset)

# Visualise the Linear regression Dataset
#install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             color='red')+
  geom_line(aes(x=dataset$Level, y=predict(lin_reg, newdata = dataset)),
            color='Blue')+
  ggtitle('True vs Bluff(Linear Regression)')+
  xlab('Level')+
  ylab('Salary')

#Visualise the Polynomial regression Dataset
#install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             color='red')+
  geom_line(aes(x=dataset$Level, y=predict(regressor, newdata = dataset)),
            color='Blue')+
  ggtitle('True vs Bluff(Polynomial Regression)')+
  xlab('Level')+
  ylab('Salary')
#Visualise the Polynomial regression Dataset. 
#Higher Degree... Thus more accuracy
#install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x=dataset$Level3, y=dataset$Salary),
             color='red')+
  geom_line(aes(x=dataset$Level3, y=predict(regressor, newdata = dataset)),
            color='Blue')+
  ggtitle('True vs Bluff(Plynomial Regression)')+
  xlab('Level')+
  ylab('Salary')

