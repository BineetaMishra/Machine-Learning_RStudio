# Decision Tree Regression
#Polynomial Regression
#Importing the dataset
dataset = read.csv('Position_Salaries.csv')
#resetting the dataset to only the colmn that is needed
dataset = dataset[2:3]
#if d dataset is small no need to splt in test & train set
#Feature scaling is not required because a polynomial regression
#model is a multiple regression model with polynomial terms

#Building the regression model
#Fitting Decission Tree regression to the dataset
install.packages('rpart')
library(rpart)
regressor = rpart(formula = Salary ~.,
                  data= dataset,
                  control = rpart.control(minsplit = 1))

#Predicting a new result with Linear Regression
y_pred = predict(regressor, data.frame(Level=6.5))


#Visualise the Decision Tree regression Dataset
#install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             color='red')+
  geom_line(aes(x=dataset$Level, y=predict(regressor, newdata = dataset)),
            color='Blue')+
  ggtitle('True vs Bluff(Decision Tree)')+
  xlab('Level')+
  ylab('Salary')

#the model is not contionouis in DTR non linear and non contious
#for non linear and non contious take higher resoulition
#Visualise the Decision Tree regression Dataset(higher resolution)
#HIGHER Resolution
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