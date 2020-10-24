#SimpleLinearRegression

#Importing the dataset
dataset = read.csv('Salary_Data.csv')

#Splitting d dataset to Training & Tst
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set= subset(dataset, split== TRUE)
test_set = subset(dataset, split== FALSE)

#Fitting Simple Linear regression to training set
regressor = lm(formula = Salary ~ YearsExperience, data=training_set)

# Predicting the test set result
y_pred = predict(regressor, newdata = test_set)

# Visualise the Training set
#install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x=training_set$YearsExperience, y=training_set$Salary),
             color='red')+
  geom_line(aes(x=training_set$YearsExperience, y=predict(regressor, newdata = training_set)),
             color='Blue')+
  ggtitle('Salary vs Experience(Training_Set)')+
  xlab('Years of Experience')+
  ylab('Salary')
            

# Visualise the Test set
#install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x=test_set$YearsExperience, y=test_set$Salary),
             color='red')+
  geom_line(aes(x=training_set$YearsExperience, y=predict(regressor, newdata = training_set)),
            color='Blue')+
  ggtitle('Salary vs Experience(Test_Set)')+
  xlab('Years of Experience')+
  ylab('Salary')

==============================================
#MultipleLinearRegression       \\\\\\\\\\\\\\
================================================
  
#Importing the dataset
dataset = read.csv('50_Startups.csv')

# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

#Splitting d dataset to Training & Tst
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set= subset(dataset, split== TRUE)
test_set = subset(dataset, split== FALSE)

#Fitting Multiple Linear regression to training set (linear combo of all independent variable)
regressor = lm(formula = Profit ~ R.D.Spend + Administration + 
                 Marketing.Spend + State,data=training_set)
# formula = Profit ~ . (this code is same as the abve, R 
#understand dot(.))
# the lower the P value is the more impact will the dependent var
#hv on d independent var. Check P value down in console in summary

# Predicting the test set result
y_pred = predict(regressor, newdata = test_set)


#Homework.
regressor = lm(formula = Profit ~ R.D.Spend + Administration,data= dataset)