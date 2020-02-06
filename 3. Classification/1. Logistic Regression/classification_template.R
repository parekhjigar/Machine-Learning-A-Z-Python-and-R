# Classification Template

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]

# Splitting the dataset into Training and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature scaling
training_set[, 1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale(test_set[, 1:2])

# Fitting the classifier to the Training Set
# Create your classifier here

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

# Visualising the Training set results
# install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_pred = predict(classifier, type = "response", newdata = grid_set)%>% {ifelse(.>0.5, 1, 0)}
BG_color = ifelse(y_pred == 1, 'palegreen', 'salmon')
point_fill = ifelse(set$Purchased == 1,'green4', 'red3')

ggplot() +
  geom_point(data = grid_set, aes(x = Age, y = EstimatedSalary), color = BG_color) +
  ggtitle(label = 'Classifier (Training set)') +
  geom_point(data = set, aes(x = Age, y = EstimatedSalary), size = 1, color = point_fill) +
  geom_point(data = set, aes(x = Age, y = EstimatedSalary), size = 1, shape = 1)

# Visualising the Test set results
# install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_pred = predict(classifier, type = "response", newdata = grid_set)%>% {ifelse(.>0.5, 1, 0)}
BG_color = ifelse(y_pred == 1, 'palegreen', 'salmon')
point_fill = ifelse(set$Purchased == 1,'green4', 'red3')

ggplot() +
  geom_point(data = grid_set, aes(x = Age, y = EstimatedSalary), color = BG_color) +
  ggtitle(label = 'Classifier (Test set)') +
  geom_point(data = set, aes(x = Age, y = EstimatedSalary), size = 1, color = point_fill) +
  geom_point(data = set, aes(x = Age, y = EstimatedSalary), size = 1, shape = 1)