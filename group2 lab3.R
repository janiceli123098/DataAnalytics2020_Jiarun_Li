# clear all objects from the workspace
rm(list = ls())

# Regression Tree
# import required libraries
library(rpart)
library(rpart.plot)

data("msleep")
str(msleep)
help("msleep") 

# creating a new data frame with the following columns included.
mSleepDF1 <- msleep[,c(3,6,10,11)] # 3 = vore ,6=sleep_total, 10=brainwt, 11=bodywt
# observe the structure of the mSleepDF 
str(mSleepDF1)
head(mSleepDF1)

# Building Regression Decision Tree that #predicts the total sleeping
# hours of the mamals based on the other #variables available on the dataset
help("rpart") # Read the documentation for the rpart() function. 
sleepModel_1 <- rpart(sleep_total ~ ., data=mSleepDF1, method = "anova")
# method we are using here is anova becuase our target here is sleep_total is a numerical one.
sleepModel_1

help("rpart.plot")
rpart.plot(sleepModel_1, type = 3, fallen.leaves = TRUE)
# type = 3, Draw separate split labels for the left and right directions. See the documentation 
#fallen.leaves = TRUE,  Default TRUE to position the leaf nodes at the bottom of the graph. 
#It can be helpful to use FALSE if the graph is too crowded and the text size is too small.
rpart.plot(sleepModel_1, type = 3,digits = 3, fallen.leaves = TRUE) # with 3 digits 
rpart.plot(sleepModel_1, type = 3,digits = 4, fallen.leaves = TRUE)

# Classification Tree
# instrall the C50 package
# install.packages("C50")
require(C50)
# we will be using the iris dataset to do a #classfication
data("iris")
head(iris)
str(iris)
table(iris$Species)

# set the seed in order to get the same randomly generated numbers when we run the model
# set the seed
set.seed(1984)
# generate random numbers
grn <-runif(nrow(iris))

# creating a randomized iris dataset ,  shuffling the dataset
# we use the order() function along with the #random numbers we generated.
irisrand <-iris[order(grn),]

# observe that rows are now randomly shuffled.
str(irisrand)
classificationmodel1 <-C5.0(irisrand[1:100,-5], irisrand[1:100,5])
classificationmodel1
summary(classificationmodel1)

# now we will do the prediction using the #predict() function
# We are using the remaining last 50 rows for #here starting from 101 row to 150th row
prediction1 <- predict(classificationmodel1,irisrand[101:150,])
prediction1

# we will use the confusion matrix to #understand our prediction
# Read the documentation for the table() function in RStudio help
table(irisrand[101:150,5],prediction1)
# you can write the same above line by defining what is the "predicted"
##  table(irisrand[101:150,5],Predicted = prediction1)

# We can plot the classification model tree #using the plot() function
plot(classificationmodel1)

# Naive Bayes Classifier 
## Call the NaiveBayes Classifier Package e1071, which auto calls the Class package ##
library("e1071")
classifier<-naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$Petal.Length
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green") 



























