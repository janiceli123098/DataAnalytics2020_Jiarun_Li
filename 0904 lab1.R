library(MASS)

attach(Boston)

?Boston

help("Boston")

head(Boston)

dim(Boston)

names(Boston)

str(Boston)

nrow(Boston)

ncol(Boston)

summary(Boston)

summary(Boston$crim)

### _____________________________________________ ###

library(ISLR)

data("Auto")

head(Auto)

?Auto

head(Auto,10)

names(Auto)

summary(Auto)

fivenum(Auto$mpg)

boxplot(Auto$weight)

mean(Auto$weight)

median(Auto$weight)

### _____________________EPI data set________________________ ###
data <- read.csv("EPI_data.csv")

# data <- read.csv(file.choose(), header = TRUE)

names(data)

dim(data)

str(data)

summary(data$EPI)

library(plotly)







