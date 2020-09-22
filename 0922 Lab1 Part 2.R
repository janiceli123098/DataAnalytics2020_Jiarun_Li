# clear all objects from the workspace
rm(list = ls())

# import libraries
library(dplyr)

# read the EPI data
data <- read.csv("EPI_data.csv")
attach(data)

# Exercise 1 : fitting a distribution beyond histograms
# cumulative density function
plot(ecdf(EPI), do.points = FALSE, verticals = TRUE)

# Quantile-Quantile
help("qqnorm")

par(pty="s")
qqnorm(EPI)
qqline(EPI)

# Make a Q-Q plot against the generating distribution 
x <- seq(30,95,1)
x

qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

# Try fitting other distributions
qqplot(EPI, DALY)


# comparing distributions
boxplot(EPI, DALY)

# Linear regression
multivariate <- read.csv("multivariate.csv")
attach(multivariate)

mm <- lm(Homeowners~Immigrant)
mm

summary(mm)$coef

plot(Homeowners~Immigrant)

help(abline)

abline(mm)

abline(mm, col=2, lwd=3)

newImmigrantdata <- data.frame(Immigrant = c(0,  20))
mm %>% predict(newImmigrantdata)

abline(mm)
abline(mm,col=3,lwd=3) # line color = green, line width = 3
attributes(mm)
mm$coefficients

# In-class work: ggplot examples
# creating plots
# Chapter 2 - R Graphics Cookbook

library(ggplot2)

plot(mtcars$wt, mtcars$mpg)

qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data=mtcars)
ggplot(mtcars, aes(x=wt, y=mpg))+geom_point()

plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature, pressure$pressure/2, col="blue")

qplot(pressure$temperature, pressure$pressure, geom="line")
qplot(temperature, pressure, data = pressure, geom = "line")

ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()

# creating Bar graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)

barplot(table(mtcars$cyl))

qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))

# Bar graph of counts
qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

# creating histograms using ggplot
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)

qplot(mpg, data = mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=5)

# creating Box plots using ggplot
# creating box plot
plot(ToothGrowth$supp, ToothGrowth$len)

# formula syntax
boxplot(len~supp, data = ToothGrowth)
boxplot(len~supp+dose, data = ToothGrowth)

qplot(supp, len, data = ToothGrowth, geom = "boxplot")
qplot(ToothGrowth$len, ToothGrowth$supp, geom = "boxplot")

ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()

qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")

ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()








