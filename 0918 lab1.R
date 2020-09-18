# clear all objects from the workspace
rm(list = ls())

# read the csv file 
data <- read.csv("multivariate.csv")

attach(data)

names(data)

mm <- lm(Homeowners~Immigrant)
mm

# Create some scatterplots
plot(Income, Immigrant, main = "Scatterplot")
plot(Immigrant, Homeowners)

abline(mm)
abline(mm, col=2, lwd=3)

summary(mm)

attributes(mm)
mm$coefficients

help(abline)

HP <- Homeowners/Population
PD <- Population/area

mm <- lm(Immigrant~Income+Population+HP+PD)
summary(mm)

cm <- coef(mm)
cm











