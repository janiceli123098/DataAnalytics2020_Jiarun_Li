# clear all objects from the workspace
rm(list = ls())

######## EPI dataset ############
# read the csv file 
EPI_data <- read.csv("EPI_data.csv")

# View the data in a new tab
View(EPI_data)

# set the default object
attach(EPI_data)

# launch a simple data editor？？？
fix(EPI_data)

# print out values EPI_data$EPI
EPI

# record True values if the value is NA
tf <- is.na(EPI)

# filter out NA values to create a new array
E <- EPI[!tf]
E

# stats
summary(EPI)

fivenum(EPI, na.rm = TRUE)

stem(EPI)

hist(EPI)

hist(EPI, seq(30.,95.,1.0),probability = TRUE)

lines(density(EPI, na.rm = TRUE, bw=1.))
#lines(density(EPI, na.rm = TRUE, bw=SJ))

rug(EPI)

# Exercise 1 : fitting a distribution beyond histograms
# cumulative density function
plot(ecdf(EPI), do.points=FALSE,verticals = TRUE)

# Quantile-quantile
par(pty='s')
qqnorm(EPI)
qqline(EPI)

# make a Q-Q plot against the generating distribution by
x <- seq(39,95,1)
qqplot(qt(point(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

# comparing distributions
boxplot(EPI, DALY)
qqplot(EPI, DALY)

help("distributions")

# Exercise 2 : filtering (popuplations)
# conditional filtering
EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), probability = TRUE)

# how to filter on EPI_regions or GEO_subregion
names(EPI_data)

table(EPI_data$EPI_regions)

EPI_South_Asia <- EPI[EPI_regions=="South Asia"]
EPI_South_Asia










