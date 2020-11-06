library(ggplot2)
library(dplyr)
library(GGally)
library(rpart)
library(rpart.plot)
library(randomForest)
library(party)

data("Titanic")

dt <- rpart(Survived~.,
             data=Titanic,
             method="class")
plot(dt)
rpart.plot(dt, type=0, extra=2, cex=1.5)

rf <- randomForest(Survived~., data=Titanic, importance=TRUE,ntree=1000)
rf

ct <- ctree(Survived~., data=Titanic)
ct
plot(ct)



