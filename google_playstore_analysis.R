
install.packages("ggalt")
library(tidyverse) 
library(ggalt)
library(gridExtra) 
library(broom) 
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(ggplot2)
library(lattice)
library(caret)
install.packages("C50")
library(C50)
library(kernlab)
library(mlbench)
library(randomForest)
library(caretEnsemble)
library(MASS)
library(klaR)
library(nnet)
f2<-read.csv("googleplaystore.csv")

str(f2)
View(f2)
##For family type
f3<-f2[c(2 ,3,4,6)]
na.omit(f3)

f4=subset(f3, f2$Category=="FAMILY")
f4
f4$Reviews<-as.numeric(f4$Reviews)
f4$Installs<-as.numeric(f4$Installs)
#mean and median
summary(f4$Installs)
summary(f4$Reviews)
#standard deviation
sd(f4$Reviews)
sd(f4$Installs)
#mode
mode<-(as.numeric(f4$Reviews))
temp<-table(as.vector((mode)))
names(temp)[temp==max(temp)]
mode<-(as.numeric(f2$Installs))
temp<-table(as.vector((mode)))
names(temp)[temp==max(temp)]
#for Business type apps
f5=subset(f3, f2$Category=="BUSINESS")
f5
f5$Reviews<-as.numeric(f5$Reviews)
f5$Installs<-as.numeric(f5$Installs)
#mean and median
summary(f5$Installs)
summary(f5$Reviews)
#standard deviation
sd(f5$Reviews)
sd(f5$Installs)
#mode
mode<-(as.numeric(f5$Reviews))
temp<-table(as.vector((mode)))
names(temp)[temp==max(temp)]
mode<-(as.numeric(f5$Installs))
temp<-table(as.vector((mode)))
names(temp)[temp==max(temp)]
# the attribute installs have smaller standard deviation for the business category.

ggplot(f4, aes(x=f4$Rating, y=f4$Installs, fill=f4$Rating))+geom_bar(stat="identity")
ggplot(f5, aes(x=f5$Rating, y=f5$Installs, fill=f5$Rating))+geom_bar(stat="identity")

#This graph projects a very interesting trend, The number of installs increase with ratings but after reaching a rating of 4 its starts to fall, then there is a slight increase when the rating is 5.
ggplot(f4, aes(x=f4$Reviews, y=f4$Installs, fill=f4$Reviews))+geom_bar(stat="identity")
ggplot(f5, aes(x=f5$Reviews, y=f5$Installs, fill=f5$Reviews))+geom_bar(stat="identity")

# The category 'free' has most number of installs.

str(f2)
f2$Category<-is.numeric(f2$Category)

# Separate the dataset into 25% training data and 75% test data.  Remove the class attribute values from the test data set.
f22<- sample(2, nrow(f2), replace=TRUE, prob=c(0.75, 0.25))
testdata <- f2[f22==1,]
traindata <- f2[f22==2,]

f2<-na.omit(f2)
x=f2[,c(2,8)]

x[,1]=(x[,1]-mean(x[,1]))/sd(x[,1])
x[,2]=(x[,2]-mean(x[,2]))/sd(x[,2])

#Then prepare the training data to run with two of the classifiers below.  
#naive bayes
library(neuralnet)
library(ggplot2)
library(nnet)
library(dplyr)
library(reshape2)
f23 <- f2[1:100, c(2,3,4,5,6)]
str(f23)
set.seed(123)
# Converting RESULT into one vector.
labels <- (as.factor(f23$Installs))
standardizer <- function(x){(x-min(x))/(max(x)-min(x))}
f23[, 1:4] <- lapply(f23[, 1:4], standardizer)
f23  
# Normalizing /standardizing the predictors
pre_process_data <- cbind(f23[,c(1:4)], labels)
pre_process_data
library(e1071)
o<-naiveBayes(Installs~Category+Rating,data = f2)
o
str(o)
nb_predict <- predict(o,f2)
table(f2$Installs,nb_predict)
#Random Forest
f2$Installs<-as.numeric(f2$Installs)
f22<- sample(2, nrow(f2), replace=TRUE, prob=c(0.75, 0.25))
testdata <- f2[f22==1,]
trainData <- f2[f22==2,]

library(randomForest)
na.omit(f2$Installs)
rf<-randomForest(Installs~Category+Rating,data = f2, ntree=50,proximity=TRUE)

table(predict(rf), trainData$Installs)
print(rf)
attributes(rf)

plot(rf)

importance(rf)
varImpPlot(rf)
Pred <- predict(rf, newdata=testdata)
table(Pred, testdata$Installs)
plot(margin(rf, testdata$Installs))

# Depending on the model size we can determine the algorithm we may use. Naive Bayes is simple calculation model and hence it cannot represnt complex behaviour. However, when your data is dynamic and keeps changing. NB can adapt quickly to the changes and new data.
# Random forest can be used to represent complex behaviour as random forest model size is very large and if not carefully built, it results in over fittting.


# Removing the class attribute and running two of the x clustering algorithms below.  Producing a biplot to identify attribute-to-attribute clusters

#kmeans
install.packages("h2o") 
install.packages("cluster")
install.packages("fpc") 

f24 <- f2[1:100,c(2,3,4,5,7)]
f24[sapply(f24, is.factor)] <- lapply(f24[sapply(f24, is.factor)], as.numeric)

is.na(f24)
sum(is.na(f24))
f2 <- na.omit(f2)
sum(is.na(f2))
#f2$suicides.100k.pop <- NULL
str(f2)
kmeans.result <- kmeans(f24, 4)
print(kmeans.result)
kmeans.result$cluster

table(f2$Installs, kmeans.result$cluster)


plot(f2[c("Category", "Installs")], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Category", "Installs")], col = 1:3, pch = 8, cex=2)

# Hierarchical
# We apply hierarchical clustering to the data.
A <- read.csv("googleplaystore.csv")

idx <- sample(1:dim(A)[1], 40)
ASample <- A[idx,]
ASample$Installs <- NULL
hc <- hclust(dist(ASample), method="ave")

plot(hc, hang = -1, labels=A$Installs[idx])

# Cutting the tree into 4 clusters
rect.hclust(hc, k=4)
groups <- cutree(hc, k=4)
