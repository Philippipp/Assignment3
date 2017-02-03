
#OTTo Challange 
library(caret)
library(ROCR)
library(tree)
library(randomForest)
library(nnet)
library(mlr)
#-----------------
#PART A 
#-----------------

setwd("C:/Users/Philmen/Desktop/MMU/Data Mining/Assignment/Part 3")


train <- read.csv(file = "OTTO CHallenge/train.csv", header =  T, sep = ",")
head(train)
summary(train$target)

names(train$target)
sum(is.na(train))

test <- read.csv(file = "OTTO CHallenge/test.csv", header = T, sep = ",")
summary(test)
length(test)


sample <- read.csv("OTTO CHallenge/sampleSubmission.csv", header = T, sep = ",")
head(sample)
summary(sample)

#-----------------
#PART B
#-----------------

# Remove id column so it doesn't get picked up by the current classifier
train <- train[,-1]


#-----------------
#PART C
#-----------------
# I would choose the Roc curfe Because its one of the best way to meassure the performance of a Classifikation task.
#AUROCC	Interpretation
#1.0	Perfect test
#0.9 to 0.99	Excellent test
#0.8 to 0.89	Food test
#0.7 to 0.79	Fair test
#0.51 to 0.69	Poor test
#0.5	here is something wrong 

#-----------------
#PART D
#-----------------

task = makeClassifTask( data = train, target = "target")
task

#-----------------
#Neuronal Networks 
#-----------------
lrn1 = makeLearner("classif.nnet",  predict.type = "prob")
mod1 = train(lrn1, task)
pred1 = predict(mod1, newdata = test)
head(pred1)
performance(pred1,  measures = tpr)

#-----------------
#Naiv Bayes 
#-----------------

lrn2 = makeLearner("classif.naiveBayes",  predict.type = "prob")
mod2 = train(lrn2, task)
pred2 = predict(mod2, newdata = test)
head(pred2)
performance(pred2,  measure= tpr)


#-----------------
#Tree 
#-----------------

lrn3 = makeLearner("classif.rpart",  predict.type = "prob")
mod3 = train(lrn3, task)
pred3 = predict(mod3, newdata = test)
head(pred3)
performance(pred3,  measure= tpr)




#-----------------
#ROC 
#-----------------

#Tryed to make a Performance Meassure but allways problem with an error You need to have a 'truth' column in your pred object for
getDefaultMeasure(task)
getConfMatrix(pred2)


