library(foreign)
library(Hmisc)
library(psych)
library(magrittr)
library(dplyr)
library(ggpubr)
library(corrplot)
library(aod)
library(ggplot2)
library(tidyverse)
library(caret)

setwd("/Users/laytonwoohbill/Desktop/Depaul/2019-summer/Final Project")
MyData <- read.csv(file="data.csv", header=TRUE, sep=",")
MyData
head(MyData)
describe(MyData)
plot(MyData)
summary(MyData)
table(MyData$diagnosis)

levels(MyData$diagnosis)
factor2 <- factor(MyData$diagnosis)

#dummy
library(dummies)
MyData <- cbind(MyData, dummy(MyData$diagnosis, sep = "_"))

#plot
library(GGally)
library(ggplot2)
library(corrplot)
library(glmnet)


# Split the data into training and test set
set.seed(123)
smp_size<-floor(0.70*nrow(MyData))

train_ind <- sample(seq_len(nrow(MyData)), size = smp_size)

train <- MyData[train_ind, ]
test <- MyData[-train_ind, ]


#split data in to three group 
train.all<- train[,c(3:32)]
train.b<- train[,c(34)]
train.mean<- train[,c(3:12)]
train.se<- train[,c(34,13:22)]
train.worst<- train[,c(34,23:32)]
summary(train.all)

test.all<- test[,c(3:32)]
test.b<- test[,c(34)]
test.mean<- test[,c(3:12)]
test.se<- test[,c(34,13:22)]
test.worst<- test[,c(34,23:32)]


M<-cor(train.all, method="spearman")
M
corrplot(M, method = "square")
ggcorr(train.all)


fit.lasso = glmnet(as.matrix(train.all), train.b,alpha=1)
plot(fit.lasso)
plot(fit.lasso,xvar="lambda",label=TRUE)

fit.ridge = glmnet(as.matrix(train.all), train.b, alpha=0)
plot(fit.ridge)
plot(fit.ridge,xvar="lambda",label=TRUE)

# Find the best lambda using cross-validation
cv.lasso <- cv.glmnet(as.matrix(train.all),train.b, alpha=1, 
                      family = "binomial",type.measure="class",nfolds=10)
plot(cv.lasso)

# Display regression coefficients
coef(cv.lasso )

cv.lasso$lambda.min
cv.lasso$lambda.1se

coef(cv.lasso, cv.lasso$lambda.min)
coef(cv.lasso, cv.lasso$lambda.1se)


# Final model with lambda.min
lasso.model <- glmnet(as.matrix(train.all),train.b,alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.min)
lasso.model
coef(lasso.model)
# Make prediction on test data
x.test <- model.matrix(radius_mean ~., test.all)
x.test
probabilities <- predict(lasso.model,newx = x.test,s=NULL)
probabilities

predicted.classes <- ifelse(probabilities > 0.5, "B", "M")
predicted.classes 
# Model accuracy
observed.classes <- test$diagnosis
table(predicted.classes,observed.classes)
mean(predicted.classes == observed.classes)

# Fit the final model on the training data
model <- glmnet(as.matrix(train.all), train.b, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.1se)
model
coef(model)
# Make prediction on test data
y.test <- model.matrix(radius_mean ~., test.all)
y.test
probabilities_y <- predict(model,newx = y.test,s=NULL)

predict.classes <- ifelse(probabilities_y > 0.5, "B", "M")
predict.classes 
# Model accuracy
observed.classes <- test$diagnosis
table(predict.classes,observed.classes)
mean(predict.classes == observed.classes)



# Find the best lambda using cross-validation
cv.ridge <- cv.glmnet(as.matrix(train.all),train.b,family = "binomial", 
                      alpha=0,type.measure="class",nfolds=10)

coef(cv.ridge)

cv.ridge$lambda.min
cv.ridge$lambda.1se

coef(cv.ridge, cv.ridge$lambda.min)
coef(cv.ridge, cv.ridge$lambda.1se)

######

ridge.model <- glmnet(as.matrix(train.all),train.b,alpha = 0, family = "binomial",
                      lambda = cv.ridge$lambda.min)

ridge.model
coef(ridge.model)
# Make prediction on test data
a.test <- model.matrix(radius_mean ~., test.all)
a.test
probabilities_a <- predict(ridge.model,newx = a.test,s=NULL)

a_predict.classes <- ifelse(probabilities_a > 0.5, "B", "M")
a_predict.classes 
# Model accuracy
observed.classes <- test$diagnosis
table(a_predict.classes,observed.classes)
mean(a_predict.classes == observed.classes)


######
######
######
se_ridge.model <- glmnet(as.matrix(train.all),train.b,alpha = 0, family = "binomial",
                      lambda = cv.ridge$lambda.1se)

se_ridge.model
coef(se_ridge.model)
# Make prediction on test data
b.test <- model.matrix(radius_mean ~., test.all)
b.test
probabilities_b <- predict(se_ridge.model,newx = b.test,s=NULL)

b_predict.classes <- ifelse(probabilities_b > 0.5, "B", "M")
b_predict.classes 
# Model accuracy
observed.classes <- test$diagnosis
table(b_predict.classes,observed.classes)
mean(b_predict.classes == observed.classes)







