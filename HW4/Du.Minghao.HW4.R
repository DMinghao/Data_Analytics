# BU.510.650	      Homework #4
# Data Analytics	  Page 2 of 2
# Spring 2021	      The Johns Hopkins University
# Arnab Bisi	      Carey Business School

library(ISLR)
library(glmnet)

############  Question 1  ############
# In this question, you will use the LASSO method on the AutoLoss data set from
# Assignment 3. Our goal is to predict the losses paid by an insurance company as
# a function of the predictors, which are several features of a vehicle.

AutoLoss <-
  read.csv("AutoLoss.csv",
           na.strings = "?",
           stringsAsFactors = T)
AutoLoss <- na.omit(AutoLoss)

# (a)	Fit a LASSO model to the AutoLoss data set, using Losses as the response
# (output variable) and all other variables as predictors (input variables),
# for lambda values ranging from 1 to 10 in increments of 1. (See, in particular,
# the code we used to perform Task 1 and Task 3 on Slide 36 in Session 3, when we
# applied LASSO to Hitters data set.) State the coefficient estimates you obtained
# for lambda = 1 and lambda = 10.

x <- model.matrix(Losses ~ ., data = AutoLoss)[, -1]
y <- AutoLoss$Losses

head(x)
head(y)

lasso.1.10 <- glmnet(x, y, alpha = 1, lambda = c(1:10))
lasso.1.10.co <- coef(lasso.1.10)

lasso.1.10.co[, 1]

lasso.1.10.co[, 10]


############  Question 2  ############
# In this question, you will continue to use the LASSO method on the AutoLoss data set.
# This time, you will use 5-fold cross-validation to find the best value for lambda. (This
# follows up on the last question, where you ran LASSO for lambda values ranging from 1 to 10 in
# increments of 1. Now, you will be determining the best possible value for lambda. For guidance,
# see the code we used to perform Tasks 8 and 10 on Slide 41 in Session 3.)

# (a)	Fit a LASSO model to the AutoLoss data set, using Losses as the response and all other
# variables as predictors, using 5-fold cross-validation on the entire data to find the best
# value for lambda. (R Hints: To make it a five-fold cross-validation, include nfolds=5 as an argument
# inside cv.glmnet(). **Remember to include set.seed(1) before cv.glmnet(), so we all end up making
# the same split.**) State the best value for lambda.

set.seed(1)
cv.out <- cv.glmnet(x,
                    y,
                    alpha = 1,
                    nfolds = 5,
                    lambda = c(10:1))
cv.out
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

# (b)	Run LASSO one more time, this time using the best value for lambda. State the coefficient
# estimates you obtained. What predictors are included in the resulting model?

lasso.final <- glmnet(x, y, alpha = 1, lambda = bestlam)

coef(lasso.final)


############  Question 3  ############
# Suppose that we collect data for a group of students at the Johns Hopkins Carey Business
# School who recently took the written test and the road test in their driver's license
# applications. The data include written test score (X1), hours of supervised practice driving
# (X2) and the road test results (Y, can be pass or fail). We want to predict the probability
# of passing the driving test based on the written test score (X1) and hours of supervised practice
# driving (X2). After running the logistic regression, we obtain the coefficients:
beta0hat <- -1.2
beta1hat <- 0.02
beta2hat <- 0.01

# (a) Suppose that Sam will take the road test next week. His written test score is 25; he practiced
# 50 hours of supervised driving. Estimate the probability that Sam will pass the road test.

passProb <-
  function(test, practice)
    return((exp(1) ^ (
      beta0hat + test * beta1hat + practice * beta2hat
    )) / (1 + exp(1) ^ (
      beta0hat + test * beta1hat + practice * beta2hat
    )))

p <- passProb(25, 50)
p

# (b)	Another student Shengqi just passed the written exam with score 24. How many hours should he
# practice to have 50% chance of passing the road test? (2 points)

uniroot(
  function(test, practice)
    return(passProb(test, practice) - 0.5),
  c(0, 100),
  tol = 1e-8,
  extendInt = "yes",
  test = 24
)$root

############  Question 4  ############
# (a) For the Smarket data set from "ISLR" library, using year 2005 data as test data and
# remaining as training data, what is your prediction accuracy for market Direction if Lag 1,
# Lag 2, Lag 3, Lag 4, Lag 5 and Volume are used as predictor variables? (1 point)

train <- (Smarket$Year != 2005)
Smarket.2005 <- Smarket[!train,]
Direction.2005 <- Smarket$Direction[!train]
glm.fit <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  family = binomial,
  data = Smarket,
  subset = train
)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
mean(glm.pred == Direction.2005)


# (b) For the same setting, if you use only Lag 1 and Lag 2 as predictor variables, what is
# your prediction accuracy for market Direction? (1 point)

glm.fit <- glm(
  Direction ~ Lag1 + Lag2,
  family = binomial,
  data = Smarket,
  subset = train
)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
mean(glm.pred == Direction.2005)

