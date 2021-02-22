# BU.510.650	      Homework #1
# Data Analytics	  Page 2 of 2
# Spring 2021	      The Johns Hopkins University
# Arnab Bisi	      Carey Business School


library(ISLR)
library(MASS)
library(leaps)

############  Question 1  ############

# (b) For the Hitters data set, using "Adjusted R-square" as the model selection criterion,
# which models would you select using Best Subset and Forward Stepwise selection methods?

Hitters.data <- na.omit(Hitters)
regfit.bss <- regsubsets(Salary ~ .,
                         data = Hitters.data,
                         nvmax = ncol(Hitters.data) - 1)

regfit.fwd <- regsubsets(Salary ~ .,
                         data = Hitters.data,
                         nvmax = ncol(Hitters.data) - 1,
                         method = 'forward')

coef(regfit.bss, which.max(summary(regfit.bss)$adjr2))
coef(regfit.fwd, which.max(summary(regfit.fwd)$adjr2))


# Similarly, using "BIC" as the model selection criterion, which models would you select
# using Best Subset and Backward Stepwise selection methods? (2 points)

regfit.bwd <- regsubsets(Salary ~ .,
                         data = Hitters.data,
                         nvmax = ncol(Hitters.data) - 1,
                         method = 'backward')

coef(regfit.bss, which.max(summary(regfit.bss)$bic))
coef(regfit.bwd, which.max(summary(regfit.bwd)$bic))


############  Question 3  ############

# (a)	Using the best subset selection method and allowing up to 15 predictors, use regsubsets()
# to determine the best model with k predictors for k = 1, 2, ., 15. Use the output to answer the
# following question: Which predictors are included in the best model with 10 predictors?

Auto = na.omit(read.csv("AutoLoss.csv",
                        header = T,
                        na.strings = "?",
                        stringsAsFactors = T))

auto.bss <- regsubsets(Losses ~ .,
                       data = Auto,
                       nvmax = 15)
coef(auto.bss, 10)

# (b)	Allowing up to 15 predictors, what is the best model according to Cp criterion? State the
# predictors in the best model and their coefficients. Comment on predictors: What types of cars
# tend to have higher losses? What types of cars tend to have lower losses?

coef(auto.bss, which.min(summary(auto.bss)$cp))

# (c)	Using the forward stepwise selection method, what would be the best model with 5 predictors?
# (State the predictors included in the model and their coefficients.)

auto.fwd <- regsubsets(Losses ~ .,
                       data = Auto,
                       nvmax = 15,
                       method = 'forward')
coef(auto.fwd, 5)

# (d)	Using the backward stepwise selection method, what would be the best model with 5 predictors?
# (State the predictors included in the model and their coefficients.)

auto.bwd <- regsubsets(Losses ~ .,
                       data = Auto,
                       nvmax = 15,
                       method = 'backward')
coef(auto.bwd, 5)
