###############################################
#### Example: Stock Market -- Logistic regression
###############################################


## TASK 1 ## 

# load library ISLR and see what the dataframe Smarket looks like
library(ISLR)
View(Smarket)

# check the names of the columns in Smarket
names(Smarket)

# view sumary information about the columns in Smarket
summary(Smarket)

# check the dimensions of Smarket (how many rows, how many columns)
dim(Smarket)

# check how many missing values there are in the dataframe Smarket
sum(is.na(Smarket))

## TASK 2 ## 

# the following will give an error message
cor(Smarket)

# display pairwise correlations among predictors (except column 9, which is not numeric)
cor(Smarket[,-9])

## TASK 3 ## 

# produce a scatter plot with all variables
plot(Smarket)

# plot today's return (Today) as a function of the return one day ago (Lag1)
plot(Smarket$Today~Smarket$Lag1)

# plot a histogram of today's returns 
hist(Smarket$Today)

# plot a histogram of today's returns, with 5 breaks
hist(Smarket$Today,breaks=5)

# plot a histogram of today's returns, with 9 breaks 
hist(Smarket$Today,breaks=9)

## TASK 4 ## 

# run logistic regression with Direction as the response variable and Volume, Lag 1, ..., Lag 5 as predictors
glm.fit = glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family=binomial, data=Smarket)

# view the summary output
summary(glm.fit)

## TASK 5 ## 

# predict the probability of Up for each day in the training data
# if you do not provide "newdata" option in predict(), 
# it will provide predictions for the data that was used for estimation (i.e., training data) 
predict(glm.fit,type="response")

# store prediction results in a variable called glm.probs
glm.probs <- predict(glm.fit,type="response")

# set the option so that we display 4 digits after the decimal point
options("digits"=4)

# display first five element of glm.probs
glm.probs[1:5]

# if you wanted to check what 1 and 0 corresponds to (for the response variable)
contrasts(Smarket$Direction) 

## TASK 6 ##

# create a vector glm.pred, which initially consists of Down for all 1250 days
glm.pred=rep("Down",1250)

# update the vector to Up, for any day in which the probability of Up is greater than 50%
glm.pred[glm.probs>.5]="Up"

# create a table, which cross-tabulates our predictions (in glm.pred vector)
# with respect to actual Direction (in the Direction column of Smarket)
table(glm.pred,Smarket$Direction)

# compute the percentage of time we were correct
mean(glm.pred==Smarket$Direction)

## TASK 7 ##

# use the attach() command so that we can refer to a column without the "Smarket$" preceding it
attach(Smarket)

# create a vector train, wth 1250 values, TRUE whenever Year < 2005, FALSE otherwise
train=(Year<2005)

# Pick all the rows for which the year is 2005, this will be our test data
# store this data in a variable called Smarket.2005
Smarket.2005=Smarket[!train,]

# check the dimension of Smarket.2005
dim(Smarket.2005)

# store the vector of directions for the year 2005 in a variable called Direction.2005
Direction.2005=Direction[!train]

## TASK 8 ##
# run logistic regression, but only on the training data (2001  through 2004)
glm.fit=glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family=binomial,data=Smarket,subset=train)

## TASKs 9 and 10##
# predict the probability of direction being up for the days in the test data
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.probs

# create a vector glm.pred, which initially consists of Down for all 1250 days
glm.pred=rep("Down",252)

# update the vector to Up, for any day in which the probability of Up is greater than 50%
glm.pred[glm.probs>.5]="Up"

# compute the percentage of time we were correct
mean(glm.pred==Direction.2005)

## TASK 11 -- exercise##

# try running regressoon with only Lag 1 and Lag 2 as pedictor variables, using only training data


# predict the probability of direction being up for the days in the test data


# make a prediction of up whenever the probability is more than 50% and check the accuracy of the prediction



