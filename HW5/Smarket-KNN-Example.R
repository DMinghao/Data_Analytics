## TASK 1 ## 

# load library ISLR and see what the dataframe Smarket looks like
library(ISLR)
View(Smarket)

# use the attach() command so that we can refer to a column without the "Smarket$" preceding it
attach(Smarket)

# create a vector train, wth 1250 values, TRUE whenever Year < 2005, FALSE otherwise
train=(Year<2005)

# Pick all the rows for which the year is 2001 - 2004, and columns correspondng to Lag1,..., Lag5, Volume (columns 2 through 7)
# this will be our training data
# store this data in a variable called Smarket.train
Smarket.train = Smarket[train,c(2,3,4,5,6,7)]

# Pick all the rows for which the year is 2005, and columns correspondng to Lag1,..., Lag5, Volume (columns 2 through 7)
# this will be our test data
# store this data in a variable called Smarket.test
Smarket.test=Smarket[!train,c(2,3,4,5,6,7)]

# store the vector of directions for the year 2001 - 2004 in a variable called Direction.train
Direction.train=Direction[train]

# store the vector of directions for the year 2005 in a variable called Direction.test
Direction.test=Direction[!train]

## TASK 2 ##

# load library class, you may have to install it first
library(class)

# set random seed 
set.seed(1)

# run knn with K=1 and store prediction results in a variable called knn.pred
knn.pred = knn(Smarket.train,Smarket.test,Direction.train,k=1)

## TASK 3 ##

# display a table that shows predicted directions for 2005 versus actual directions in 2005
table(knn.pred,Direction.test)

# calculate prediction accuracy
mean(knn.pred==Direction.test)

## TASK 4 ##

# run knn with K=3 and store prediction results in a variable called knn.pred
knn.pred = knn(Smarket.train,Smarket.test,Direction.train,k=3)

# display a table that shows precidcted directions for 2005 versus actual directions in 2005
table(knn.pred,Direction.test)

# calculate prediction accuracy
mean(knn.pred==Direction.test)


if(!require(caret)){
  install.packages('caret')
  install.packages('e1071', dependencies=TRUE)
}
library(caret)
trControl <- trainControl(method  = "cv",
                          number  = 5)
fit <- train(Smarket$Direction ~ Smarket$Lag1+Smarket$Lag2+Smarket$Lag3+Smarket$Lag4+Smarket$Lag5+Smarket$Volume,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = Smarket)