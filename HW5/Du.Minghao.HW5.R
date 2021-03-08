# BU.510.650	      Homework #5
# Data Analytics	  Page 2 of 2
# Spring 2021	      The Johns Hopkins University
# Arnab Bisi	      Carey Business School

library(ISLR)
library(tree)

############  Question 1  ############
# For the Smarket data set from "ISLR" library, using the 2005 data as the test
# data and the remaining as the training data, what is your recommended value of
#  K in the K-Nearest Neighbors (KNN) approach and why? (2 points)

train <- (Smarket$Year < 2005)
Smarket.train <- Smarket[train, c(2, 3, 4, 5, 6, 7)]
Smarket.test <- Smarket[!train, c(2, 3, 4, 5, 6, 7)]
Direction.train <- Smarket$Direction[train]
Direction.test <- Smarket$Direction[!train]

library(class)
set.seed(1)

acc.list <- c()
for (i in 1:(nrow(Smarket.train) / 2)) {
  knn.pred <- knn(Smarket.train, Smarket.test, Direction.train, k = i)
  acc.list[i] <- mean(knn.pred == Direction.test)
}

#+ fig.width=11, fig.height=8
plot(acc.list)
which.max(acc.list)
acc.list[which.max(acc.list)]

############  Question 3  ############
# In this question, you will use the K-Nearest Neighbors (KNN) algorithm to
# predict whether a passenger will survive or not.

Titanic <-
  read.csv('TitanicforKNN.csv',
           na.strings = '?',
           stringsAsFactors = T)
Titanic <- na.omit(Titanic)
head(Titanic)

set.seed(1)
sample_int <-
  sample(seq_len(nrow(Titanic)), size = floor(nrow(Titanic) * 0.5))
Titanic.train <- Titanic[sample_int,]
Titanic.test <- Titanic[-sample_int,]

# (a)	Run the KNN algorithm to predict the response variable Survived for each
# passenger in the test data. Do this for K = 1, 3, and 5. According to these
# predictions for K = 1, 3, and 5, what is the proportion of passengers in the
# test data that will survive? (2 points)

Titanic.knn.1 <-
  knn(
    subset(Titanic.train, select = -Survived),
    subset(Titanic.test, select = -Survived),
    Titanic.train$Survived,
    k = 1
  )

table(Titanic.knn.1, Titanic.test$Survived)
length(which(Titanic.knn.1 == 1)) / length(Titanic.knn.1)

Titanic.knn.3 <-
  knn(
    subset(Titanic.train, select = -Survived),
    subset(Titanic.test, select = -Survived),
    Titanic.train$Survived,
    k = 3
  )

table(Titanic.knn.3, Titanic.test$Survived)
length(which(Titanic.knn.3 == 1)) / length(Titanic.knn.3)

Titanic.knn.5 <-
  knn(
    subset(Titanic.train, select = -Survived),
    subset(Titanic.test, select = -Survived),
    Titanic.train$Survived,
    k = 5
  )

table(Titanic.knn.5, Titanic.test$Survived)
length(which(Titanic.knn.5 == 1)) / length(Titanic.knn.5)

# (b)	For each K, compute the accuracy of predictions for the test data.
# Which K works best in this case?

mean(Titanic.knn.1 == Titanic.test$Survived)
mean(Titanic.knn.3 == Titanic.test$Survived)
mean(Titanic.knn.5 == Titanic.test$Survived)

############  Question 4  ############
# In this question, you will estimate a decision tree for the AutoLoss data.
# The data file for this question, AutoLoss-DT.csv, is slightly different from
# the previous data file. In particular, instead of the actual loss amount for
# each vehicle, it has a column called HighLoss, which indicates whether the
# loss is high ("Yes") or low ("No") for each vehicle. Our goal is to create a
# decision tree that predicts whether the loss for a vehicle will be high or low.

AutoLoss <-
  read.csv("AutoLoss-DT.csv",
           na.strings = "?",
           stringsAsFactors = T)
AutoLoss <- na.omit(AutoLoss)
head(AutoLoss)
set.seed(15)

# (a)	Fit a decision tree to the entire data, with HighLoss as the response
# and all other variables as predictors. Plot the tree (including the names
# of predictors in the plot) and answer the following questions: Which
# predictors are used at the nodes of the tree? How many terminal nodes
# (leaves) does the tree have? (1 point)

AutoLoss.tree <- tree(AutoLoss$HighLoss ~ ., AutoLoss)

summary(AutoLoss.tree)

#+ fig.width=11, fig.height=8
plot(AutoLoss.tree)
text(AutoLoss.tree, pretty = 0)

# (b)	Determine the best tree size, using cross-validation and pruning. (See
# how we accomplished this in TASK 7 of Carseats example.)  Plot the tree you
# obtained (including the names of predictors in the plot).  (1 point)

AutoLoss.tree.cv <- cv.tree(AutoLoss.tree, FUN = prune.misclass)
AutoLoss.tree.cv

AutoLoss.tree.prune <-
  prune.misclass(AutoLoss.tree, best = AutoLoss.tree.cv$size[which.min(AutoLoss.tree.cv$dev)])

#+ fig.width=11, fig.height=8
plot(AutoLoss.tree.prune)
text(AutoLoss.tree.prune, pretty = 0)

# (c)	Use the best tree to answer the following question (you do not need to
# use R for this): Suppose my car fits the description shown below. Will this
# car incur a high loss or not?

data = data.frame(
  "gas",
  "std",
  "four",
  "wagon",
  "4wd",
  177.00,
  71.00,
  61.00,
  3527.00,
  122.00,
  241.00,
  5000.00,
  22.00,
  25000.00,
  ''
)
names(data) <- c(colnames(AutoLoss))
AutoLoss <- rbind(AutoLoss, data)

predict(AutoLoss.tree.prune,
        AutoLoss[nrow(AutoLoss), ],
        type = "class")
