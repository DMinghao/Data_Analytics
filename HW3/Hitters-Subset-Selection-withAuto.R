#######################################################
############# Example: Hitters -- Subset selection
#######################################################

## TASK 1 ##

# load library ISLR and see what the dataframe Hitters looks like
library(ISLR)
View(Hitters)

# check the names of the columns in Hitters
names(Hitters)

# check the first five rows of Hitters
Hitters[1:5, ]

# check the dimensions of Hitters (how many rows, how many columns)
dim(Hitters)

# check the names of rows in Hitters
rownames(Hitters)

# write the dataframe Hitters to a file
write.table(Hitters, file = "Hitters.csv", row.names = TRUE)

# check how many missing values there are in the Salary column of Hitters
sum(is.na(Hitters$Salary))

# remove missing values from Hitters and assign it to a dataframe called Hitters2
Hitters2 = na.omit(Hitters)

# check the dimension of Hitters2 (how many rows, how many columns)
dim(Hitters2)

# check how many missing values there are in the dataframe Hitters
sum(is.na(Hitters2))

## TASK 2 ##

# load library leaps, which has the regsubsets() command for subset selection
library(leaps)

# run regsubsets using Salary as the repsonse, all other variables as predictors, with data from Hitters2
# store the results in a variable called regfit.full
regfit.full = regsubsets(Salary ~ ., data = Hitters2)
summary(regfit.full)

## TASK 3 ##

# if you wanted to see best subsets with up to 15 predictors
regfit.full = regsubsets(Salary ~ ., data = Hitters2, nvmax = 15)
summary(regfit.full)


## TASK 4 ##

# store the sumary of results in a variable called reg.summary
reg.summary = summary(regfit.full)

# check the names of columns in reg.summary
names(reg.summary)

# display the column reg.summary$rsq
# what you see in the output is R^2 for the best subset with one predictor,
# R^2 for the best subset with two predictors, etc.
reg.summary$rsq

# display the column reg.summary$adjr2
# what you see in the output is adjusted R^2 for the best subset with one predictor,
# adjusted R^2 for the best subset with two predictors, etc.
reg.summary$adjr2

# display the column reg.summary$cp
# what you see in the output is Cp for the best subset with one predictor,
# Cp for the best subset with two predictors, etc.
reg.summary$cp

which.max(reg.summary$adjr2) # function which.max() returns the index of maximum value
coef(regfit.full, 11)

## TASK 5 ##

# plot adjusted R^2 as a function of predictors in the model
#+ fig.width=11, fig.height=8
plot(reg.summary$adjr2, xlab = "Number of predictors", ylab = "Adjusted R^2")

# display the number of predictors for which adjusted R^2 reaches its maximum -- it is 11
which.max(reg.summary$adjr2)

# display the coefficient estimates for the best model with 11 predictors
coef(regfit.full, 11)

## TASK 6 ##

# plot Cp as a function of predictors in the model
#+ fig.width=11, fig.height=8
plot(reg.summary$adjr2, xlab = "Number of predictors", ylab = "Cp")

# display the number of predictors for which Cp reaches its minimum -- it is 10
which.min(reg.summary$cp)

# display the coefficient estimates for the best model with 10 predictors
coef(regfit.full, 10)

## TASK 7 ##

# run forward stepwise selection, allowing subsets with up to 19 predictors
regfit.fwd = regsubsets(Salary ~ .,
                        data = Hitters2,
                        nvmax = 19,
                        method = "forward")
summary(regfit.fwd)

## TASK 8 ##

# run backward stepwise selection, allowing subsets with up to 19 predictors
regfit.bwd = regsubsets(Salary ~ .,
                        data = Hitters2,
                        nvmax = 19,
                        method = "backward")
summary(regfit.bwd)

## TASK 9 ##

# the coefficient estimates for the best model with 7 predictors when using best subset selection
coef(regfit.full, 7)

# the coefficient estimates for the best model with 7 predictors when using forward selection
coef(regfit.fwd, 7)

# the coefficient estimates for the best model with 7 predictors when using backward selection
coef(regfit.bwd, 7)


###############################################
#### in-class exercise
library(leaps)
Auto = read.csv(
  "Auto.csv",
  header = T,
  na.strings = "?",
  stringsAsFactors = T
)
Auto2 = na.omit(Auto)

head(Auto2)

my.auto = regsubsets(mpg ~ . - name, data = Auto2)
summary(my.auto)
coef(my.auto, 3)


my.auto2 = regsubsets(mpg ~ . - name, data = Auto2, method = "forward")
coef(my.auto2, 3)

my.auto3 = regsubsets(mpg ~ . - name, data = Auto2, method = "backward")
coef(my.auto3, 3)

#################################################
