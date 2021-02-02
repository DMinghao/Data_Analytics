############  Question 2  ############

# (a) Write the R-code to create a matrix in which the first column consists of
# x variable taking integer values from 1 to 6 and the second column taking 6 random
# values from a standard normal distribution. Provide the matrix you generated.

mtrx <- matrix(c((1:6), rnorm(6)), ncol = 2, byrow = F)
mtrx


# (b) What is the key difference between a matrix and a data frame in R?
# Provide an example to demonstrate the difference. (1 point)

setClass('Person', representation(name = 'character', age = 'numeric'))
john <- list(list(new(
  'Person', name = 'John Smith', age = 42
)))

mtx <- matrix(c(1:9), ncol = 3, byrow = T)
mtx
# mtx[4, ] <- c(10, 11, 12) # Error in `[<-`(`*tmp*`, 4, , value = c(10, 11, 12)) : subscript out of bounds
mtx[1, 1] <- john # Matrix structure compromised: variable mtx is now a list object
class(mtx)

df <- as.data.frame(matrix(c(1:9), ncol = 3, byrow = T))
df
df[4, ] <- c(10, 11, 12) # Dataframe allows appending new row to existing collection
df[1, 1] <- john # Dataframe allows storing heterogeneous data
df



############  Question 3  ############

# Download the Auto data set, from the course Blackboard page to answer questions (a) - (e).
# Make sure that the missing values have been removed from the data.

auto <- read.csv('Auto.csv',
                 stringsAsFactors = T,
                 na.strings = '?')
nrow(auto)
auto <- na.omit(auto)
rownames(auto) <- NULL
nrow(auto)
head(auto)
summary(auto)


# (b)	What is the range and median of each quantitative predictor?
# (Hint: For range, use the range() function.)

quant <- sapply(auto[, 1:7], function(c) c(range(c), median(c)))
row.names(quant) <- c('MIN', 'MAX', 'MID')
quant


# (c)	What is the mean and standard deviation of each quantitative predictor?

quant <- sapply(auto[, 1:7], function(c) c(mean(c), sd(c)))
row.names(quant) <- c('MEAN', 'STDDEV')
quant


# (d)	Remove the 25th through 115th observations. What is the range, median, mean,
# and standard deviation of each predictor in the subset of the data that remains?

quant <- sapply(auto[-(25:115), 1:7], function(c) c(range(c), median(c), mean(c), sd(c)))
row.names(quant) <- c('MIN', 'MAX', 'MID', 'MEAN', 'STDDEV')
quant


# (e)	Suppose that we wish to predict gas mileage (mpg) on the basis of other variables.
# Using the full data set which variables do you believe will be useful in predicting mpg?
# Explain your answer using plots and correlation coefficients of the data.

res <- sapply(auto[, 2:7], function(c) cor(auto$mpg, c))
res
#+ fig.width=11, fig.height=8
full <- plot(auto[, 1:7])
#+ fig.width=11, fig.height=8
plt <-
  barplot(res,
          names.arg = names(res),
          ylim = c(-1, 1),
          ylab = 'Correlation Coefficients')
text(
  plt,
  y = round(unname(res), 4),
  label = round(unname(res), 4),
  pos = 3,
  col = "red"
)
