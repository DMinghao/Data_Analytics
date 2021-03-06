# Sample code to accompany Slides on Topic 01
plot(rnorm(1000))
# If no additional arguments are provided rnorm creates draws from a standard-normal distribution
# assign values to variables
a = 5
b <- 10

# simple math calculations
a + b - a * b

# Display the names of the objects
objects()
ls()

# remove variables
rm(a)
# a

# Variable names should be short, but descriptive
#Camel caps:
MyMathScore = 95
#Underscore:
my_math_score = 95
#Dot separated:
my.math.score = 95

# If you know the name of the function or object on which you want
# help:
# help(read.csv)
# help('read.csv')
# ?read.csv

# If you do not know the name of the function or object on which you
# want help:
# help.search('input')
# RSiteSearch('input')
# ??input

# Assignment using function c()
x <- c(1, 2, 3)
c(1, 2, 3) -> x
y <- c(x, 2, x)
z <- c(1e3, 100)


# Vector arithmetic
x2 <- c(2, 4, 6)
x3 <- x + x2
x4 <- x - x2
x5 <- x * x2
x6 <- x / x2


# Generating regular sequences
seq(-5, 5, by = 1) -> x
x <- seq(length = 10, from = -5, by = .5)
x <- rep(x, times = 5)
x <- rep(x, each = 5)

# Vector Operations
x = 1:6
y = 2
x * y

y = c(1, 10)
x * y
x / y

# Logical vectors are generated by conditions:
x <- 5 > 4


# missing value: NA,
z <- c(1:5, NA)
is.na(z)

# miss value: NULL
# NULL cannot exist within a vector; if used, it simply disappears
z <- c(1, NULL, 3)
z

#create a numerical vector
x = 1:6
#test whether x>3, create a logical vector
x > 3
x <= 3
x == 3
y = (x > 3)

x = seq(0, 2, .2)
#create a new vector from the 5th element of x
x[4]
#y=x[4:]
y = x[4:length(x)]
x[c(5, 6, 8)]
z = 2:5

x = 1:10
y = (x > 5)
summary(y)
x[y]
x[!y]


# Factors: Examples
country <- c("US", "UK", "China", "India", "Japan", "Korea", "Canada")
# convert to factor
countryf <- factor(country)
country
countryf

# convert factor back to character vector
as.character(countryf)
# convert to numeric vector
as.numeric(countryf)
as.numeric(country)


# Matrices and Data Frames
aa = 1:6
dim(aa) <- c(2, 3)
aa

# Create a Matrix
a <- 1:5
b <- rnorm(5)
# make a matrix by column binding
c.matrix <- cbind(a, b)
# names of rows and columns
rownames(c.matrix)
colnames(c.matrix)

# Indexing for matrices
c.matrix[4, 2]
b
c.matrix[1, ]
a
b
c.matrix[, 2]
c.matrix[c.matrix > 1]

# Matrix Operations
# create a matrix with 2 columns and 3 rows
# filled with random normal values
m.normal = matrix(rnorm(6), nrow = 3)
m2 = m.normal * 10
m2
m2[, 2] = m2[, 2] + 50
summary(m2)


# Matrices Versus Data Frames
x = 1:10
y = rnorm(10)
mat <- cbind(x, y)
class(mat[, 1])
z = paste0('a', 1:10)
tab <- cbind(x, y, z)
class(tab)
mode(tab[, 1])
head(tab, 4)

tab <- data.frame(x, y, z)
class(tab)
head(tab)
mode(tab[, 1])
rownames(tab)
rownames(tab) <- paste0("row", 1:10)
rownames(tab)

# Data frame columns can be refered to by name using the "dollar sign" operator $
tab$x
attach(tab)
x

# Column names can be set, which can be useful for referring to data later
colnames(tab)
colnames(tab) <- c('a', 'b', 'c')
colnames(tab)
colnames(tab) <- paste0('col', 1:3)
colnames(tab)


# A list is a collection of objects that may be the same or different types.
# A data frame is a list of matched column vectors.
# Create a list
x = list(1, "y", c(2, 4, 6))
x
length(x)
class(x)
x[[2]]

is.list(tab)
tab[[2]]
names(tab)


# Basic Plot Functions
x = rnorm(50)
y = seq(from = 0,
        to = 100,
        length.out = 50)
plot(
  x,
  y,
  xlab = 'x normal random',
  ylab = 'y sequence',
  main = 'plot test',
  pch = 5,
  col = 4
)
# plot(x)
lines(x, y)
points(x, y)


# Save a png image to a file
png("my.first.plot.png", width = 480, height = 360)
dev.off(3)
dev.list()
dev.set(2)
# setwd()
getwd()

# boxplot, (try larger size)
x = rpois(lambda = 10, 50)
boxplot(x)
par(mfrow = c(1, 2))
boxplot(x)
boxplot(log(x))
par(mfrow = c(1, 1))

# Read Data
# Use read.table() or read.csv() to read data into R
Auto = read.csv("Auto.csv", header = T, na.strings = "?")

# read data from the Internet
theURL <- "http://www.jaredlander.com/data/Tomato%20First.csv"
tomato <- read.table(file = theURL, header = TRUE, sep = ",")
head(tomato)


# Probability Distributions
pnorm(2, mean = 5, sd = 10)
dnorm(2, mean = 5, sd = 10)
qnorm(.38, mean = 5, sd = 10)
z = rnorm(mean = 5, sd = 100, n = 10)

# Example: Uniform Distribution
dunif(x = 8, min = 5, max = 15)
punif(10, min = 5, max = 15)
qunif(.8, min = 5, max = 15)
runif(10, min = 5, max = 15)

# Example: Normal Distribution
set.seed(5)
rnorm(3, mean = 10, sd = 20)
rnorm(3, mean = 10, sd = 20)

set.seed(5)
rnorm(3, mean = 10, sd = 20)

# Sample Function
sample(1:40, 5)
sample(c("H", "T"), 10, replace = T)
sample(c("success", "failure"),
       10,
       replace = T,
       prob = c(0.8, 0.2))

# Probability
x <- seq(-4, 4, 0.01)
plot(x, dnorm(x), type = "l")
x <- 0:50
plot(x, dbinom(x, size = 50, prob = .33), type = "h")

# Define a Function
f <- function(x) {
  3 * x ^ (-4)
}
f(2)

# Verify whether a function is a well-defined density function
integrate(f, 1, Inf)

# Simulation: Generate Random Variables Following Any Distribution
set.seed(13)
U = runif(1000)
X = (1 - U) ^ (-1 / 3)
