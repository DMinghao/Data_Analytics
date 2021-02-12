# R code Class 01

## read the data
## you'll first need to make your 'working directory' to the folder you've stored the data.
## You can do this from the menu bar, or with a command like
## setwd("/Users/.../data")

###########################################
#### advertising data set
##########################################
ad = read.csv("Advertising.csv", stringsAsFactors = T)
predict(
  lm(Sales ~ TV + Radio + Newspaper, data = ad),
  data.frame(
    TV = 100,
    Radio = 50,
    Newspaper = 25
  ),
  interval = "confidence"
)
predict(lm(Sales ~ TV + Radio + Newspaper, data = ad),
        data.frame(
          TV = 100,
          Radio = 50,
          Newspaper = 25
        ))
# ad <- na.omit(ad)

#########################################################################
getwd() # this will print your working directory
trucks <- read.csv("pickup.csv", stringsAsFactors = T)
# trucks <- na.omit(trucks)

## explore the data set

nrow(trucks) # sample size

# subsetting
head(trucks)
tail(trucks)
trucks[1,] # the first observation
trucks[1:10,] # the first 10 observations
trucks[, 1] # the first variable (year)
trucks$year # same thing
trucks[, 'year'] # same thing again

# summary of each variable
summary(trucks)


# logic subsetting
trucks[trucks$miles > 200000,]

## the make variable is special
class(trucks$make) # it is a factor (categorical)
levels(trucks$make) # with 3 levels
trucks$make[1:2] # the first two obs are GMC and Dodge
as.numeric(trucks$make[1:2]) # which R calls levels 3 and 1

## produce some plots
? hist()

par(mfrow = c(1, 3))
hist(trucks$year, col = "blue") ## a histogram
hist(trucks$miles, col = "blue")
hist(trucks$price, col = "blue")

par(mfrow = c(1, 2))
plot(price ~ make, data = trucks, col = "blue") ## a boxplot
plot(miles ~ make, data = trucks, col = "blue")

par(mfrow = c(1, 2))
plot(price ~ miles, data = trucks) ## simple scatterplot
plot(price ~ year, data = trucks) ## simple scatterplot

plot(price ~ miles, data = trucks, log = "y") ## price on log scale
plot(price ~ miles,
     data = trucks,
     log = "y",
     col = trucks$make) ## in color
## add a legend (colors 1,2,3 are black,red,green)
par(mfrow = c(1, 2))
plot(price ~ miles, data = trucks, col = trucks$make) ## simple scatterplot
legend("topright",
       fill = 1:3,
       legend = levels(trucks$make))

plot(price ~ year, data = trucks, col = trucks$make) ## simple scatterplot
legend("bottomleft",
       fill = 1:3,
       legend = levels(trucks$make))


###########################################################################
## Alumni Donation Data Set
getwd()
dir()
don <- read.csv("contribution.csv", stringsAsFactors = T)
head(don)
class(don)
summary(don)
attach(don)
class(don$Marital.Status)
levels(don$Marital.Status)
levels(don$Next.Degree)
class(don$Next.Degree)
tail(don)
# fix(don)

library(lattice)
barchart(
  table(Class.Year),
  horizontal = F,
  xlab = "Class Year",
  col = "black"
)
don$TGiving = don[, 'FY00Giving'] + don[, 'FY01Giving'] + don[, 'FY02Giving'] + don[, 'FY03Giving'] + don[, 'FY04Giving']
head(don$TGiving)
quantile(don$TGiving, probs = seq(0.95, 1, 0.01))
head(don)
quantile(don$TGiving, probs = seq(0.95, 1, 0.01))
sum(don$TGiving)
hist(don$TGiving)
ff1 = don$TGiving[don$TGiving != 0]
ff2 = ff1[ff1 <= 1000]
hist(ff2,
     main = paste("Histogram of TGivingTrunc"),
     xlab = "TGiving")
ff2[1:5]
ff1[1:5]
ff1 = don$TGiving[don$TGiving != 0]
ff1[1:5]
ff2 = ff1[ff1 <= 1000]
ff2[1:5]
hist(ff2,
     main = paste("Histogram of TGivingTrunc"),
     xlab = "TGiving")
boxplot(TGiving ~ Class.Year, data = don, outline = FALSE)
boxplot(TGiving ~ Class.Year, data = don, outline = T)
boxplot(TGiving ~ Gender, data = don, outline = F)
boxplot(TGiving ~ Marital.Status, data = don, outline = F)
