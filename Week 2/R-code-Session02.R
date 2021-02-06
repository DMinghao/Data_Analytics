## class 02: linear regression & model selection

getwd()
dir()


###################################
#### advertising example
##################################
my.ad<-read.csv("Advertising.csv")
attach(my.ad)

names(my.ad)
summary(my.ad)

par(mfrow=c(1,1)) # par(mfrow=c(x,y)) divides plot space into x rows and y colums
plot(Sales,TV)
plot(my.ad$Sales,my.ad$TV)
plot(Sales~TV)

my.first.lr=lm(Sales~TV)
my.first.lr
summary(my.first.lr)

names(my.first.lr)

abline(my.first.lr)
my.first.lr$residuals
require(psych)
describe(my.first.lr$residuals)
boxplot(my.first.lr$residuals)


### regress Sales w.r.t. Radio
plot(Sales~Radio)
my.lm.2=lm(Sales~Radio)
summary(my.lm.2)
abline(my.lm.2)

plot(Sales~Newspaper)
my.lm.3=lm(Sales~Newspaper)
summary(my.lm.3)

my.lm.4=lm(Sales~TV+Radio+Newspaper)
summary(my.lm.4)

my.lm.4=lm(Sales~.,data=my.ad)
summary(my.lm.4)

my.lm.5=lm(Sales~TV+Radio)
summary(my.lm.5)
detach(my.ad)

########################################
##### Example Fuel efficiency
########################################

FuelEff<-read.csv("FuelEfficiency.csv")
head(FuelEff)
attach(FuelEff)
plot(MPG~GPM)
MPG*GPM

plot(GPM~WT,data=FuelEff)
plot(GPM~DIS,data=FuelEff)
plot(GPM~NC,data=FuelEff)
plot(GPM~HP,data=FuelEff)

m0=lm(GPM~WT)
summary(m0)

m1=lm(GPM~.,data=FuelEff)
summary(m1)

FuelEff2=FuelEff[-1]
head(FuelEff2)

m2 <- lm(GPM~.,data=FuelEff2)
m2 <- lm(GPM~.-MPG,data=FuelEff)
summary(m2)

m3 <- lm(GPM~WT + DIS, data=FuelEff)
summary(m3)
detach(FuelEff)

####################################
#### Boston Data Set
####################################

library(MASS)
# library(ISLR)
# utils:::menuInstallPkgs()
attach(Boston)
head(Boston)
?Boston
names(Boston)

lm.fit=lm(medv~lstat,data=Boston)
summary(lm.fit)
coef(lm.fit)

# 95% confidence interval
confint(lm.fit)

# 90% confidence interval
confint(lm.fit,"lstat",level=.9)

predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval="prediction")

plot(medv~lstat,data=Boston)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=5)
abline(lm.fit,lwd=5,col=2)
abline(lm.fit,lwd=5,col=3)
abline(lm.fit,lwd=5,col="red")
plot(medv~lstat,data=Boston,pch=20)
plot(medv~lstat,data=Boston,pch="$")
plot(1:20,1:20,pch=1:20)

lm.fit.1=lm(medv~lstat+age,data=Boston)
summary(lm.fit.1)

lm.fit.1=lm(medv~lstat+age+rm,data=Boston)
summary(lm.fit.1)

lm.fit.2=lm(medv~.,data=Boston)
summary(lm.fit.2)

lm.fit.3=lm(medv~.-age,data=Boston)
summary(lm.fit.3)

lm.fit.4=lm(medv~.-age-indus,data=Boston)
summary(lm.fit.4)

fit.lm.2=lm(medv~lstat:age,data=Boston)
summary(fit.lm.2)

fit.lm.3=lm(medv~lstat*age,data=Boston)
summary(fit.lm.3)

x3=lstat*age
x3=Boston$lstat*Boston$age
x3
fit.lm.10=lm(medv~x3,data=Boston)
summary(fit.lm.10)

fit.lm.11=lm(medv~lstat+age+x3,data=Boston)
summary(fit.lm.11)

plot(medv~lstat)

lm.fit4=lm(medv~lstat+ I(lstat^2),data=Boston)
summary(lm.fit4)

x4=Boston$lstat^2
lm.fit5=lm(medv~lstat+x4,data=Boston)
summary(lm.fit5)

lm.fit4=lm(medv~lstat+ I(lstat^2)+I(lstat^3),data=Boston)
summary(lm.fit4)

