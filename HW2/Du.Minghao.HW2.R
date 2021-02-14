# BU.510.650	      Homework #2
# Data Analytics	  Page 2 of 2
# Spring 2021	      The Johns Hopkins University
# Arnab Bisi	      Carey Business School



############  Question 3  ############

# GPA's (Grade Point Averages) for 16 graduating MBA students, and their GMAT scores taken
# before entering the MBA program are given below. Use this data to respond to questions (a)-(c).

students <- data.frame(
  'GMAT' = c(
    560,
    540,
    520,
    580,
    520,
    620,
    660,
    630,
    550,
    550,
    600,
    537,
    610,
    570,
    590,
    650
  ),
  'GPA' = c(
    3.20,
    3.44,
    3.70,
    3.10,
    3.00,
    4.00,
    3.38,
    3.83,
    2.67,
    2.75,
    2.33,
    3.75,
    3.85,
    3.30,
    3.50,
    3.65
  )
)
attach(students)

# (a)	Create a linear regression model that uses GMAT scores as a predictor of GPA.
# Obtain and interpret the coefficient of determination R^2.

GPA.lm <- lm(GPA ~ GMAT)
summary(GPA.lm)

# (b)	Calculate the fitted value for the fourth student on the list (GMAT = 580).

GPA.pred <-
  predict(GPA.lm, data.frame(GMAT = 580), interval = "confidence")
GPA.pred

# (c)	Test whether GMAT is an important variable using a significance level of 0.05.

summary(GPA.lm)

detach(students)

############  Question 4  ############

# Use the rnorm() function to create a vector of 150 observations drawn from a N(0,1)
# distribution (call this vector x), and another vector of 150 observations drawn from a
# N(0, 0.2) distribution (call this vector Error). Use these to create a vector y according to
# the model Y = -1.5 + 0.8 X + Error. Consider this data to answer questions (a) and (b).

x <- c(rnorm(150, 0, 1))
error <- c(rnorm(150, 0, 0.2))
y <- -1.5 + 0.8*x + error

# (a)	Fit a least squares linear model to predict y using x. 
# How do beta0hat and beta1hat compare to the actual values of beta0 and beta1?

model <- lm(y ~ x)
summary(model)  

# (b)	What are the 95% confidence intervals for beta0hat and beta1hat and 
# what is the prediction interval for a case with x = 1?

confint(model, '(Intercept)', level = .95)
confint(model, 'x', level = .95)
pred <- predict(model, data.frame(x = 1), interval = "prediction")
pred
