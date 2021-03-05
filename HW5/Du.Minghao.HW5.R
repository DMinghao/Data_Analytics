# BU.510.650	      Homework #5
# Data Analytics	  Page 2 of 2
# Spring 2021	      The Johns Hopkins University
# Arnab Bisi	      Carey Business School

library(ISLR)

############  Question 1  ############
# For the Smarket data set from "ISLR" library, using the 2005 data as the test
# data and the remaining as the training data, what is your recommended value of
#  K in the K-Nearest Neighbors (KNN) approach and why? (2 points)

attach(Smarket)

train <- (Year < 2005)
Smarket.train <- Smarket[train,c(2,3,4,5,6,7)]