library(dslabs)
library(tidyr)
library(ggplot2)

#load data: every data set is an example: 
if(!exists("mnist")) mnist <- read_mnist()

#save predictor in a matrix and outcome in a vector
class(mnist$train$images) #see predictor is matrix

# get first 1000 predictors
x <- mnist$train$images[1:60000,]

# proportion of grays
new_x <- x
length(new_x[new_x > 50 & new_x < 205])/length(new_x)
