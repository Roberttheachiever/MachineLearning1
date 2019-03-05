library(tidyverse)
library(caret)
library(dslabs)
library(purrr)
library(ggplot2)
data("heights")
y <- heights$sex
x <- heights$height
set.seed(1)
ks = seq(1,101,3)
test_index <- createDataPartition(y, times = 1,p = 0.5, list = FALSE)
  train_set <- heights[-test_index, ]
  test_set <- heights[test_index, ]
  
Accuracy <- map_df(ks, function(k) {
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type="class")
})

Accuracy