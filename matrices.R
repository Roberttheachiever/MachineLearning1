library(maps)## load maps first to avoid map conflict with purrr
library(MASS) ## load MASS and matrixStats first to avoid select and count conflict
#library(tidyverse)
library(matrixStats) 

library(tidyr)
library(dslabs)
#library(dplyr)
suppressPackageStartupMessages(library(ggplot2))


if(!exists("mnist")) mnist <- read_mnist()

names(mnist)

dim(mnist$train$images)

class(mnist$train$labels)

table(mnist$train$labels)

# need smaller sample
set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index, ]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$train$images), 1000)
x_test <- mnist$train$images[index, ]
y_test <- factor(mnist$train$labels[index])

#remove predictors that are highly correlated with others, 
#removing predictors with very non- unique or close to zero variations

library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

library(caret)
nzv <- nearZeroVar(x)

# see results after removing predictors (see above)
image(matrix(1:784 %in% nzv, 28, 28))

# remaining column counts
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

# caret requires column names be added to the feature matrices
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)

# kNN k-fold cross validation to improve speed
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y, 
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
ggplot(train_knn)

# modify by changing subset size the n and b to deterimine processing time
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index], 
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

fit_knn <- knn3(x[ ,col_index], y, k = 5)

y_hat_knn <- predict(fit_knn,
                x_test[ , col_index],
                type = "class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

cm$byClass[, 1:2]
#note how 8 is hardest to detect and 7 is the most commonly incorrectly predicted

# use random forest for better prediction
library(Rborist)
control<- trainControl(method = "cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1), predFixed = c(10, 15, 35))

train_rf <- train(x[ , col_index],
                  y,
                  method = "Rborist",
                  nTree = 50,
                  tuneGrid = grid,
                  nSamp = 5000)

ggplot(train_rf)
train_rf$bestTune

fit_rf <-Rborist(x[,col_index], y,
                 nTree = 1000,
                 minNode = train_rf$bestTune$minNode,
                 predFixed = train_rf$bestTune$predFixed)
y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]


