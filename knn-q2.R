library(dslabs)
library(purrr)
data("tissue_gene_expression")
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
ks <- seq(1, 11, 2)
set.seed(1)
test_index <- createDataPartition(y, times = 1,p = 0.5, list = FALSE)
train_set_x <- x[test_index, ]
test_set_x <- x[-test_index, ]
train_set_y <- y[test_index ]
test_set_y <- y[-test_index ]
train_set <- data.frame(train_set_x,train_set_y)
test_set <- data.frame(test_set_x,test_set_y)
class(train_set)
Accuracy <- map_df(ks, function(k){
  knn_fit <- knn3(train_set_x,train_set_y,k = k)
  y_hat <- predict(knn_fit, test_set_x, type="class")
  test_error <- confusionMatrix(data = y_hat,reference = test_set_y)$overall["Accuracy"]
  list(k=k,test=test_error)
})

Accuracy
print(as.matrix((Accuracy)))
sum(Accuracy$test)

fit <- train(train_set_x, train_set_y, method = "knn", tuneGrid = data.frame(k = seq(101,301,25)))
ggplot(fit)


fit$bestTune
fit$finalModel

train_knn <- train(y ~ ., method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, hightlight = TRUE)