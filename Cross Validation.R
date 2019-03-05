library(caret)
train_glm <- train(y ~ ., method = "glm", mnist_27$train)
train_knn <- train(y ~ ., method = "knn", mnist_27$train)

predict.train

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall["Accuracy"]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

getModelInfo("knn")

modelLookup("knn")

train_knn <- train( y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)

train_knn <- train(y ~ ., method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, hightlight = TRUE)

train_knn$bestTune
train_knn$finalModel

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]


control <- trainControl(method="cv", number = 10, p = .9)
train_knn <- train(y ~ ., method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)),
                   trControl = control)
ggplot(train_knn, hightlight = TRUE)

train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))

install.packages("gam")
modelLookup("gamLoess")
train_loess <- train( y~ .,
                      method = "gamLoess",
                      tuneGrid = grid,
                      dat = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

library(dplyr)
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5), color="black")
}
confusionMatrix(data= predict(train_loess, mnist_27$test),reference = mnist_27$test$y)$overall["Accuracy"]

plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])



#exercises
library(caret)
library(tidyr)
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]
#q1

fit <- train(x_subset, y, method = "glm")
fit$results

#q2
library(devtools)
devtools::install_bioc("genefilter")
library(genefilter)
tt <- colttests(x, y)
tt <- filter(tt,tt$p.value>=.01)
idx <-createDataPartition(tt$p.value,times=1,p=.01, list= FALSE)

rdacb.kfold.crossval.reg <- function(df, nfolds){
  fold <- sample(1:nfolds, nrow(df), replace = TRUE)
  mean.sqr.errs <- sapply(1:nfolds, rdacb.kfold.cval.reg.iter, df, fold)
  list("mean_sqr_errs" = mean.sqr.errs,
       "overall_mean_sqr_err" = mean(mean.sqr.errs),
       "std_dev_mean_sqr_err" = sd(mean.sqr.errs))
}

rdacb.kfold.cval.reg.iter <- function(k, df, fold){
  trg.idx <- !fold %in% c(k)
  test.idx <- fold %in% c(k)
  mod <- lm(p.value ~ ., data = df[trg.idx, ] )
  pred <- predict(mod, df[test.idx, ])
  sqr.errs <- (pred - df[test.idx, "p.value"])^2
  mean(sqr.errs)
}

res <- rdacb.kfold.crossval.reg(tt, 5)
res$mean_sqr_errs
res$overall_mean_sqr_err
res$std_dev_mean_sqr_err

cc <- filter(tt,tt$p.value<=.01)

