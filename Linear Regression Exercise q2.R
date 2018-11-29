library(caret)
library(tidyr)
library(dplyr)

myrmse <- function(n) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  #set.seed(1)
  
  rmse <- replicate(n = 100, {
    test_index <- createDataPartition(dat$y, times = 1,p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice( test_index)
    fit <- lm(y~x, data = train_set)
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  list(mean(rmse), sd(rmse))
}
n <- c(100,500, 1000,5000,10000)
set.seed(1)
sapply(n,myrmse)