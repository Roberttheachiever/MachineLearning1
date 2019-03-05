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

-----------------------------------------

data("tissue_gene_expression")
fit <- with(tissue_gene_expression, train(x, y, method = "knn", tuneGrid = data.frame( k = seq(1, 7, 2))))
ggplot(fit)
fit$results





