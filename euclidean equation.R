#use warnings()
#supress warnings
#install.packages("rafalib") 
library(devtools)
install_github("ririzarr/rafalib")
library(rafalib)
library(dslabs)
library(tidyr)
library(ggplot2)

mypar()
plot(c(0,1,1), c(0,0,1), pch=16, cex=2, xart="n", yart="n", xlab="", ylab="", bty="n",xlim=c(-0.52,1.25), ylim=c(-0.25,1.25))
lines(c(0,1,1,0),c(0,0,1,0))
text(0,.2,expression(paste('(A'[x]*',A'[y]*')')),cex=1.5)
text(1,1.2,expression(paste('(B'[x]*',B'[y]*')')),cex=1.5)
text(-0.1,0,"A",cex=2)
text(1.1,1,"B",cex=2)

set.seed(0)
#load data: every data set is an example: 
if(!exists("mnist")) mnist <- read_mnist()
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

# get predictors
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

#example
y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]
sqrt(sum((x_1-x_2)^2))
sqrt(sum((x_1-x_3)^2))
sqrt(sum((x_2-x_3)^2))

d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]

image(as.matrix(d))
image(as.matrix(d)[order(y), order(y)])

d <- dist(t(x))
dim(as.matrix(d))
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))




