library(dslabs)
library(tidyr)
library(ggplot2)

#load data: every data set is an example: 
if(!exists("mnist")) mnist <- read_mnist()

#save predictor in a matrix and outcome in a vector
class(mnist$train$images) #see predictor is matrix

# get first 1000 predictors
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

#work with matrix
length(x[,1])
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1,x_2)
#matrix diminsions
dim(x)

# vectors no diminsion
dim(x_1)

# convert to matrix to get diminsion
dim(as.matrix(x_1))

#convert Vector to Matrix
my_vector <- 1:15
mat <- matrix(my_vector,5 ,3)
mat

# fill by row
mat_t <- matrix(my_vector,3 ,5 , byrow = TRUE)
mat_t

#test so see if transposed is identical
identical(t(mat), mat_t)

matrix(my_vector,5,5)
grid <- matrix(x[3,],28,28)
image(1:28,1:28,grid)
image(1:28,1:28,grid[,28:1])

# 1.Do some digits require more ink than others? 
#Study the distribution of the total pixel darkness and how it varies by digits.

sums <- rowSums(x)
avg <- rowMeans(x)
data.frame(labels = as.factor(y),row_averages = avg) %>%
  ggplot(aes(labels, row_averages)) +
  geom_boxplot()

new_x <- x
new_x[new_x<50 & new_x < 205]

library(matrixStats)
r_sums <- rowSds(x)
c_sums <- colSds(x)

# apply -- not as fast as rowMeans same result
avgs <- apply(x, 1, mean)
# calculate sd for each column
sds <- apply(x, 2, sd)
plot(x,sds)

# 2.Are some pixels uninformative? 
#Study the variation of each pixel and remove predictors (columns) associated 
#with pixels that don’t change much and thus can’t provide much information 
#for classification.
sds <- colSds(x) #matrixStats
qplot(sds, bins = "30", color = I("black"))
image(1:28, 1:28, matrix(sds,28,28)[,28:1])

# remove unneeded columns
x[ ,c(351,352)]
# remove unneeded rows
x[c(2,3),]
# remove uninformative predictors sd > 60
new_x <- x[ ,colSds(x)> 60]
dim(x)
dim(new_x)

#important warning related to subsetting matrices: 
#if you select one column or one row, the result is no longer a matrix but a vector.
class(x[, 1])
dim(x[, 1])

# use drop to preserve matrix
class(x[, 1, drop = FALSE])
dim(x[, 1, drop = FALSE])

# plot histogram of predictor data
qplot(as.vector(x), bins = 30, color = I("black"))


#Can we remove smudges? First, look at the distribution of all pixel values. 
#Use this to pick a cutoff to define unwritten space. 
#Then, set anything below that cutoff to 0.
# remove smudges
new_x <- x
new_x[new_x < 50] <- 0


#Binarize the data. First, look at the distribution of all pixel values. 
#Use this to pick a cutoff to distinguish between writing and no writing. 
#Then, convert all entries into either 1 or 0 respectively.
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
# or
bin_X <- (x > 255/2)*1

grid <- matrix(bin_x[3,],28,28)
image(1:28,1:28,grid)


#Scale each of the predictors in each entry to have the same average and 
#standard deviation.
#scale each row of matrix
(x - rowMeans(x))/ rowSds(x)

#to scale columns tranpase to rows then back
t(t(x) - colMeans(x))

# use sweep
X_mean_0 <- sweep(x, 2, colMeans(x))

x_mean_0 <- sweep(x, 2, colMeans(x))
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")

t(x) %*% x
# or
crossprod(x)
# inverse
solve(crossprod(x))
# QR decompostion
qr(x)

x <- matrix(rnorm(100*10), 100, 10)
length(x[,1])
length(x[1,])
rowMeans(x)
