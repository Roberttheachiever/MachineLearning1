install.packages("rattle")
install.packages("RGtk2")
library(rattle)
rattle()
loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds <- "pima-indians-diabetes/pima-indians.diabetes.data"
url <- paste(loc, ds, sep="")
diabetes <- read.table(url, sep=",", header=FALSE)
names(diabetes)
names(diabetes <- c(""))