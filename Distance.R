#Comprehension check Distance

library(dslabs)
data("tissue_gene_expression")
class(tissue_gene_expression)

dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x)
dim(as.matrix(d))

d_1<-as.matrix(d)[1,2]
d_2<-as.matrix(d)[39,40]
d_3<-as.matrix(d)[73,74]

ind <- c(1,2,39,40,73,74)
as.matrix(d)[ind,ind]
image(as.matrix(d))

fit <- knn3(tissue_gene_expression$x,tissue_gene_expression$y, k = 1)
y_hat <- predict(fit, )