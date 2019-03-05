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

#calculate overall accuracy
y_hat <- sample(c("Male","Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat==test_set$sex)

#can we do better,lets explore with data analysis
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

#predict male within 2 sd of avg male
y_hat <- ifelse( x > 62, "Male","Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

#let look at multiple cutoffs to predict even better
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
# display cutoffs
plot(cutoff,accuracy)
# or
data.frame(cutoff, accuracy) %>%
ggplot(aes(cutoff, accuracy)) +
  geom_point() +
  geom_line()

max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#Test our cutoff value in the test dataset
y_hat <- ifelse(test_set$height > best_cutoff, "Male","Female") %>% factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

#tabulate all combimations of predictions and actual values using function table
table(predicted = y_hat, actual = test_set$sex)

#study table closely, compute accuracy of each sex
test_set %>% mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarise(accuracy = mean(y_hat == sex))
#note the imbalanes between males and females. to mamy female predictions were wrong
prev <- mean(y == "Male")
prev

#calulate Sensitivity and specificity with confusionMatrix
confusionMatrix(data = y_hat, reference = test_set$sex)

#balanced accuracy F1_score instaed of overall acuracy
cutoff <- seq(61,70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male","Female") %>% factor(levels= levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

plot(cutoff,F_1)
data.frame(cutoff,accuracy) %>%
  ggplot(aes(cutoff, accuracy)) +
  geom_point() +
  geom_line()

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))
confusionMatrix(data = y_hat, reference = test_set$sex)


#ROC and precision
#quessing male with the higher probability would give us higher accuravy due to bias (example)
p <- 0.9
y_hat <- sample(c("Male","Female"),length(test_index), replace = TRUE, prob = c(p, 1-p)) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

probs <- seq(0, 1, length.out = 10)
quessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male","Female"),length(test_index), replace = TRUE, prob = c(p, 1-p)) %>%
    factor(levels = levels(test_set$sex))
  list(method= "quessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

quessing %>% qplot(FPR, TPR, data = ., xlab = "1 - Specficity", ylab = "Sensitivity")

#create ROC curve based on height
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female","Male"))
  list(method= "Height cutoffs",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
height_cutoff %>% qplot(FPR, TPR, data = ., xlab = "1 - Specficity", ylab = "Sensitivity")

#combine curves
bind_rows(quessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specficity") +
  ylab("Sensitivity")


# add point labels
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female","Male"))
  list(method= "Height cutoffs",
       cutoff = x,
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label=cutoff)) +
  geom_line() +
  geom_point() +
  geom_text(nudge_y = .1)

height_cutoff

#precision-recall plot. The idea is similar, but we instead plot precision against recall:
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
  
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

#----------------------------------------------------------------------------

library(purrr)
y <- heights$sex
x <- heights$height
set.seed(1)
ks = seq(1,101,3)
test_index <- createDataPartition(y, times = 1,p = 0.5, list = FALSE)
train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]
accuracy <- map_df(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = 1)
  y_hat <- predict(fit, test_set, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = test_set$sex)
  test_error <- cm_train$overall["Accuracy"]
  list(k=k, test = test_error)
})
accuracy
F_1 <- map_dbl(ks, function(k){
  y_hat <- ifelse(train_set$height > k, "Male","Female") %>% factor(levels= levels(test_set$sex))
  f_1 <- F_meas(data = y_hat, reference = factor(train_set$sex))
  list(k=k,f_1 = f_1)
})
F_1

max(F_1)
print(as.matrix(F_1))

Accuracy <- map_df(ks, function(k) {
  test_index < -  createDataPartion(...)
  ...
  F_1 <- F_meas(...)
  list( k=k , F_1 = F_1)
  
})

Accuracy %>% slice(which.max(F_1))