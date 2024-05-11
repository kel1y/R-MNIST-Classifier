models <- c("glm", "lda", "naive_bayes", "knn", "gamLoess", "qda", "rf")

library(caret)
library(dslabs)
library(tidyverse)
set.seed(1)
data("mnist_27")

# train mnist27 with all methods we studied
fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 
names(fits) <- models

#predicting with fits
y_hats <- sapply(fits, function(fit){
  predict(fit, mnist_27$test)
})

#computing accuracies for each model
accuracies <- colMeans(y_hats == mnist_27$test$y)

#ensemble prediction taht returns a 7 when 50% of the models predicts a 7
votes <- rowMeans(y_hats == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#individuals that performed better than ensemble
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)

# new mode with functions that have accuracy above 80
# the former question asked the mean of methods with cross validation and accuracies lower than 80
models <- c("naive_bayes", "gamLoess", "qda", "rf")