library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlr3measures)
library(mlr3pipelines)
library(smotefamily)
library(tidyverse)
library(e1071)


### classify-function
###
### classifies input data according to 4 classifiers and evaluates performance
### with 5-fold crossvalidation and 3 performance metrics
### Input:
###   data  A dataframe. The data to be classified
### Output:
###   a list with length 5
###     [1] a dataframe with the performance values
###     [2]-[5] confusion matrices


classify <- function(data) {
  values <- data.frame(matrix(rep(0, times = 12), ncol = 4, nrow = 3))
  colnames(values) <- c("logreg", "nb", "cart", "knn")
  rownames(values) <- c("bACC", "fbeta", "recall")
  output <- list(values)
  cv5 = rsmp("cv", folds = 5)
  
  task <- TaskClassif$new("data", data, "class", positive = "1")
  
  ##logistic regression
  logreg <- lrn("classif.glmnet")
  #resampling
  logreg_rr <- resample(task, logreg, cv5)
  output[[1]][1,1] <- logreg_rr$aggregate(msr("classif.bacc"))
  output[[1]][2,1] <- logreg_rr$aggregate(msr("classif.fbeta"))
  output[[1]][3,1] <- logreg_rr$aggregate(msr("classif.recall"))
  output[[2]] <- logreg_rr$prediction()$confusion
  
  
  ##nb
  nb <- lrn("classif.naive_bayes")
  #resampling
  nb_rr <- resample(task, nb, cv5)
  output[[1]][1,2] <- nb_rr$aggregate(msr("classif.bacc"))
  output[[1]][2,2] <- nb_rr$aggregate(msr("classif.fbeta"))
  output[[1]][3,2] <- nb_rr$aggregate(msr("classif.recall"))
  output[[3]] <- nb_rr$prediction()$confusion
  
  
  ##desicion tree
  cart <- lrn("classif.rpart")
  #resampling
  cart_rr <- resample(task, cart, cv5)
  output[[1]][1,3] <- cart_rr$aggregate(msr("classif.bacc"))
  output[[1]][2,3] <- cart_rr$aggregate(msr("classif.fbeta"))
  output[[1]][3,3] <- cart_rr$aggregate(msr("classif.recall"))
  output[[4]] <- cart_rr$prediction()$confusion
  
  
  ##knn
  knn <- lrn("classif.kknn")
  #resampling
  knn_rr <- resample(task, knn, cv5)
  output[[1]][1,4] <- knn_rr$aggregate(msr("classif.bacc"))
  output[[1]][2,4] <- knn_rr$aggregate(msr("classif.fbeta"))
  output[[1]][3,4] <- knn_rr$aggregate(msr("classif.recall"))
  output[[5]] <- knn_rr$prediction()$confusion
  
  output
}

smote_cv(data, lrnr, perf) {
  #manual 5-fold CV
  splits <- split(data, sample(rep(1:5, times = c(2000,2000,2000,2000,2000))))
  results <- c(0,0,0,0,0)
  for(i in 1:5){
    #define train and test for split
    train <- data[-splits[[i]],]
    test <- splits[[i]]
    
    #apply smote to training data
    list_train <- SMOTE(train[,2:4], train[,1], 5)
    train <- list_train$data
    
    #learn model with new training data
    task <- TaskClassif$new("train", train, "class")
    learner <- lrnr
    learner$train(task)
    
    #predict test data with learner
    learner$predict(test)
  }
}