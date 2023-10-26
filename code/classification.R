library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlr3measures)
library(mlr3pipelines)
library(smotefamily)
library(tidyverse)
library(e1071)


### classify - function
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
  rownames(values) <- c("bACC", "f1", "recall")
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


### smote_cv - function
###
### classifies data with 5 fold cv while applying the smote-algorithm to the 
### training data in each fold. The classifying learner and the performance measure 
### is to be specified
### Input:
###   data    A dataframe. The data to be classified
###   lrnr    An object of mlr3-class Learner. The learner to be trained
###   perf    An object of mlr3-class Measure. The measure of the performance evaluation.
### Output:
###   A numeric. The aggregated performance measure


smote_cv <- function(data, lrnr, perf) {
  #manual 5-fold CV
  splits <- split(data, sample(rep(1:5, times = c(2000,2000,2000,2000,2000))))
  results <- c(0,0,0,0,0)
  for(i in 1:5){
    #define train and test for split
    train <- data.frame()
    trainsets <- c(1,2,3,4,5)
    trainsets <- trainsets[!trainsets %in% c(i)]
    for(j in 1:4){
      train <- rbind(train, splits[[j]])
    }
    test <- splits[[i]]
    
    #apply smote to training data
    list_train <- SMOTE(train[,2:4], train[,1], 5)
    train <- list_train$data
    train$class <- as.factor(train$class)
    
    #learn model with new training data
    task <- TaskClassif$new("train", train, "class", positive = "1")
    learner <- lrnr
    learner$train(task)
    
    #predict test data with learner
    prediction <- learner$predict_newdata(test)
    results[i] <- prediction$score(perf)
  }
  mean(results)
}


### classify_smote - function
###
### classifies a (imbalanced) data set according to 4 classifiers and evaluates 
### performance with 5-fold cv and 3 performance metrics while using SMOTE to
### oversample the minority class
### Input:
###   data    A dataframe. The data to be classified
### Output:
###   A dataframe with the performance scores for each learner

classify_smote <- function(data) {
  #define output dataframe
  output <- data.frame(matrix(rep(0, times = 12), ncol = 4, nrow = 3))
  colnames(output) <- c("logreg", "nb", "cart", "knn")
  rownames(output) <- c("bACC", "f1", "recall") 
  
  #logreg
  output[1,1] <- smote_cv(data, lrn("classif.glmnet"), msr("classif.bacc"))
  output[2,1] <- smote_cv(data, lrn("classif.glmnet"), msr("classif.fbeta"))
  output[3,1] <- smote_cv(data, lrn("classif.glmnet"), msr("classif.recall"))
  
  #naive bayes
  output[1,2] <- smote_cv(data, lrn("classif.naive_bayes"), msr("classif.bacc"))
  output[2,2] <- smote_cv(data, lrn("classif.naive_bayes"), msr("classif.fbeta"))
  output[3,2] <- smote_cv(data, lrn("classif.naive_bayes"), msr("classif.recall"))
  
  #cart
  output[1,3] <- smote_cv(data, lrn("classif.rpart"), msr("classif.bacc"))
  output[2,3] <- smote_cv(data, lrn("classif.rpart"), msr("classif.fbeta"))
  output[3,3] <- smote_cv(data, lrn("classif.rpart"), msr("classif.recall"))
  
  #knn
  output[1,4] <- smote_cv(data, lrn("classif.kknn"), msr("classif.bacc"))
  output[2,4] <- smote_cv(data, lrn("classif.kknn"), msr("classif.fbeta"))
  output[3,4] <- smote_cv(data, lrn("classif.kknn"), msr("classif.recall"))
  
  output
}

classify_smote(data61)
