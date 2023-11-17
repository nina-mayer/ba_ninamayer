library(mlr3)
library(mlr3learners)
library(mlr3measures)
library(mlr3tuning)
library(tidyverse)
library(e1071)
library(bimba)
library(caret)
library(ebmc)


set.seed(12226947)

### NO RESAMPLING

### classify - function
###
### classifies input data according to 4 classifiers and evaluates performance
### with 5-fold crossvalidation and 3 performance metrics
### Input:
###   data  A dataframe. The data to be classified
###   imb   A string. The data imbalance
### Output:
###   a list with length 5
###     [1] a dataframe with the performance values
###     [2]-[5] confusion matrices


classify <- function(data, imb) {
  output <- data.frame(matrix(rep(0, times = 36), ncol = 4, nrow = 9))
  colnames(output) <- c("classifier", "value", "imbalance", "performance")
  output[,1] <- c(rep("NB", times = 3), rep("RF", times = 3), rep("kNN", times = 3))
  output[,3] <- rep(imb, times = 9)
  output[,4] <- rep(c("bACC", "F1", "Recall"), times = 3)
  cv5 = rsmp("cv", folds = 5)
  
  task <- TaskClassif$new("data", data, "class", positive = "1")
  
  
  ##nb
  nb <- lrn("classif.naive_bayes")
  #resampling
  nb_rr <- resample(task, nb, cv5)
  output[1,2] <- nb_rr$aggregate(msr("classif.bacc"))
  output[2,2] <- nb_rr$aggregate(msr("classif.fbeta"))
  output[3,2] <- nb_rr$aggregate(msr("classif.recall"))
  
  
  ##random forest
  rf <- lrn("classif.ranger")
  #resampling
  rf_rr <- resample(task, rf, cv5)
  output[4,2] <- rf_rr$aggregate(msr("classif.bacc"))
  output[5,2] <- rf_rr$aggregate(msr("classif.fbeta"))
  output[6,2] <- rf_rr$aggregate(msr("classif.recall"))
  
  
  ##knn
  knn <- lrn("classif.kknn")
  #resampling
  knn_rr <- resample(task, knn, cv5)
  output[7,2] <- knn_rr$aggregate(msr("classif.bacc"))
  output[8,2] <- knn_rr$aggregate(msr("classif.fbeta"))
  output[9,2] <- knn_rr$aggregate(msr("classif.recall"))
  
  output
}

# classify data

raw4 <- classify(data4, "1:4")
raw9 <- classify(data9, "1:9")
raw19 <- classify(data19, "1:19")
raw99 <- classify(data99, "1:99")
raw199 <- classify(data199, "1:199")

raw <- rbind(raw4, raw9, raw19, raw99, raw199)
raw$imbalance <- factor(raw$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))

raw_spect <- classify(spectf_heart, "1:3.84")
raw_fd <- classify(fraud_detection, "1:577.9")



### RESAMPLING METHODS

### resampling_cv - function
###
### classifies data with 5 fold cv while applying the resampling method to the 
### training data in each fold. The classifying learner and the performance measure 
### is to be specified
### Input:
###   data      A dataframe. The data to be classified
###   lrnr      An object of mlr3-class Learner. The learner to be trained
###   perf      An object of mlr3-class Measure. The measure of the performance evaluation.
###   resample  A "bimba"-method. The resampling method
### Output:
###   A numeric. The aggregated performance measure


resampling_cv <- function(data, lrnr, resample) {
  #manual 5-fold CV
  splits <- split(data, sample(rep(1:5, times = rep(2000, times = 5))))
  results <- data.frame(matrix(rep(0, times = 15), ncol = 3, nrow = 5))
  for(i in 1:5){
    #define train and test for split
    train <- data.frame()
    trainsets <- 1:5
    trainsets <- trainsets[!trainsets %in% c(i)]
    for(j in 1:4){
      train <- rbind(train, splits[[j]])
    }
    test <- splits[[i]]
    
    #apply smote to training data
    train <- resample(train)
    
    #learn model with new training data
    task <- TaskClassif$new("train", train, "class", positive = "1")
    learner <- lrnr
    learner$train(task)
    
    #predict test data with learner
    prediction <- learner$predict_newdata(test)
    results[i,1] <- prediction$score(msr("classif.bacc"))
    results[i,2] <- prediction$score(msr("classif.recall"))
    results[i,3] <- prediction$score(msr("classif.fbeta"))
  }
  #aggregate results
  output <- rep(0, times = 3)
  for(i in 1:3){
    output[i] <- mean(results[,i])
  }
  output
}


### classify_resampling - function
###
### classifies a (imbalanced) data set according to 4 classifiers and evaluates 
### performance with 5-fold cv and 3 performance metrics while resampling the data
### Input:
###   data    A dataframe. The data to be classified
###   resample  A "bimba"-method. The resampling method
###   imb   A string. The data imbalance
### Output:
###   A dataframe with the performance scores for each learner

classify_resampling <- function(data, resample, imb) {
  #define output dataframe
  output <- data.frame(matrix(rep(0, times = 36), ncol = 4, nrow = 9))
  colnames(output) <- c("classifier", "value", "imbalance", "performance")
  output[,1] <- c(rep("NB", times = 3), rep("RF", times = 3), rep("kNN", times = 3))
  output[,3] <- rep(imb, times = 9)
  output[,4] <- rep(c("bACC", "F1", "Recall"), times = 3) 
  
  #naive bayes
  measures <- resampling_cv(data, lrn("classif.naive_bayes"), resample)
  output[1,2] <- measures[1]
  output[2,2] <- measures[2]
  output[3,2] <- measures[3]
  
  #rf
  measures <- resampling_cv(data, lrn("classif.ranger"), resample)
  output[4,2] <- measures[1]
  output[5,2] <- measures[2]
  output[6,2] <- measures[3]
  
  #knn
  measures <- resampling_cv(data, lrn("classif.kknn"), resample)
  output[7,2] <- measures[1]
  output[8,2] <- measures[2]
  output[9,2] <- measures[3]
  
  output
}

### SMOTE

smote4 <- classify_resampling(data4, SMOTE, "1:4")
smote9 <- classify_resampling(data9, SMOTE, "1:9")
smote19 <- classify_resampling(data19, SMOTE, "1:19")
smote99 <- classify_resampling(data99, SMOTE, "1:99")
smote199 <- classify_resampling(data199, SMOTE, "1:199")

smote <- rbind(smote4, smote9, smote19, smote99, smote199)
smote$imbalance <- factor(smote$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))

smote_spect <- classify_resampling(spectf_heart, SMOTE, "1:3.84")
smote_fd <- classify_resampling(fraud_detection, SMOTE, "1:577.9")


### SBC

sbc4 <- classify_resampling(data4, SBC, "1:4")
sbc9 <- classify_resampling(data9, SBC, "1:9")
sbc19 <- classify_resampling(data19, SBC, "1:19")
sbc99 <- classify_resampling(data99, SBC, "1:99")
sbc199 <- classify_resampling(data199, SBC, "1:199")

sbc <- rbind(sbc4, sbc9, sbc19, sbc99, sbc199)
sbc$imbalance <- factor(sbc$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))

sbc_spect <- classify_resampling(spectf_heart, SBC, "1:3.84")
sbc_fd <- classify_resampling(fraud_detection, SBC, "1:577.9")


### ROS

ros4 <- classify_resampling(data4, ROS, "1:4")
ros9 <- classify_resampling(data9, ROS, "1:9")
ros19 <- classify_resampling(data19, ROS, "1:19")
ros99 <- classify_resampling(data99, ROS, "1:99")
ros199 <- classify_resampling(data199, ROS, "1:199")

ros <- rbind(ros4, ros9, ros19, ros99, ros199)
ros$imbalance <- factor(ros$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))

ros_spect <- classify_resampling(spectf_heart, ROS, "1:3.84")
ros_fd <- classify_resampling(fraud_detection, ROS, "1:577.9")


### RUS

rus4 <- classify_resampling(data4, RUS, "1:4")
rus9 <- classify_resampling(data9, RUS, "1:9")
rus19 <- classify_resampling(data19, RUS, "1:19")
rus99 <- classify_resampling(data99, RUS, "1:99")
rus199 <- classify_resampling(data199, RUS, "1:199")

rus <- rbind(rus4, rus9, rus19, rus99, rus199)
rus$imbalance <- factor(rus$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))

rus_spect <- classify_resampling(spectf_heart, RUS, "1:3.84")
rus_fd <- classify_resampling(fraud_detection, RUS, "1:577.9")

### HYPERPARAMETER TUNING
detach("package:mlr3measures", unload = TRUE)

### hyper_resampling_cv - function
###
### classifies data with 5 fold cv while applying the resampling method to the 
### training data in each fold. The classifying learner is to be specified
### Input:
###   data      A dataframe. The data to be classified
###   lrnr      A string. The learner to be trained
###   resample  A "bimba"-method. The resampling method
### Output:
###   A numeric. The aggregated performance measure


hyper_resampling_cv <- function(data, lrnr, resample) {
  #manual 5-fold CV
  splits <- split(data, sample(rep(1:5, times = rep(2000, times = 5))))
  results <- data.frame(matrix(rep(0, times = 15), ncol = 3, nrow = 5))
  for(i in 1:5){
    #define train and test for split
    train <- data.frame()
    trainsets <- 1:5
    trainsets <- trainsets[!trainsets %in% c(i)]
    for(j in 1:4){
      train <- rbind(train, splits[[j]])
    }
    test <- splits[[i]]
    
    #apply smote to training data
    train <- resample(train)
    
    #learn model with new training data while tuning hyperparameters
    task <- TaskClassif$new("train", train, "class", positive = "1")
    tuner <- tnr("grid_search")
    terminator <- trm("run_time", secs = 30)
    
    if(lrnr == "rf"){
      learner <- lrn("classif.ranger", max.depth = to_tune(1,35), num.trees = to_tune(1,2000))
    } else if(lrnr == "knn") {
      learner <- lrn("classif.kknn", k = to_tune(3,30))
    }
    
    #initialize auto-tuning
    auto_tune <- auto_tuner(tuner = tuner, learner = learner, rsmp("cv", folds = 3), 
                            measure = msr("classif.ce"), terminator = terminator)
    auto_tune$train(task)
    
    #predict test data with learner
    prediction <- auto_tune$predict_newdata(test)
    results[i,1] <- prediction$score(msr("classif.bacc"))
    results[i,2] <- prediction$score(msr("classif.recall"))
    results[i,3] <- prediction$score(msr("classif.fbeta"))
  }
  #aggregate results
  output <- rep(0, times = 3)
  for(i in 1:3){
    output[i] <- mean(results[,i])
  }
  output
}


### classify_hyper_resampling - function
###
### classifies a (imbalanced) data set according to 4 classifiers and evaluates 
### performance with 5-fold cv and 3 performance metrics while resampling the data
### and performing hyperparameter tuning on each fold
### Input:
###   data    A dataframe. The data to be classified
###   resample  A "bimba"-method. The resampling method
###   imb   A string. The data imbalance
### Output:
###   A dataframe with the performance scores for each learner

classify_hyper_resampling <- function(data, resample, imb) {
  #define output dataframe
  output <- data.frame(matrix(rep(0, times = 24), ncol = 4, nrow = 6))
  colnames(output) <- c("classifier", "value", "imbalance", "performance")
  output[,1] <- c(rep("RF", times = 3), rep("kNN", times = 3))
  output[,3] <- rep(imb, times = 6)
  output[,4] <- rep(c("bACC", "Recall", "F1"), times = 2) 
  
  
  #rf
  measures <- hyper_resampling_cv(data, "rf", resample)
  output[1,2] <- measures[1]
  output[2,2] <- measures[2]
  output[3,2] <- measures[3]
  
  #knn
  measures <- hyper_resampling_cv(data, "knn", resample)
  output[4,2] <- measures[1]
  output[5,2] <- measures[2]
  output[6,2] <- measures[3]
  
  output
}

### SMOTE + Hyperparametertuning

hyper_smote4 <- classify_hyper_resampling(data4, SMOTE, "1:4")
hyper_smote9 <- classify_hyper_resampling(data9, SMOTE, "1:9")
hyper_smote19 <- classify_hyper_resampling(data19, SMOTE, "1:19")
hyper_smote99 <- classify_hyper_resampling(data99, SMOTE, "1:99")
hyper_smote199 <- classify_hyper_resampling(data199, SMOTE, "1:199")

hyper_smote <- rbind(hyper_smote4, hyper_smote9, hyper_smote19, hyper_smote99, hyper_smote199)
hyper_smote$imbalance <- factor(hyper_smote$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))

hyper_smote_spect <- classify_hyper_resampling(spectf_heart, SMOTE, "1:3.84")
hyper_smote_fd <- classify_hyper_resampling(fraud_detection, SMOTE, "1:577.9")

comp_hyper_smote <- rbind(hyper_smote, smote)
comp_hyper_smote <- comp_hyper_smote[comp_hyper_smote$performance == "bACC",]
comp_hyper_smote <- comp_hyper_smote[!(comp_hyper_smote$classifier == "NB"),]
comp_hyper_smote$hyper <- c(rep("yes", times = 10), rep("no", times = 10))
comp_hyper_smote_rf <- comp_hyper_smote[comp_hyper_smote$classifier == "RF",]
comp_hyper_smote_knn <- comp_hyper_smote[comp_hyper_smote$classifier == "kNN",]

### SBC + Hyperparametertuning

hyper_sbc4 <- classify_hyper_resampling(data4, SBC, "1:4")
hyper_sbc9 <- classify_hyper_resampling(data9, SBC, "1:9")
hyper_sbc19 <- classify_hyper_resampling(data19, SBC, "1:19")
hyper_sbc99 <- classify_hyper_resampling(data99, SBC, "1:99")
hyper_sbc199 <- classify_hyper_resampling(data199, SBC, "1:199")

hyper_sbc <- rbind(hyper_sbc4, hyper_sbc9, hyper_sbc19, hyper_sbc99, hyper_sbc199)
hyper_sbc$imbalance <- factor(hyper_sbc$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))

hyper_sbc_spect <- classify_hyper_resampling(spectf_heart, SBC, "1:3.84")
hyper_sbc_fd <- classify_hyper_resampling(fraud_detection, SBC, "1:577.9")

comp_hyper_sbc <- rbind(hyper_sbc, sbc)
comp_hyper_sbc <- comp_hyper_sbc[comp_hyper_sbc$performance == "bACC",]
comp_hyper_sbc <- comp_hyper_sbc[!(comp_hyper_sbc$classifier == "NB"),]
comp_hyper_sbc$hyper <- c(rep("yes", times = 10), rep("no", times = 10))
comp_hyper_sbc_rf <- comp_hyper_sbc[comp_hyper_sbc$classifier == "RF",]
comp_hyper_sbc_knn <- comp_hyper_sbc[comp_hyper_sbc$classifier == "kNN",]

### COMPARISON 1

comp1 <- data.frame(matrix(rep(0, times = 75), ncol = 3, nrow = 25))
colnames(comp1) <- c("method", "imbalance", "bACC")
comp1[,1] <- c(rep("No Resampling", times = 5), rep("SMOTE", times = 5), 
               rep("ROS", times = 5), rep("SBC", times = 5), 
               rep("RUS", times = 5))
comp1[,2] <- rep(c("1:4", "1:9", "1:19", "1:99", "1:199"), times = 5) 
comp1[,3] <- c(raw[4,2], raw[13,2], raw[22,2], raw[31,2], raw[40,2],
               smote[4,2], smote[13,2], smote[22,2], smote[31,2], smote[40,2],
               ros[4,2], ros[13,2], ros[22,2], ros[31,2], ros[40,2],
               sbc[4,2], sbc[13,2], sbc[22,2], sbc[31,2], sbc[40,2],
               rus[4,2], rus[13,2], rus[22,2], rus[31,2], rus[40,2])
comp1$imbalance <- factor(comp1$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))
comp1$method <- factor(comp1$method, levels = c("No Resampling", "SMOTE", "ROS", "SBC", "RUS"))



library(mlr3measures)

### HYBRID

### hyb_resampling_cv - function
###
### classifies data with 5 fold cv while applying the resampling method to the 
### training data in each fold. The classifying learner and the performance measure 
### is to be specified
### Input:
###   data      A dataframe. The data to be classified
###   lrnr      An object of mlr3-class Learner. The learner to be trained
###   perf      An object of mlr3-class Measure. The measure of the performance evaluation.
###   oversample  A "bimba"-method. The oversampling method
###   undersample  A "bimba"-method. The undersampling method
### Output:
###   A numeric. The aggregated performance measure

hyb_resampling_cv <- function(data, lrnr, oversample, undersample) {
  #manual 5-fold CV
  splits <- split(data, sample(rep(1:5, times = rep(2000, times = 5))))
  results <- data.frame(matrix(rep(0, times = 15), ncol = 3, nrow = 5))
  for(i in 1:5){
    #define train and test for split
    train <- data.frame()
    trainsets <- 1:5
    trainsets <- trainsets[!trainsets %in% c(i)]
    for(j in 1:4){
      train <- rbind(train, splits[[j]])
    }
    test <- splits[[i]]
    
    #apply smote to training data
    train <- oversample(train, 50)
    train <- undersample(train, 33.33)
    
    #learn model with new training data
    task <- TaskClassif$new("train", train, "class", positive = "1")
    learner <- lrnr
    learner$train(task)
    
    #predict test data with learner
    prediction <- learner$predict_newdata(test)
    results[i,1] <- prediction$score(msr("classif.bacc"))
    results[i,2] <- prediction$score(msr("classif.recall"))
    results[i,3] <- prediction$score(msr("classif.fbeta"))
  }
  #aggregate results
  output <- rep(0, times = 3)
  for(i in 1:3){
    output[i] <- mean(results[,i])
  }
  output
}


### hyb_classify_resampling - function
###
### classifies a (imbalanced) data set according to 4 classifiers and evaluates 
### performance with 5-fold cv and 3 performance metrics while resampling the data
### with one oversampling and one undersampling method
### Input:
###   data    A dataframe. The data to be classified
###   oversample  A "bimba"-method. The oversampling method
###   undersample  A "bimba"-method. The undersampling method
###   imb   A string. The data imbalance
### Output:
###   A dataframe with the performance scores for each learner

hyb_classify_resampling <- function(data, oversample, undersample, imb) {
  #define output dataframe
  output <- data.frame(matrix(rep(0, times = 36), ncol = 4, nrow = 9))
  colnames(output) <- c("classifier", "value", "imbalance", "performance")
  output[,1] <- c(rep("NB", times = 3), rep("RF", times = 3), rep("kNN", times = 3))
  output[,3] <- rep(imb, times = 9)
  output[,4] <- rep(c("bACC", "Recall", "F1"), times = 3) 
  
  
  #naive bayes
  measures <- hyb_resampling_cv(data, lrn("classif.naive_bayes"), oversample, undersample)
  output[1,2] <- measures[1]
  output[2,2] <- measures[2]
  output[3,2] <- measures[3]
  
  #rf
  measures <- hyb_resampling_cv(data, lrn("classif.ranger"), oversample, undersample)
  output[4,2] <- measures[1]
  output[5,2] <- measures[2]
  output[6,2] <- measures[3]
  
  #knn
  measures <- hyb_resampling_cv(data, lrn("classif.kknn"), oversample, undersample)
  output[7,2] <- measures[1]
  output[8,2] <- measures[2]
  output[9,2] <- measures[3]
  
  output
}

### SMOTE + SBC

smotesbc4 <- hyb_classify_resampling(data4, SMOTE, SBC, "1:4")
smotesbc9 <- hyb_classify_resampling(data9, SMOTE, SBC, "1:9")
smotesbc19 <- hyb_classify_resampling(data19, SMOTE, SBC, "1:19")
smotesbc99 <- hyb_classify_resampling(data99, SMOTE, SBC, "1:99")
smotesbc199 <- hyb_classify_resampling(data199, SMOTE, SBC, "1:199")

smotesbc <- rbind(smotesbc4, smotesbc9, smotesbc19, smotesbc99, smotesbc199)
smotesbc$imbalance <- factor(smotesbc$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))

smotesbc_spect <- hyb_classify_resampling(spectf_heart, SMOTE, SBC, "1:3.84")
smotesbc_fd <- hyb_classify_resampling(fraud_detection, SMOTE, SBC, "1:577.9")

### SMOTE + RUS

smoterus4 <- hyb_classify_resampling(data4, SMOTE, RUS, "1:4")
smoterus9 <- hyb_classify_resampling(data9, SMOTE, RUS, "1:9")
smoterus19 <- hyb_classify_resampling(data19, SMOTE, RUS, "1:19")
smoterus99 <- hyb_classify_resampling(data99, SMOTE, RUS, "1:99")
smoterus199 <- hyb_classify_resampling(data199, SMOTE, RUS, "1:199")

smoterus <- rbind(smoterus4, smoterus9, smoterus19, smoterus99, smoterus199)
smoterus$imbalance <- factor(smoterus$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))

smoterus_spect <- hyb_classify_resampling(spectf_heart, SMOTE, RUS, "1:3.84")
smoterus_fd <- hyb_classify_resampling(spectf_heart, SMOTE, RUS, "1:3.84")



### Combination of Bagging and Resampling Methods


### bag_resampling_cv - functions
###
### classifies data with 5 fold cv while applying the hybrid method to the 
### training data in each fold. The performance measure is to be specified.
### Input:
###   data      A dataframe. The data to be classified
###   perf      An object of mlr3-class Measure. The measure of the performance evaluation.
###   method    A "ebmc"-method. The hybrid method
###   alg       A string. The algorithm for the weak learners.
### Output:
###   A numeric. The aggregated performance measure

bag_resampling_cv <- function(data, method, alg) {
  #manual 5-fold CV
  splits <- split(data, sample(rep(1:5, times = rep(2000, times = 5))))
  results <- data.frame(matrix(rep(0, times = 15), ncol = 3, nrow = 5))
  for(i in 1:5){
    #define train and test for split
    train <- data.frame()
    trainsets <- 1:5
    trainsets <- trainsets[!trainsets %in% c(i)]
    for(j in 1:4){
      train <- rbind(train, splits[[j]])
    }
    test <- splits[[i]]
    
    #apply method to training data and learn an ensemble model of weak learners
    model <- method(class ~., data = train, size = 100, alg = alg)
    
    #predict test data with ensemble model
    prediction <- predict.modelBag(model,newdata = test, type = "class" )
    
    results[i,1] <- bacc(test$class, prediction)
    results[i,2] <- fbeta(test$class, prediction, positive = "1")
    results[i,3] <- recall(test$class, prediction, positive = "1")
  }
  #aggregate results
  output <- rep(0, times = 3)
  for(i in 1:3){
    output[i] <- mean(results[,i])
  }
  output
}


### bag_classify_resampling - function
###
### classifies a (imbalanced) data set according to 2 classifiers and evaluates 
### performance with 5-fold cv and 3 performance metrics while using a hybrid approach
### of a resampling method and bagging
### Input:
###   data    A dataframe. The data to be classified
###   method  A "ebmc"-method. The hybrid method
###   imb   A string. The data imbalance
### Output:
###   A dataframe with the performance scores for each learner

bag_classify_resampling <- function(data, method, imb) {
  #define output dataframe
  output <- data.frame(matrix(rep(0, times = 24), ncol = 4, nrow = 6))
  colnames(output) <- c("classifier", "value", "imbalance", "performance")
  output[,1] <- c(rep("NB", times = 3), rep("CART", times = 3))
  output[,3] <- rep(imb, times = 6)
  output[,4] <- rep(c("bACC", "F1", "Recall"), times = 2) 
  
  
  #naive bayes
  measures <- bag_resampling_cv(data, method, "nb")
  output[1,2] <- measures[1]
  output[2,2] <- measures[2]
  output[3,2] <- measures[3]
  
  #cart
  measures <- bag_resampling_cv(data, method, "cart")
  output[4,2] <- measures[1]
  output[5,2] <- measures[2]
  output[6,2] <- measures[3]
  
  output
}

### SMOTE + Bagging

smotebag4 <- bag_classify_resampling(data4, sbag, "1:4")
smotebag9 <- bag_classify_resampling(data9, sbag, "1:9")
smotebag19 <- bag_classify_resampling(data19, sbag, "1:19")
smotebag99 <- bag_classify_resampling(data99, sbag, "1:99")
smotebag199 <- bag_classify_resampling(data199, sbag, "1:199")

smotebag <- rbind(smotebag4, smotebag9, smotebag19, smotebag99, smotebag199)
smotebag$imbalance <- factor(smotebag$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))

smotebag_spect <- classify_hyper_resampling(spectf_heart, SBC, "1:3.84")
smotebag_fd <- classify_hyper_resampling(fraud_detection, SBC, "1:577.9")


### RUS + Bagging

rusbag4 <- bag_classify_resampling(data4, ub, "1:4")
rusbag9 <- bag_classify_resampling(data9, ub, "1:9")
rusbag19 <- bag_classify_resampling(data19, ub, "1:19")
rusbag99 <- bag_classify_resampling(data99, ub, "1:99")
rusbag199 <- bag_classify_resampling(data199, ub, "1:199")

rusbag <- rbind(rusbag4, rusbag9, rusbag19, rusbag99, rusbag199)
rusbag$imbalance <- factor(rusbag$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))

rusbag_spect <- classify_hyper_resampling(spectf_heart, SBC, "1:3.84")
rusbag_fd <- classify_hyper_resampling(fraud_detection, SBC, "1:577.9")


### COMPARISON 2

comp2 <- data.frame(matrix(rep(0, times = 90), ncol = 3, nrow = 30))
colnames(comp2) <- c("method", "imbalance", "bACC")
comp2[,1] <- c(rep("SMOTE", times = 5), rep("SBC", times = 5), 
               rep("SMOTE + SBC", times = 5), rep("SMOTE + RUS", times = 5), 
               rep("SMOTE + Bagging", times = 5), rep("RUS + Bagging", times = 5))
comp2[,2] <- rep(c("1:4", "1:9", "1:19", "1:99", "1:199"), times = 6) 
comp2[,3] <- c(smote[4,2], smote[13,2], smote[22,2], smote[31,2], smote[40,2],
               sbc[4,2], sbc[13,2], sbc[22,2], sbc[31,2], sbc[40,2],
               smotesbc[4,2], smotesbc[13,2], smotesbc[22,2], smotesbc[31,2], smotesbc[40,2],
               smoterus[4,2], smoterus[13,2], smoterus[22,2], smoterus[31,2], smoterus[40,2],
               smotebag[4,2], smotebag[10,2], smotebag[16,2], smotebag[22,2], smotebag[28,2],
               rusbag[4,2], rusbag[10,2], rusbag[16,2], rusbag[22,2], rusbag[28,2])
comp2$imbalance <- factor(comp2$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))
comp2$method <- factor(comp2$method, levels = c("SMOTE", "SBC", "SMOTE + SBC", "SMOTE + RUS", 
                                                "SMOTE + Bagging", "RUS + Bagging"))
