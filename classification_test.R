library(mlr3)
library(mlr3learners)
library(kknn)

###grade_prediction

#task
gp_task <- TaskClassif$new("grade_prediction", grade_prediction, "pass")
mlr_tasks$add("grade_prediction", gp_task)

#desicion tree
gp_learner <- lrn("classif.rpart")
gp_learner$train(gp_task)
gp_learner$model

mlr3

#resampling
cv5 = rsmp("cv", folds = 5)
gp_rr <- resample(gp_task, gp_learner, cv5)
gp_rr$aggregate(msr("classif.ce"))
gp_rr$prediction()

gp_scores <- gp_rr$score()
gp_scores[, c("iteration", "classif.ce")]


###thoracic_surgery

#task
ts_task <- TaskClassif$new("thoracic_surgery", thoracic_surgery, "Class")
mlr_tasks$add("thoracic_surgery", ts_task)

#decision tree
ts_learner <- lrn("classif.rpart")
ts_learner$train(ts_task)
ts_learner$model

#resampling
cv5 = rsmp("cv", folds = 5)
ts_rr <- resample(ts_task, ts_learner, cv5)
ts_rr$aggregate(msr("classif.ce"))
ts_rr$prediction()

ts_scores <- ts_rr$score()
ts_scores[, c("iteration", "classif.ce")]


###benchmark

learners <- list(lrn("classif.rpart"), lrn("classif.kknn"))
tasks <- list(tsk("grade_prediction"), tsk("thoracic_surgery"))

design <- benchmark_grid(tasks, learners, cv5)
bmr <- benchmark(design)
