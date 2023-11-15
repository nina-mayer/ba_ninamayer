library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlr3measures)
library(mlr3tuning)

###grade_prediction

#task
gp_task <- TaskClassif$new("grade_prediction", grade_prediction, "pass")
mlr_tasks$add("grade_prediction", gp_task)

#desicion tree
gp_learner <- lrn("classif.rpart")
gp_learner$train(gp_task)
gp_learner$model

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



###law_school_admission

#task
lsa_task <- TaskClassif$new("law_school_admission", law_school_admission, "ugpagt3")
mlr_tasks$add("law_school_admission", lsa_task)

#decision tree
lsa_learner <- lrn("classif.rpart")
lsa_learner$train(lsa_task)
lsa_learner$model

#resampling
cv5 = rsmp("cv", folds = 5)
lsa_rr <- resample(lsa_task, lsa_learner, cv5)
lsa_rr$aggregate(msr("classif.ce"))
lsa_rr$prediction()

lsa_scores <- lsa_rr$score()
lsa_scores[, c("iteration", "classif.ce")]



###good_customer

#task
gc_task <- TaskClassif$new("good_customer", good_customer, "bad_client_target")
mlr_tasks$add("good_customer", gc_task)

#decision tree
gc_learner <- lrn("classif.rpart")
gc_learner$train(gc_task)
gc_learner$model

#resampling
cv5 = rsmp("cv", folds = 5)
gc_rr <- resample(gc_task, gc_learner, cv5)
gc_rr$aggregate(msr("classif.ce"))
gc_rr$prediction()

gc_scores <- gc_rr$score()
gc_scores[, c("iteration", "classif.ce")]



###benchmark

learners <- list(lrn("classif.rpart"), lrn("classif.kknn"), lrn("classif.ranger"))
tasks <- list(tsk("grade_prediction"), tsk("thoracic_surgery"), tsk("law_school_admission"), tsk("good_customer"))

design <- benchmark_grid(tasks, learners, cv5)
bmr <- benchmark(design)

bmr_ag <- bmr$aggregate()
bmr_ag[, c("task_id", "learner_id", "classif.ce")]

autoplot(bmr)


### hyperparametertuning

task <- TaskClassif$new("train", train_smote, "class")
lrnr <- lrn("classif.kknn", k = to_tune(1,10))
tuner <- tnr("grid_search", batch_size = 5)
terminator <- trm("stagnation", iters = 3, threshold = 0.01)

instance <- ti(task = task, learner = lrnr, resampling = cv5, terminator = terminator)
tuner$optimize(instance)
instance$result$learner_param_vals
lrnr_tuned <- lrn("classif.kknn")
lrnr_tuned$param_set$values <- instance$result_learner_param_vals
lrnr_tuned$train(task)
at <- auto_tuner(tuner = tuner, learner = lrnr, resampling = cv5, measure = msr("classif.ce"))
at$train(task)
prediction <- at$predict_newdata(test)
prediction$score(msr("classif.bacc"))
prediction$score(msr("classif.recall"))
prediction$score(msr("classif.fbeta"))
