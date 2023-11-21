## Bachelor's Thesis
# A Comparison of Machine Learning Methods for Dealing with Imbalanced Data
Nina Mayer

Supervisor: Dr. Ludwig Bothmann


### Abstract
The classification of imbalanced data has been an issue due to the bias towards
the majority class. Therefore, many methods to resolve this problem have
been developed. In this thesis, the Synthetic Oversampling Technique (SMOTE),
Cluster-based Undersampling (SBC), and Bagging are presented and then applied
to simulated as well as real data sets with varying imbalance ratios. Without applying
these tailored methods, the performance of a model decreases with increasing
data imbalance. The models are trained by three classifying learners: k-Nearest
Neighbor (kNN), Random Forests (RF), and Naive Bayes (NB). In the application
process, models trained with NB on simulated data sets seem to perform worse than
those trained with RF or kNN, which both perform similarly. These methods tailored
for dealing with imbalanced data are applied individually and in combination
as hybrid methods. The results of the analysis show that SMOTE seems to work
best for simulated data. With the real data sets undersampling methods perform
better. By combining SMOTE with Random Undersampling as a hybrid method
an improvement of the performance can be observed in some cases. SBC does also
improve the performance of models, but can’t keep up with SMOTE or Random
Undersampling. Combining Bagging with Resampling methods does not seem to
achieve an enhancement on the simulated data sets but still on the real data sets.
I

### Code
The Code and more for the Bachelor's Thesis "A Comparison of Machine Learning Methods for Dealing with Imbalanced Data" is provided in this repository. The folders contain the following content:
+ code: The files that have the code available
+ data: The data that was used for classification
+ figures: The figures that were shown in the thesis
