## Integrated Learners




| Id | Name | Short | Packages | prob / se  | Num. | Fact. | NAs | Weights | Further | Note |
| :---:| :---:| :---:| :---:| :---:| :---:| :---:| :---:| :---:| :---:| :---:|
| classif.ada | ada Boosting | ada | [ada](http://cran.r-project.org/web/packages/ada/index.html) | X | X | X |  | X | twoclass,multiclass |  |
| classif.blackboost | Gradient boosting with regression trees | blackbst | [mboost](http://cran.r-project.org/web/packages/mboost/index.html), [party](http://cran.r-project.org/web/packages/party/index.html) | X | X | X | X | X | twoclass |  |
| classif.boosting | Adabag Boosting | adabag | [adabag](http://cran.r-project.org/web/packages/adabag/index.html) | X | X | X | X |  | twoclass,multiclass | Note that \code{xval} has been set to 0 by default for speed. |
| classif.cforest | Random forest based on conditional inference trees | cforest | [party](http://cran.r-project.org/web/packages/party/index.html) | X | X | X |  | X | twoclass,multiclass |  |
| classif.ctree | Conditional Inference Trees | ctree | [party](http://cran.r-project.org/web/packages/party/index.html) | X | X | X | X | X | twoclass,multiclass |  |
| classif.fnn | Fast k-Nearest Neighbor | fnn | [FNN](http://cran.r-project.org/web/packages/FNN/index.html) |  | X |  |  |  | twoclass,multiclass |  |
| classif.gbm | Gradient boosting machine | gbm | [gbm](http://cran.r-project.org/web/packages/gbm/index.html) | X | X | X | X | X | twoclass,multiclass |  |
| classif.geoDA | Geometric Predictive Discriminant Analysis | geoDA | [DiscriMiner](http://cran.r-project.org/web/packages/DiscriMiner/index.html) |  | X |  |  |  | twoclass,multiclass |  |
| classif.glmboost | Boosting for GLMs | glmbst | [mboost](http://cran.r-project.org/web/packages/mboost/index.html) | X | X | X |  | X | twoclass | Note that \code{family} has been set to \code{Binomial()} by default. |
| classif.glmnet | GLM with lasso or elasticnet regularization | glmnet | [glmnet](http://cran.r-project.org/web/packages/glmnet/index.html) | X | X |  |  | X | twoclass,multiclass |  |
| classif.IBk | K-nearest neighbours | IBk | [RWeka](http://cran.r-project.org/web/packages/RWeka/index.html) | X | X | X |  |  | twoclass,multiclass |  |
| classif.J48 | J48 Decision Trees | J48 | [RWeka](http://cran.r-project.org/web/packages/RWeka/index.html) | X | X | X | X |  | twoclass,multiclass | Note that NAs are directly passed to WEKA with \code{na.action = na.pass} |
| classif.JRip | Propositional Rule Learner | JRip | [RWeka](http://cran.r-project.org/web/packages/RWeka/index.html) | X | X | X | X |  | twoclass,multiclass | Note that NAs are directly passed to WEKA with \code{na.action = na.pass} |
| classif.kknn | k-Nearest Neighbor | kknn | [kknn](http://cran.r-project.org/web/packages/kknn/index.html) | X | X | X |  |  | twoclass,multiclass |  |
| classif.knn | k-Nearest Neighbor | knn | [class](http://cran.r-project.org/web/packages/class/index.html) |  | X |  |  |  | twoclass,multiclass |  |
| classif.ksvm | Support Vector Machines | ksvm | [kernlab](http://cran.r-project.org/web/packages/kernlab/index.html) | X | X | X |  |  | twoclass,multiclass | Note that kernel parameters have to be passed directly and not by using the kpar list in ksvm.\cr Note that \code{fit} has been set to \code{FALSE} by default for speed. |
| classif.lda | Linear Discriminant Analysis | lda | [MASS](http://cran.r-project.org/web/packages/MASS/index.html) | X | X | X |  |  | twoclass,multiclass |  |
| classif.LiblineaRBinary | Regularized Binary Linear Predictive Models Estimation | LiblineaRBinary | [LiblineaR](http://cran.r-project.org/web/packages/LiblineaR/index.html) |  | X |  |  |  | twoclass | Note that this model subsumes the types 1,2,3,5. |
| classif.LiblineaRLogReg | Regularized Logistic Regression | RegLReg | [LiblineaR](http://cran.r-project.org/web/packages/LiblineaR/index.html) | X | X |  |  |  | twoclass | Note that this model subsumes type 0,6,7. |
| classif.LiblineaRMultiClass | Multi-class Support Vector Classification by Crammer and Singer | mcsvc | [LiblineaR](http://cran.r-project.org/web/packages/LiblineaR/index.html) |  | X |  |  |  | twoclass,multiclass | Note that this model is type 4. |
| classif.linDA | Linear Discriminant Analysis | linDA | [DiscriMiner](http://cran.r-project.org/web/packages/DiscriMiner/index.html) |  | X |  |  |  | twoclass,multiclass |  |
| classif.logreg | Logistic Regression | logreg | stats | X | X | X |  | X | twoclass,multiclass |  |
| classif.lqa | classif.lqa | lqa | [lqa](http://cran.r-project.org/web/packages/lqa/index.html) | X | X |  |  | X | twoclass | Note that \code{penalty} has been set to \dQuote{lasso} and \code{lambda} to 0.1 by default. |
| classif.lssvm | Least Squares Support Vector Machine | lssvm | [kernlab](http://cran.r-project.org/web/packages/kernlab/index.html) |  | X | X |  |  | twoclass,multiclass | Note that \code{fitted} has been set to \code{FALSE} by default for speed. |
| classif.lvq1 | Learning Vector Quantization | lvq1 | [class](http://cran.r-project.org/web/packages/class/index.html) |  | X |  |  |  | twoclass,multiclass |  |
| classif.mda | Mixture Discriminant Analysis | mda | [mda](http://cran.r-project.org/web/packages/mda/index.html) | X | X | X |  |  | twoclass,multiclass | Note that \code{keep.fitted} has been set to \code{FALSE} by default for speed. |
| classif.multinom | Multinomial Regression | multinom | [nnet](http://cran.r-project.org/web/packages/nnet/index.html) | X | X | X |  | X | twoclass,multiclass |  |
| classif.naiveBayes | Naive Bayes | nBayes | [e1071](http://cran.r-project.org/web/packages/e1071/index.html) | X | X | X | X |  | twoclass,multiclass |  |
| classif.nnet | Neural Network | nnet | [nnet](http://cran.r-project.org/web/packages/nnet/index.html) | X | X | X |  | X | twoclass,multiclass | Note that \code{size} has been set to 3 by default. |
| classif.OneR | 1-R classifier | OneR | [RWeka](http://cran.r-project.org/web/packages/RWeka/index.html) | X | X | X | X |  | twoclass,multiclass | Note that NAs are directly passed to WEKA with \code{na.action = na.pass} |
| classif.PART | PART decision lists | PART | [RWeka](http://cran.r-project.org/web/packages/RWeka/index.html) | X | X | X | X |  | twoclass,multiclass | Note that NAs are directly passed to WEKA with \code{na.action = na.pass} |
| classif.plr | Logistic regression with a L2 penalty | plr | [stepPlr](http://cran.r-project.org/web/packages/stepPlr/index.html) | X | X | X |  | X | twoclass | Note that AIC and BIC penalty types can be selected via the new parameter \code{cp.type} |
| classif.plsDA | Partial Least Squares (PLS) Discriminant Analysis | plsDA | [DiscriMiner](http://cran.r-project.org/web/packages/DiscriMiner/index.html) |  | X |  |  |  | twoclass,multiclass |  |
| classif.plsdaCaret | Partial Least Squares (PLS) Discriminant Analysis | plsdaCaret | [caret](http://cran.r-project.org/web/packages/caret/index.html) | X | X |  |  |  | twoclass |  |
| classif.qda | Quadratic Discriminant Analysis | qda | [MASS](http://cran.r-project.org/web/packages/MASS/index.html) | X | X | X |  |  | twoclass,multiclass |  |
| classif.quaDA | Quadratic Discriminant Analysis | quaDA | [DiscriMiner](http://cran.r-project.org/web/packages/DiscriMiner/index.html) |  | X |  |  |  | twoclass,multiclass |  |
| classif.randomForest | Random Forest | RF | [randomForest](http://cran.r-project.org/web/packages/randomForest/index.html) | X | X | X |  |  | twoclass,multiclass | The argument \code{fix.factors} restores the factor levels seen in the training data before prediction to circumvent randomForest's internal sanity checks. Default is \code{FALSE}. |
| classif.randomForestSRC | Random Forest | RFsrc | [randomForestSRC](http://cran.r-project.org/web/packages/randomForestSRC/index.html) | X | X | X | X |  | twoclass,multiclass |  |
| classif.rda | Regularized Discriminant Analysis | rda | [klaR](http://cran.r-project.org/web/packages/klaR/index.html) | X | X | X |  |  | twoclass,multiclass | Note that \code{estimate.error} has been set to \code{FALSE} by default for speed. |
| classif.rpart | Decision Tree | rpart | [rpart](http://cran.r-project.org/web/packages/rpart/index.html) | X | X | X | X | X | twoclass,multiclass | Note that \code{xval} has been set to 0 by default for speed. |
| classif.rrlda | classif.rrlda | rrlda | [rrlda](http://cran.r-project.org/web/packages/rrlda/index.html) |  | X |  |  |  | twoclass,multiclass |  |
| classif.svm | Support Vector Machines (libsvm) | svm | [e1071](http://cran.r-project.org/web/packages/e1071/index.html) | X | X | X |  |  | twoclass,multiclass |  |
| cluster.EM | Expectation-maximization clustering | EM | [RWeka](http://cran.r-project.org/web/packages/RWeka/index.html) |  | X |  |  |  |  |  |
| cluster.SimpleKMeans | k-means clustering | SimpleKMeans | [RWeka](http://cran.r-project.org/web/packages/RWeka/index.html) |  | X |  |  |  |  |  |
| cluster.XMeans | XMeans (k-means with automatic determination of k) | XMeans | [RWeka](http://cran.r-project.org/web/packages/RWeka/index.html) |  | X |  |  |  |  | Note that you might have to install the Weka package: \code{WPM("install-package", "XMeans")} |
| regr.blackboost | Gradient boosting with regression trees | blackbst | [mboost](http://cran.r-project.org/web/packages/mboost/index.html), [party](http://cran.r-project.org/web/packages/party/index.html) |  | X | X |  | X |  |  |
| regr.cforest | Random forest based on conditional inference trees | cforest | [party](http://cran.r-project.org/web/packages/party/index.html) |  | X | X |  | X |  |  |
| regr.crs | Regression Splines | crs | [crs](http://cran.r-project.org/web/packages/crs/index.html) | X | X | X |  | X |  |  |
| regr.earth | Multivariate Adaptive Regression Splines | earth | [earth](http://cran.r-project.org/web/packages/earth/index.html) |  | X | X |  |  |  |  |
| regr.fnn | Fast k-Nearest Neighbor | fnn | [FNN](http://cran.r-project.org/web/packages/FNN/index.html) |  | X |  |  |  |  |  |
| regr.gbm | Gradient boosting machine | gbm | [gbm](http://cran.r-project.org/web/packages/gbm/index.html) |  | X | X | X | X |  | Note that \code{distribution} has been set to \dQuote{gaussian} by default. |
| regr.glmnet | GLM with lasso or elasticnet regularization | glmnet | [glmnet](http://cran.r-project.org/web/packages/glmnet/index.html) | X | X |  |  | X | twoclass,multiclass |  |
| regr.IBk | K-nearest neighbours | IBk | [RWeka](http://cran.r-project.org/web/packages/RWeka/index.html) | X | X | X |  |  | twoclass,multiclass |  |
| regr.kknn | K-Nearest-Neighbor regression | kknn | [kknn](http://cran.r-project.org/web/packages/kknn/index.html) |  | X | X |  |  |  |  |
| regr.km | Kriging | km | [DiceKriging](http://cran.r-project.org/web/packages/DiceKriging/index.html) | X | X |  |  |  |  |  |
| regr.ksvm | Support Vector Machines | ksvm | [kernlab](http://cran.r-project.org/web/packages/kernlab/index.html) |  | X | X |  |  |  | Note that kernel parameters have to be passed directly and not by using the kpar list in ksvm.\cr Note that \code{fit} has been set to \code{FALSE} by default for speed. |
| regr.lm | Simple linear regression | lm | stats | X | X | X |  | X |  |  |
| regr.mars | Multivariate Adaptive Regression Splines | mars | [mda](http://cran.r-project.org/web/packages/mda/index.html) |  | X |  |  |  |  |  |
| regr.mob | Model-based recursive partitioning  yielding a tree with fitted models associated with each terminal node | mob | [party](http://cran.r-project.org/web/packages/party/index.html) |  | X | X |  | X |  |  |
| regr.nnet | Neural Network | nnet | [nnet](http://cran.r-project.org/web/packages/nnet/index.html) |  | X | X |  | X |  | Note that \code{size} has been set to 3 by default. |
| regr.pcr | Principal component regression | pcr | [pls](http://cran.r-project.org/web/packages/pls/index.html) |  | X | X |  |  |  | Note that \code{model} has been set to \code{FALSE} by default for speed. |
| regr.penalized.lasso | Lasso regression | lasso | [penalized](http://cran.r-project.org/web/packages/penalized/index.html) |  | X | X |  |  |  |  |
| regr.penalized.ridge | regr.penalized.ridge | ridge | [penalized](http://cran.r-project.org/web/packages/penalized/index.html) |  | X | X |  |  |  |  |
| regr.plsr | Partial least squares regression | plsr | [pls](http://cran.r-project.org/web/packages/pls/index.html) |  | X | X |  |  |  |  |
| regr.randomForest | Random Forest | RF | [randomForest](http://cran.r-project.org/web/packages/randomForest/index.html) | X | X | X |  |  |  | The argument \code{fix.factors} restores the factor levels seen in the training data before prediction to circumvent randomForest's internal sanity checks. Default is \code{FALSE}. |
| regr.randomForestSRC | Random Forest | RFsrc | [randomForestSRC](http://cran.r-project.org/web/packages/randomForestSRC/index.html) |  | X | X | X |  |  |  |
| regr.rpart | Decision Tree | rpart | [rpart](http://cran.r-project.org/web/packages/rpart/index.html) |  | X | X | X | X |  | Note that \code{xval} has been set to 0 by default for speed. |
| regr.rsm | Response surface regression | rsm | [rsm](http://cran.r-project.org/web/packages/rsm/index.html) |  | X |  |  |  |  | Note that you select the order of the regression by using modelfun = "FO" (first order), "TWI" (two-way interactions, this is with 1st oder terms!) and "SO" (full second order) |
| regr.rvm | Relevance Vector Machine | rvm | [kernlab](http://cran.r-project.org/web/packages/kernlab/index.html) |  | X | X |  |  |  | Note that kernel parameters have to be passed directly and not by using the kpar list in rvm.\cr Note that \code{fit} has been set to \code{FALSE} by default for speed. |
| regr.svm | Support Vector Machines (libsvm) | svm | [e1071](http://cran.r-project.org/web/packages/e1071/index.html) |  | X | X |  |  |  |  |
| surv.cforest | Random forest based on conditional inference trees | cRF | [party](http://cran.r-project.org/web/packages/party/index.html), [survival](http://cran.r-project.org/web/packages/survival/index.html) | X | X | X |  | X | twoclass,multiclass |  |
| surv.CoxBoost | Cox proportional hazards model with componentwise likelhood based boosting | CoxBoost | [CoxBoost](http://cran.r-project.org/web/packages/CoxBoost/index.html) |  | X |  |  | X | rcens |  |
| surv.coxph | Cox proportional hazard model | coxph | [survival](http://cran.r-project.org/web/packages/survival/index.html) | X | X | X | X | X | rcens |  |
| surv.glmboost | surv.glmboost | glmboost | [mboost](http://cran.r-project.org/web/packages/mboost/index.html) |  | X | X |  | X | rcens |  |
| surv.glmnet | GLM with regularization | glmnet | [glmnet](http://cran.r-project.org/web/packages/glmnet/index.html) |  | X |  |  | X | rcens |  |
| surv.penalized | surv.penalized | penalized | [penalized](http://cran.r-project.org/web/packages/penalized/index.html) |  | X | X |  |  | rcens |  |
| surv.randomForestSRC | Random Forests for Survival | RFsrc | [randomForestSRC](http://cran.r-project.org/web/packages/randomForestSRC/index.html) |  | X | X | X |  | rcens |  |








