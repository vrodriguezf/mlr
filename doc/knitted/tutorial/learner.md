Learners
========

These classes provide a unified interface to all machine learning methods in R
-- classification, regression, survival analysis, and clustering.
Many are already integrated,
others are not, but the package is specifically designed to make
extensions simple. If your favorite methods is missing, either
send the **mlr** maintainers an email or see the part of the tutorial
on [how to extend the package yourself](create_learner.md).

To see which learners are already implemented, have a look at the [learners](http://berndbischl.github.io/mlr/man/learners.html) documentation page.

A learner in **mlr** is constructed by calling [makeLearner](http://berndbischl.github.io/mlr/man/makeLearner.html).
In the constructor you can do the following things:

* Set hyperparameters.
* Set the ID to name the object (some methods will later use this ID to name results).
* Control the output for later prediction, e.g. for classification
  whether you want a factor for predicted classes or probabilities.


```splus
# classifier, set it up for prediction probabilities
lrn = makeLearner("classif.rpart", predict.type = "prob")
# regression
lrn = makeLearner("regr.rpart")
# gradient boosting machine with hyperparameters and custon name
lrn = makeLearner("regr.gbm", id = "mygbm", n.trees = 500, interaction.depth = 3)
```


The first argument specifies which algorithm to use.
The naming convention is ``classif.<R_method_name>`` for
classification methods, ``regr.<R_method_name>`` for regression methods,
``surv.<R_method_name>`` for survival analysis, and ``cluster.<R_method_name>``
for clustering methods.
The tables in [IntegratedLearners](../integrated_learners.md) provide more
information on the properties for all integrated learning methods.

Further information
-------------------

The generated learner is an object of class [Learner](http://berndbischl.github.io/mlr/man/makeLearner.html).
It contains the properties the method, e.g. which types of features it can handle,
what can be output during prediction, whether multi-class problems,
observations weights or missing values are supported and so on.


```splus
# classifier, set it up for prediction probabilities
lrn = makeLearner("classif.rpart", predict.type = "prob", minsplit = 20)
lrn
```

```
## Learner classif.rpart from package rpart
## Type: classif
## Name: Decision Tree; Short name: rpart
## Class: classif.rpart
## Properties: twoclass,multiclass,missings,numerics,factors,prob,weights
## Predict-Type: prob
## Hyperparameters: xval=0,minsplit=20
```

```splus
# display stored information for hyperparameters
lrn$par.set
```

```
##                    Type len  Def   Constr Req Trafo
## minsplit        integer   -   20 1 to Inf   -     -
## minbucket       integer   -    - 1 to Inf   -     -
## cp              numeric   - 0.01   0 to 1   -     -
## maxcompete      integer   -    4 0 to Inf   -     -
## maxsurrogate    integer   -    5 0 to Inf   -     -
## usesurrogate   discrete   -    2    0,1,2   -     -
## surrogatestyle discrete   -    0      0,1   -     -
## maxdepth        integer   -   30  1 to 30   -     -
## xval            integer   -   10 0 to Inf   -     -
## parms           untyped   -    -        -   -     -
```


