Training a Learner
==================

Training a learner means fitting a model to a given data set.
In the **mlr** package this can be done by calling the function [train](http://berndbischl.github.io/mlr/man/train.html)
with a learner and a suitable [Task](http://berndbischl.github.io/mlr/man/Task.html).


Quick start
-----------

### Classification example

We start with a basic example and train a Linear Discriminant Analysis on the ``iris`` data set.


```splus
library("mlr")
task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.lda")
mod = train(lrn, task)
mod
```

```
## Model for id = classif.lda class = classif.lda
## Trained on obs: 150
## Used features: 4
## Hyperparameters:
```


### Regression example

We fit a simple linear regression model to the ``BostonHousing`` data set.


```splus
library("mlr")
library("mlbench")
data(BostonHousing)

task = makeRegrTask(data = BostonHousing, target = "medv")
lrn = makeLearner("regr.lm")
mod = train(lrn, task)
mod
```

```
## Model for id = regr.lm class = regr.lm
## Trained on obs: 506
## Used features: 13
## Hyperparameters:
```


### Clustering example

We cluster the ``iris`` data set without the target variable.


```splus
library("mlr")
task = makeClusterTask(data = iris[, -5])
lrn = makeLearner("cluster.XMeans")
mod = train(lrn, task)
mod
```

```
## Model for id = cluster.XMeans class = cluster.XMeans
## Trained on obs: 150
## Used features: 4
## Hyperparameters:
```


Further information
-------------------

As already mentioned, we can train a learner simply by
calling the function [train](http://berndbischl.github.io/mlr/man/train.html), passing it a \man[Learner] and a \man[Task]. 
In the examples above, the machine learning method was specified 
through special [Learner](http://berndbischl.github.io/mlr/man/makeLearner.html) objects. Creating a separate object
allows to set the hyperparameters of the learner before training for example.

Optionally, only a subset of the data, specified by an index set, can be used to 
train the learner. This set is passed using the ``subset`` argument of [train](http://berndbischl.github.io/mlr/man/train.html).

The return value is always an object of class [WrappedModel](http://berndbischl.github.io/mlr/man/WrappedModel.html), which wraps the
particular model of the used R classification or regression method. It
can subsequently be used to perform a \man2[prediction][predict.WrappedModel] for new observations.
