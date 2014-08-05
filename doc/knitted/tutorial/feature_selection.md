Feature Selection
==================

Often, data sets include a large number of features and it is desirable to reduce them. 
The technique of selecting a subset of relevant features is called feature selection. 
Feature selection can make the model better interpretable, the learning process
faster, and the fitted model more general by removing irrelevant features.
Different approaches exist, in order to figure out, which the relevant features
are. **mlr** supports [filters](#Filter) and [wrappers](#Wrapper).

Filters
-------

Filters are the simplest approach to find features that do not contain a lot of
additional information given the other features and can be left out.
Different methods are built into **mlr**'s function [getFeatureFilterValues](http://berndbischl.github.io/mlr/man/getFeatureFilterValues.html)
and all access filter algorithms from the package `FSelector`.
The function is given a [Task](http://berndbischl.github.io/mlr/man/Task.html) and simply returns a vector characterising
the importance of the different features.


```splus
library("mlr")
task = makeClassifTask(data = iris, target = "Species")
importance = getFilterValues(task, method = "information.gain")
```

```
## Loading required package: FSelector
```

```splus
importance
```

```
## FilterValues:
## Task: iris
## Method: information.gain
##                      name    val    type
## Sepal.Length Sepal.Length 0.6523 numeric
## Sepal.Width   Sepal.Width 0.3856 numeric
## Petal.Length Petal.Length 1.3565 numeric
## Petal.Width   Petal.Width 1.3784 numeric
```


According to this filter, `Petal.Width` and `Petal.Length` contain the most information.
With **mlr**'s function [filterFeatures](http://berndbischl.github.io/mlr/man/filterFeatures.html), you can now filter the features in
the task by leaving out all but a given number of features with the highest
feature importance.


```splus
filtered.task = filterFeatures(task, method = "information.gain", select = "abs", 
    val = 2)
```


Other filter options in [filterFeatures](http://berndbischl.github.io/mlr/man/filterFeatures.html) are to require a percentage of
features to be filtered instead or to set a threshold for the numerical importance
values.

In a proper experimental set up you might want to automate the selection of the
features so that it can be part of the validation method of your choice.
We will use the standard 10-fold cross validation.


```splus
learner = makeLearner("classif.fnn")
learnerFiltered = makeFilterWrapper(learner = learner, fw.method = "information.gain", 
    fw.select = "perc", fw.val = 0.7)
rdesc = makeResampleDesc("CV", iters = 10)
rsres = resample(learner = learnerFiltered, task = task, resampling = rdesc, 
    show.info = FALSE, models = TRUE)
rsres$aggr
```

```
## mmce.test.mean 
##        0.04667
```


You may want to know which features have been used. Luckily, we have called
`resample` with the argument `models=TRUE`, which means that `rsres$models`
contains a `list` of each model used for a fold.
In this case the [Learner](http://berndbischl.github.io/mlr/man/Learner.html) is also of the class \man[FilterWrapper] and we can call
[getFilteredFeatures](http://berndbischl.github.io/mlr/man/getFilteredFeatures.html) on each model.


```splus
sfeats = sapply(rsres$models, getFilteredFeatures)
table(sfeats)
```

```
## sfeats
## Petal.Length  Petal.Width Sepal.Length 
##           10           10           10
```


The selection of features seems to be very stable.
The `Sepal.Width` did not make it into a single fold.

### Tuning the threshold

```splus
library("mlbench")
data(Sonar)
task = makeClassifTask(data = Sonar, target = "Class", positive = "M")
lrn = makeLearner("classif.rpart")
lrnFiltered = makeFilterWrapper(learner = lrn, fw.method = "chi.squared", fw.select = "threshold", 
    fw.val = 0)
ps = makeParamSet(makeDiscreteParam("fw.val", values = seq(from = 0.2, 0.4, 
    by = 0.05)))
tuneRes = tuneParams(lrnFiltered, task = task, resampling = makeResampleDesc("CV", 
    iters = 5), par.set = ps, control = makeTuneControlGrid())
```

```
## [Tune] Started tuning learner classif.rpart.filtered for parameter set:
##            Type len Def                Constr Req Trafo
## fw.val discrete   -   - 0.2,0.25,0.3,0.35,0.4   -     -
## With control class: TuneControlGrid
## Imputation value: 1
## [Tune] 1: fw.val=0.2 : mmce.test.mean=0.318
## [Tune] 2: fw.val=0.25 : mmce.test.mean=0.318
## [Tune] 3: fw.val=0.3 : mmce.test.mean=0.308
## [Tune] 4: fw.val=0.35 : mmce.test.mean=0.318
## [Tune] 5: fw.val=0.4 : mmce.test.mean=0.336
## [Tune] Result: fw.val=0.3 : mmce.test.mean=0.308
```


Wrapper
-------

Unlike the filters, wrappers use the performance a learner can
achieve on a given subset of the features in the data to do the filtering.

#### Classification example

Let's train a decision tree on the ``iris`` data and use a sequential forward
search to find the best set of features w.r.t. the mean misclassification error
([mmce](http://berndbischl.github.io/mlr/man/mmce.html)).


```splus
library("mlr")
task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.rpart")
rdesc = makeResampleDesc("Holdout")

ctrlSeq = makeFeatSelControlSequential(method = "sfs")
sfSeq = selectFeatures(learner = lrn, task = task, resampling = rdesc, control = ctrlSeq)
```

```
## [FeatSel] Started selecting features for learner 'classif.rpart'
## With control class: FeatSelControlSequential
## Imputation value: 1
## [FeatSel] 1: 0 bits: mmce.test.mean= 0.7
## [FeatSel] 2: 1 bits: mmce.test.mean=0.22
## [FeatSel] 2: 1 bits: mmce.test.mean=0.46
## [FeatSel] 2: 1 bits: mmce.test.mean=0.06
## [FeatSel] 2: 1 bits: mmce.test.mean=0.06
## [FeatSel] 3: 2 bits: mmce.test.mean=0.06
## [FeatSel] 3: 2 bits: mmce.test.mean=0.06
## [FeatSel] 3: 2 bits: mmce.test.mean=0.06
## [FeatSel] Result: 1 bits : mmce.test.mean=0.06
```

```splus
sfSeq
```

```
## FeatSel result:
## Features (1): Petal.Length
## mmce.test.mean=0.06
```

```splus
analyzeFeatSelResult(sfSeq, reduce = FALSE)
```

```
## Features         : 1
## Performance      : mmce.test.mean=0.06
## Petal.Length
## 
## Path to optimum:
## --------------------------------------------------------------------------------
## - Features:    0  Init   :                       Perf = 0.7  Diff: NA  *
## --------------------------------------------------------------------------------
## - Features:    1  Add    : Sepal.Length          Perf = 0.22  Diff: 0.48   
## - Features:    1  Add    : Sepal.Width           Perf = 0.46  Diff: 0.24   
## - Features:    1  Add    : Petal.Length          Perf = 0.06  Diff: 0.64   
## - Features:    1  Add    : Petal.Width           Perf = 0.06  Diff: 0.64  *
## --------------------------------------------------------------------------------
## - Features:    2  Add    : Sepal.Length          Perf = 0.06  Diff: 0   
## - Features:    2  Add    : Sepal.Width           Perf = 0.06  Diff: 0   
## - Features:    2  Add    : Petal.Length          Perf = 0.06  Diff: 0   
## 
## Stopped, because no improving feature was found.
```



#### Regression example

We fit a simple linear regression model to the ``BostonHousing`` data set and
use a genetic algorithm to find a feature set that minimises the mean squared
error ([mse](http://berndbischl.github.io/mlr/man/mse.html)).


```splus
library("mlbench")
data(BostonHousing)

task = makeRegrTask(data = BostonHousing, target = "medv")
lrn = makeLearner("regr.lm")
rdesc = makeResampleDesc("Holdout")

ctrlGA = makeFeatSelControlGA(maxit = 10)
sfGA = selectFeatures(learner = lrn, task = task, resampling = rdesc, control = ctrlGA, 
    show.info = FALSE)
sfGA
```

```
## FeatSel result:
## Features (10): crim, zn, chas, nox, rm, age, dis, ptratio, b, lstat
## mse.test.mean=24.9
```


