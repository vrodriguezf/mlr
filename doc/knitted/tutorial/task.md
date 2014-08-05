Learning Tasks
==============

Learning tasks are the basic elements of the package to encapsulate the
data set and all relevant information regarding the purpose of the
task, e.g, the target variable.

The tasks are organised in a hierarchy, with the generic
[Task](http://berndbischl.github.io/mlr/man/Task.html) at the top.
The concrete tasks that can be instantiated are [ClassifTask](http://berndbischl.github.io/mlr/man/Task.html) for classification,
[RegrTask](http://berndbischl.github.io/mlr/man/Task.html) for regression, \man2[SurvTask][Task] for survival
analysis, [CostSensTask](http://berndbischl.github.io/mlr/man/Task.html) for
cost-sensitive learning, and [ClusterTask](http://berndbischl.github.io/mlr/man/Task.html) for clustering.

There are dedicated functions to create these tasks given data and other
relevant information. These are [makeClassifTask](http://berndbischl.github.io/mlr/man/Task.html),
[makeRegrTask](http://berndbischl.github.io/mlr/man/Task.html),
[makeSurvTask](http://berndbischl.github.io/mlr/man/Task.html), \man2[makeCostSensTask()][Task], and
\man2[akeClusterTask()][Task].

In the following example, we define a classification task for the data
set ``BreastCancer`` (from the package mlbench) and exclude the ID
variable from all further model fitting and evaluation.


```splus
library("mlr")
library("mlbench")
data(BreastCancer)

df = BreastCancer
df$Id = NULL
classif.task = makeClassifTask(id = "BreastCancer", data = df, target = "Class")
classif.task
```

```
## Supervised task: BreastCancer
## Type: classif
## Target: Class
## Observations: 699
## Features:
## numerics  factors 
##        0        4 
## Missings: TRUE
## Has weights: FALSE
## Has blocking: FALSE
## Classes: 2
##    benign malignant 
##       458       241 
## Positive class: benign
```


As we can see, the task records basic information about the data set,
e.g. the types of the features, the number of observations, whether
missing values are present, the number of observations per class and so on.


In many of the regression examples in this tutorial, we will use the ``BostonHousing`` data set:


```splus
data(BostonHousing)
regr.task = makeRegrTask(id = "BostonHousing", data = BostonHousing, target = "medv")
regr.task
```

```
## Supervised task: BostonHousing
## Type: regr
## Target: medv
## Observations: 506
## Features:
## numerics  factors 
##       12        1 
## Missings: FALSE
## Has weights: FALSE
## Has blocking: FALSE
```


Further information
-------------------

Let's have another look at the classification example.

As this is a binary problem, we see that a positive class is selected
by default. This will generally be auto-selected, but you might
want to do this manually for your application. It mainly concerns ROC analysis, where in order
to talk about something like a true positive rate, we need to know which of the two classes is the positive
one.


```splus
classif.task = makeClassifTask(id = "BreastCancer", data = df, target = "Class", 
    positive = "malignant")
```


There are also some convenient methods to access properties and parts of the task.
The most important ones are listed in [Task](http://berndbischl.github.io/mlr/man/Task.html).

Here are some examples.



```splus
# get the names of the input variables:
getTaskFeatureNames(classif.task)
```

```
## [1] "Cl.thickness"    "Cell.size"       "Cell.shape"      "Marg.adhesion"  
## [5] "Epith.c.size"    "Bare.nuclei"     "Bl.cromatin"     "Normal.nucleoli"
## [9] "Mitoses"
```

```splus

# get values of the target variable for all observations:
head(getTaskTargets(classif.task))
```

```
## [1] benign    benign    benign    benign    benign    malignant
## Levels: benign malignant
```

```splus
head(getTaskTargets(regr.task))
```

```
## [1] 24.0 21.6 34.7 33.4 36.2 28.7
```

```splus

# accessing the data set:
str(getTaskData(classif.task))
```

```
## 'data.frame':	699 obs. of  10 variables:
##  $ Cl.thickness   : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 5 5 3 6 4 8 1 2 2 4 ...
##  $ Cell.size      : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 1 1 2 ...
##  $ Cell.shape     : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 2 1 1 ...
##  $ Marg.adhesion  : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 5 1 1 3 8 1 1 1 1 ...
##  $ Epith.c.size   : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 2 7 2 3 2 7 2 2 2 2 ...
##  $ Bare.nuclei    : Factor w/ 10 levels "1","2","3","4",..: 1 10 2 4 1 10 10 1 1 1 ...
##  $ Bl.cromatin    : Factor w/ 10 levels "1","2","3","4",..: 3 3 3 3 3 9 3 3 1 2 ...
##  $ Normal.nucleoli: Factor w/ 10 levels "1","2","3","4",..: 1 2 1 7 1 7 1 1 1 1 ...
##  $ Mitoses        : Factor w/ 9 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 5 1 ...
##  $ Class          : Factor w/ 2 levels "benign","malignant": 1 1 1 1 1 2 1 1 1 1 ...
```


Note the many options [getTaskData](http://berndbischl.github.io/mlr/man/getTaskData.html) provides to convert the data set into a convenient format.
This is especially handy when you integrate a learner from another package into
**mlr**.

If you are more technically minded, you could also access
the information directly stored in the slot called `task$task.desc`,
which stands for "description" and is of class [TaskDesc](http://berndbischl.github.io/mlr/man/TaskDesc.html).


```splus
str(classif.task$task.desc)
```

```
## List of 11
##  $ id          : chr "BreastCancer"
##  $ type        : chr "classif"
##  $ target      : chr "Class"
##  $ size        : int 699
##  $ n.feat      : Named int [1:2] 0 4
##   ..- attr(*, "names")= chr [1:2] "numerics" "factors"
##  $ has.missings: logi TRUE
##  $ has.weights : logi FALSE
##  $ has.blocking: logi FALSE
##  $ class.levels: chr [1:2] "benign" "malignant"
##  $ positive    : chr "malignant"
##  $ negative    : chr "benign"
##  - attr(*, "class")= chr [1:2] "TaskDescClassif" "TaskDesc"
```


The [Task](http://berndbischl.github.io/mlr/man/Task.html) help page also lists several other arguments
to describe further details of the problem.

For example, we could include a blocking factor into the task.
This would tell the task that some observations "belong together", and should
not be separated when splitting into training and test sets during a resampling iteration.
Another possibility is to weight observations according to their importance.
