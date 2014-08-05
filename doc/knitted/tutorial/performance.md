Evaluating Learner Performance
===============================

The quality of the predictions of a model in **mlr** can be assessed w.r.t. a
number of different performance measures.

Typical performance measures are mean misclassification error ([mmce](http://berndbischl.github.io/mlr/man/mmce.html)),
accuracy ([acc](http://berndbischl.github.io/mlr/man/acc.html)) or measures based on ROC analysis for classification and mean
of squared errors ([mse](http://berndbischl.github.io/mlr/man/mse.html)) or mean of absolute errors (\man[mae]) for regression. 
For clustering tasks, measures such as the Dunn index ([dunn](http://berndbischl.github.io/mlr/man/dunn.html)) are provided,
while for survival predictions, the Concordance Index ([cindex](http://berndbischl.github.io/mlr/man/cindex.html)) is
supported, and for cost-sensitive predictions the misclassification penalty
([mcp](http://berndbischl.github.io/mlr/man/mcp.html)) and others. It is also possible to access the time to train the
model, the time to compute the prediction and their sum as performance measures.

To see which performance measures are implemented, have a look at [measures](http://berndbischl.github.io/mlr/man/measures.html). 
If you want to implement an additional measure or include a measure with
non-standard misclassification costs, go to section
[create_measure](create_measure.md). 
In order to calculate the performance measures, the function [performance](http://berndbischl.github.io/mlr/man/performance.html) is used.


Classification example
----------------------

We fit a Linear Discriminant Analysis on a subset of the ``iris`` data set and calculate
the mean misclassification error (mmce) on the test data set.


```splus
library("mlr")

task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.lda")
mod = train(lrn, task = task, subset = seq(1, 150, 2))
pred = predict(mod, task = task, subset = seq(2, 150, 2))

performance(pred, measures = mmce)
```

```
## mmce 
## 0.04
```

  
Let's have a look at some more performance measures. Note that in order to assess 
the time needed for training, the fitted model has to be passed.


```splus
performance(pred, measures = acc)
```

```
##  acc 
## 0.96
```

```splus

performance(pred = pred, measures = timepredict)
```

```
## timepredict 
##       0.003
```

```splus

performance(pred = pred, measures = timetrain, model = mod)
```

```
## timetrain 
##     0.006
```

```splus
performance(pred = pred, measures = timeboth, model = mod)
```

```
## timeboth 
##    0.009
```


Of course we can also calculate multiple performance measures at once by simply using a list of measures which can also include [your own measure](create_measure.md).


```splus
ms = list(mmce = mmce, acc = acc, timetrain = timetrain, timeboth = timeboth)
performance(pred = pred, measures = ms, model = mod)
```

```
##           mmce.mmce             acc.acc timetrain.timetrain 
##               0.040               0.960               0.006 
##   timeboth.timeboth 
##               0.009
```


Binary classification
---------------------

In the two-class case many more measures are available. In the following example,
the accuracy as well as the false positive and false negative rates are computed.


```splus
library("mlbench")
data(Sonar)

task = makeClassifTask(data = Sonar, target = "Class", positive = "M")
lrn = makeLearner("classif.rpart")
mod = train(lrn, task = task)
pred = predict(mod, task = task)

performance(pred, measures = list(acc, fpr, fnr))
```

```
##    acc    fpr    fnr 
## 0.8750 0.1031 0.1441
```


Note that in order to calculate AUC -- the area under the ROC (receiver 
operating characteristic) curve, we have to make sure that posterior
probabilities are predicted, i.e. set the predict type of the [Learner](http://berndbischl.github.io/mlr/man/makeLearner.html) to "prob".


```splus
library("mlbench")
data(Sonar)

task = makeClassifTask(data = Sonar, target = "Class", positive = "M")
lrn = makeLearner("classif.rpart", predict.type = "prob")
mod = train(lrn, task = task)
pred = predict(mod, task = task)

performance(pred, measures = auc)
```

```
## Error: argument "var.name" is missing, with no default
```


For more information on ROC analysis, see section [ROC Analysis](roc_analysis.md).


Regression example
------------------

In regression, everything works in the same way as in the above examples.
We again use the ``BostonHousing`` data set, fit a Gradient Boosting Machine
model on a training set and calculate the mean of squared errors and the mean of
absolute errors on the test data set.


```splus
library(mlbench)
data(BostonHousing)

task = makeRegrTask(data = BostonHousing, target = "medv")

## Training and test set indices
training.set = seq(from = 1, to = nrow(BostonHousing), by = 2)
test.set = seq(from = 2, to = nrow(BostonHousing), by = 2)

## Gradient Boosting Machine on training set
lrn = makeLearner("regr.gbm", n.trees = 1000)
mod = train(lrn, task, subset = training.set)

## Prediction on test set data
pred = predict(mod, newdata = BostonHousing[test.set, ])

## Compare predicted and true labels using measures MSE and MAE
ms = list(mse = mse, mae = mae)
sapply(ms, function(meas) performance(pred, measures = meas))
```

```
## mse.mse mae.mae 
##  42.838   4.548
```


For the other task and learning types, the performance measures work in the same
way.
