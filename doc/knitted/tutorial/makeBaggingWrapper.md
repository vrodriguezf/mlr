


Generic bagging
========================================================

One reason why random forests perform so well is that they are using bagging as
a technique to gain more stability. But why do you want to limit yourself to the
classifiers already implemented in well known random forests when it is
really easy to build your with **mlr**?

Just bag one learner already supported in **mlr** with [makeBaggingWrapper](http://berndbischl.github.io/mlr/man/makeBaggingWrapper.html).

As in a random forest, we need a Learner which is trained on a subset of the
data during each iteration of the bagging process.
The subsets are chosen according to the parameters given to [makeBaggingWrapper](http://berndbischl.github.io/mlr/man/makeBaggingWrapper.html):
* `bag.iters` On how many subsets (samples) do we want to train our learner?
* `bw.replace` Sample with replacement (also known as *bootstrapping*)?
* `bw.size` Percentage size of the samples. If `bw.replace=TRUE`, `bag.size=1` is the default. This does not mean that one sample will contain all the observations as observations will occur multiple times in each sample.
* `bw.feats` Percentage size of randomly selected features for each iteration. 

Of course we also need a `learner` which we have to pass to
[makeBaggingWrapper](http://berndbischl.github.io/mlr/man/makeBaggingWrapper.html).


```splus
library(mlr)
library(mlbench)  #for the datasets
data(Sonar)
tsk = makeClassifTask(data = Sonar, target = "Class")
lrn = makeLearner("classif.PART")
rsmpl = makeResampleDesc("CV", iters = 10)
bagLrn = makeBaggingWrapper(lrn, bag.iters = 50, bw.replace = TRUE, bw.size = 0.8, 
    bw.feats = 3/4, predict.type = "prob")
```

```
## Error: unused arguments (bag.iters = 50, predict.type = "prob")
```

Now we can compare the performance with and without bagging.
First let's try it without bagging:

```splus
result = resample(learner = lrn, task = tsk, resampling = rsmpl, show.info = FALSE)
result$aggr
```

```
## mmce.test.mean 
##          0.251
```

And now with bagging:

```splus
result = resultBagging = resample(learner = bagLrn, task = tsk, resampling = rsmpl, 
    show.info = FALSE)
```

```
## Error: object 'bagLrn' not found
```

```splus
result$aggr
```

```
## mmce.test.mean 
##          0.251
```

Training more learners takes more time, but can outperform pure learners
on noisy data with many features.



Additional Measures
-------------------
With bagging, you can get an estimated standard deviation regardless of the
wrapped learner. Below, we give a small example for regression.


```splus
data(BostonHousing)
train.inds = seq(1, 506, 3)
test.inds = setdiff(1:nrow(BostonHousing), train.inds)
task = makeRegrTask("regrtask", data = BostonHousing, target = "medv")
lrn = makeLearner("regr.randomForest")
bagLrn = makeBaggingWrapper(lrn, predict.type = "se")
```

```
## Error: unused argument (predict.type = "se")
```

```splus
mod = train(learner = bagLrn, task = task, subset = train.inds)
```

```
## Error: object 'bagLrn' not found
```

```splus
pred = predict(mod, newdata = BostonHousing, subset = test.inds)
```

```
## Error: error in evaluating the argument 'object' in selecting a method for function 'predict': Error: object 'mod' not found
```

```splus
head(pred$data)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for function 'head': Error: object 'pred' not found
```

In the column labelled `se`, the standard deviation for each prediction (`response`) is given.

Let's visualise this a bit using [ggplot2](http://ggplot2.org/).
Here we plot the percentage of lower status of the population (`lstat`) against the prediction.

```splus
library("ggplot2")
library("reshape2")
data = cbind(pred$data, BostonHousing[test.inds, ])
```

```
## Error: object 'pred' not found
```

```splus
g = ggplot(data, aes(x = lstat, y = response, ymin = response - se, ymax = response + 
    se, col = age))
```

```
## Error: ggplot2 doesn't know how to deal with data of class function
```

```splus
g + geom_point() + geom_linerange(alpha = 0.5)
```

```
## Error: object 'g' not found
```

