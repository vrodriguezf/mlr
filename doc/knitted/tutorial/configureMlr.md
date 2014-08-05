Configure mlr
=============

If you really know what you are doing you may think **mlr** is limiting you in
certain ways.
Actually **mlr** is just designed to make errors due to typos or
invalid parameter values as unlikely as possible. But sometimes you want to
break those barriers and get full access. For all parameters, simply refer
to the documentation of [configureMlr](http://berndbischl.github.io/mlr/man/configureMlr.html).

You are bothered by all the output on the console like in this example?

```splus
library("mlr")

## Define a learning task and an appropriate learner
task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.ksvm")

## Perform a 3-fold cross-validation
rdesc = makeResampleDesc("CV", iters = 3)
r = resample(lrn, task, rdesc)
```

```
## [Resample] cross-validation iter: 1
```

```
## Using automatic sigma estimation (sigest) for RBF or laplace kernel
```

```
## [Resample] cross-validation iter: 2
```

```
## Using automatic sigma estimation (sigest) for RBF or laplace kernel
```

```
## [Resample] cross-validation iter: 3
```

```
## Using automatic sigma estimation (sigest) for RBF or laplace kernel
```

```
## [Resample] Result: mmce.test.mean=0.04
```


Just try the following:


```splus
configureMlr(show.learner.output = FALSE)
r = resample(lrn, task, rdesc)
```

```
## [Resample] cross-validation iter: 1
## [Resample] cross-validation iter: 2
## [Resample] cross-validation iter: 3
## [Resample] Result: mmce.test.mean=0.0533
```


In an additional scenario, you want to access a new parameter of a learner which
is already available in **mlr**, but the parameter is not implemented in **mlr** yet.
If this is the case you might want to [contact
us](https://github.com/berndbischl/mlr#get-in-touch) or [open an issue](https://github.com/berndbischl/mlr/issues/new) as well!
But until then you can turn off **mlr**'s paramater checking like this:


```splus
library("mlr")
lrn = makeLearner("classif.ksvm", newPar = 3)
```

```
## Error: classif.ksvm: Setting parameter newPar without available description object!
## You can switch off this check by using configureMlr!
```

```splus
lrn = makeLearner("classif.ksvm", epsilon = -3)
```

```
## Error: classif.ksvm: Setting parameter epsilon without available description object!
## You can switch off this check by using configureMlr!
```

```splus
configureMlr(on.par.without.desc = "quiet")
lrn = makeLearner("classif.ksvm", newPar = 3)
lrn = makeLearner("classif.ksvm", epsilon = -3)
```


