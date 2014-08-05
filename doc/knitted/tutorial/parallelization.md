Parallelization
===============

*R* by default doesn't make use of multiple cores.
With the integration of
[parallelMap](https://github.com/berndbischl/parallelMap) into **mlr**, it
becomes easy to activate the parallel computing capabilities already supported
by **mlr**.


```splus
library("mlr")
library("parallelMap")
parallelStartSocket(2)
task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.lda")
rdesc = makeResampleDesc("CV", iters = 4)
r = resample(lrn, task, rdesc)
```

```
## [Resample] Result: mmce.test.mean=0.0268
```

```splus
parallelStop()
```


In Linux or Mac OS X, you may want to use `parallelStartMulticore()` instead.

For further ways of parallelizing your computations, consult the [parallelMap
tutorial](https://github.com/berndbischl/parallelMap#parallelmap) and help.
