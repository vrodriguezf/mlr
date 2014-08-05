Over- and Undersampling
=======================

For unevenly distributed classes in the data, it might be useful to oversample
the minority or undersample the majority class.

``data.frame``
--------------

First we show how it is done for `data.frame`s:


```splus
library(mlr)
classA = data.frame(x = rnorm(20, mean = 1), class = "A")
classB = data.frame(x = rnorm(80, mean = 2), class = "B")
data = rbind(classA, classB)
table(data$class)
```

```
## 
##  A  B 
## 20 80
```

```splus
data.over = oversample(data, "class", rate = 4)
```

```
## Error: unused argument ("class")
```

```splus
table(data.over$class)
```

```
## Error: object 'data.over' not found
```

```splus
data.under = undersample(data, "class", rate = 1/4)
```

```
## Error: unused argument ("class")
```

```splus
table(data.under$class)
```

```
## Error: object 'data.under' not found
```


Tasks
-----

Now let's see what the effect is for a classification task:

```splus
data.even = rbind(data.frame(x = rnorm(50, mean = 1), class = "A"), data.frame(x = rnorm(50, 
    mean = 2), class = "B"))
task = makeClassifTask(data = data, target = "class")
task.over = oversample(task, rate = 4)
task.under = undersample(task, rate = 1/4)
lrn = makeLearner("classif.PART")
mod = train(lrn, task)
mod.over = train(lrn, task.over)
mod.under = train(lrn, task.under)
performance(predict(mod, newdata = data.even), measures = mmce)
```

```
## mmce 
##  0.5
```

```splus
performance(predict(mod.over, newdata = data.even), measures = mmce)
```

```
## mmce 
## 0.33
```

```splus
performance(predict(mod.under, newdata = data.even), measures = mmce)
```

```
## mmce 
##  0.5
```


Learners
--------

Note that it is _highly dependent_ on the learner if over- or undersampling will
be useful. You are also able to wrap the over- and undersampling within the learner.

```splus
lrn.over = makeOversampleWrapper(lrn, osw.rate = 4)
lrn.under = makeUndersampleWrapper(lrn, usw.rate = 1/4)
mod = train(lrn, task)
mod.over = train(lrn.over, task)
mod.under = train(lrn.under, task)
performance(predict(mod, newdata = data.even), measures = mmce)
```

```
## mmce 
##  0.5
```

```splus
performance(predict(mod.over, newdata = data.even), measures = mmce)
```

```
## mmce 
## 0.33
```

```splus
performance(predict(mod.under, newdata = data.even), measures = mmce)
```

```
## mmce 
##  0.5
```

