# load_all()

source("R/TaskTransform.R")

# lets get some toy data and split it up
task = convertMLBenchObjToTask("Ionosphere")
n = getTaskSize(task)
task.train = subsetTask(task, seq(1, n, 2))
task.test = subsetTask(task, seq(2, n, 2))

# fixme: add some random task splitting!

# lets create our first (manual) TT: a feature removal op
# we define the 2 functions, and the params in the paramset
# this demos how a user could write a custom trafo, or how we would
# define the TTs in mlr. the "feature removal" would of course be pre-defined
ps = makeParamSet(
  makeNumericParam("perc", lower = 0, upper = 1)
)
f1 = function(task, ...) {
  task2 = removeConstantFeatures(task, ...)
  retrafo = getTaskFeatureNames(task2)
  list(task = task, retrafo = retrafo)
}
f2 = function(task, retrafo) {
  subsetTask(task, features = retrafo)
}
tt1 = makeTaskTransform(f1, f2, ps)

# lets apply the TT, with a bogus control value
# some feats will get removed
z = transformTask(tt1, task.train, par.vals = list(perc = 0.8))
# print(z$task.train.2)
# FIXME: it sucks that i have to return 2 objects here


# now we do the same stuff on the test split
task.test.2 = retransform(tt1, task.test, z$retrafo)
print(task.test.2)

# lets create our 2nd TT
ps = makeParamSet()
f1 = function(task, ...) {
  task = capLargeValues(task)
  list(task = task, retrafo = NULL)
}
f2 = function(task, retrafo) {
  task = capLargeValues(task)
}
tt2 = makeTaskTransform(f1, f2, ps)

tt3 = joinTaskTransforms(tt1, tt2)

z = transformTask(tt3, task.train, par.vals = list(perc = 0.8))
task.test.2 = retransform(tt3, task.test, z$retrafo)


# removeconstant %>% caplargevalues %>% filterfeatures %>% pca





