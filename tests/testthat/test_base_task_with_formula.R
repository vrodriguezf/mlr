lrn = makeLearner("classif.logreg", predict.type = "prob")

task = makeClassifTask(data = getTaskData(pid.task),
  formula = diabetes ~ pregnant + glucose,
  target = "diabetes")

m = train(lrn, task)
