makeRLearner.regr.bagEarth = function() {
  makeRLearnerRegr(
    cl = "regr.bagEarth",
    package = "caret",
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

trainLearner.regr.bagEarth = function(.learner, .task,  ...) {
  f = getTaskFormula(.task)
  d = getTaskData(.task)
  if (.task$task.desc$has.weights) {
    f = getTaskFormula(.task)
    bagEarth(f, data=getTaskData(.task), ...)
  }else  
    bagEarth(f, data=d, ...)
}

predictLearner.regr.bagEarth = function(.learner, .model, .newdata, ...) {
  predict.bagEarth(.model$learner.model, newdata=.newdata)
}
