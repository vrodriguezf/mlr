makeRLearner.regr.icr = function() {
  makeRLearnerRegr(
    cl = "regr.icr",
    package = "stats",
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

trainLearner.regr.icr = function(.learner, .task,  ...) {
  f = getTaskFormula(.task)
  icr(f, data=getTaskData(.task), ...)
}

predictLearner.regr.icr = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata=.newdata)
}