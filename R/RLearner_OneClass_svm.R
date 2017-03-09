#' @export
makeRLearner.oneclass.svm = function() {
  makeRLearnerOneClass(
    cl = "oneclass.svm",
    package = "e1071",
    par.set = makeParamSet(
      ##% add type "one-classification", already implemented in svm package
      ##% values needed, can't delete otherwise error:  Error in checkValuesForDiscreteParam(id, values) : argument "values" is missing, with no default 
      makeDiscreteLearnerParam(id = "type", default = "one-classification", values = c("one-classification")),
      makeNumericLearnerParam(id = "cost",  default = 1, lower = 0, requires = quote(type=="C-classification")),
      makeNumericLearnerParam(id = "nu", default = 0.5, requires = quote(type=="nu-classification")),
      makeNumericVectorLearnerParam("class.weights", len = NA_integer_, lower = 0),
      makeDiscreteLearnerParam(id = "kernel", default = "radial", values = c("linear", "polynomial", "radial", "sigmoid")),
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L, requires = quote(kernel=="polynomial")),
      makeNumericLearnerParam(id = "coef0", default = 0, requires = quote(kernel=="polynomial" || kernel=="sigmoid")),
      makeNumericLearnerParam(id = "gamma", lower = 0, requires = quote(kernel!="linear")),
      makeNumericLearnerParam(id = "cachesize", default = 40L),
      makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0),
      makeLogicalLearnerParam(id = "shrinking", default = TRUE),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L, tunable = FALSE),
      makeLogicalLearnerParam(id = "fitted", default = TRUE, tunable = FALSE),
      makeLogicalVectorLearnerParam(id = "scale", default = c(TRUE), tunable = TRUE)
    ),
    
    ##% change default of type to one-classification
    par.vals = list(type = "one-classification"),
    ##% add properties "oneclass", sind andere noch noetig?
    properties =  c("oneclass", "numerics", "factors"),
    
    ##%no class weights for anomaly detection, as there is only one class
    #%class.weights.param = "class.weights",
    
    name = "one-class Support Vector Machines (libsvm)",
    short.name = "one-class svm"
  )
}

#' @export
trainLearner.oneclass.svm = function(.learner, .task, .subset, .weights = NULL,  ...) {
  ##%kein prob fuer oneclass
    e1071::svm(getTaskData(.task, .subset), y = NULL, probability = .learner$predict.type == "prob", ...)
}

#' @export
predictLearner.oneclass.svm = function(.learner, .model, .newdata, ...) {
  ##% ifelse loeschen da one class nicht prob predicten kann
 #% if (.learner$predict.type == "response") { 
    predict(.model$learner.model, newdata = .newdata, ...)
  #% } else {
#%    attr(predict(.model$learner.model, newdata = .newdata, probability = TRUE, ...), "probabilities")
  #%}
}


