#' @title Classification of functional data by Generalized Kernel Additive Models.
#'
#' @description
#' Learner for classification using Generalized Kernel Additive Models and basis 
#' representation. The classification procedure uses backfitting algorithm.
#' Can only handle functional features.
#' 
#' As smoothing functions for the features smoothing splines (\code{s}) 
#' and tensor product smoothing \code{te} are supported as in GAM formulae. 
#' The classification functionality calls the regression function \code{fregre.gkam}.
#'
#' @export
makeRLearner.classif.fdauscgkam = function() {
  makeRLearnerClassif(
    cl = "classif.fdauscgkam",
    package = "fda.usc",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "binomial", values = list("binomial", 
        "gaussian", "Gamma", "inverse.gaussian", "poisson", "quasi", "quasibinomial", 
        "quasipoisson")),
      makeNumericVectorLearnerParam(id = "weights", default = 1),
      makeUntypedLearnerParam(id = "par.metric"),
      makeUntypedLearnerParam(id = "par.np"),
      makeUntypedLearnerParam(id = "offset"),
      makeIntegerLearnerParam(id = "maxit", default = 100, low = 1, tunable = FALSE),
      makeNumericLearnerParam(id = "epsilon", default = 0.001, tunable = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "inverse", default = "solve", values = list("solve", "svd"), tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "prob", "functionals"),
    name = "Generalized Kernel Additive Models classification on FDA",
    short.name = "fdauscgkam",
    note = "model$C[[1]] is set to quote(classif.gsam); parameters 'maxit', 'epsilon',
      'trace', 'inverse' are detached from orginal parameter 'control'"
  )
}

#' @export
trainLearner.classif.fdauscgkam = function(.learner, .task, .subset, .weights = NULL, ...) {
  
  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, functionals.as = "matrix")
  fd = getFunctionalFeatures(d$data)
  # transform the data into fda.usc:fdata class type.
  data.fdclass = fda.usc::fdata(mdata = as.matrix(fd))
  # transform the data into fda.usc:fdata class type and save in a list
  dat = list(df = data.frame(d$target), x = data.fdclass)
  
  #FIXME: How to build control argument as control = list(maxit, epsilon, trace, inverse) ?
  model = fda.usc::classif.gkam(d.target ~ x, data = dat, weights = .weights, ...)
    # Fix bug in package
  model$C[[1]] = quote(classif.gkam)
  return(model)
}

#' @export
predictLearner.classif.fdauscgkam = function(.learner, .model, .newdata, ...) {
  # transform the data into fda.usc:fdata class type.
  fd = getFunctionalFeatures(.newdata)
  nd = list(x = fda.usc::fdata(mdata = fd))
  # predict according to predict.type
  type = ifelse(.learner$predict.type == "prob", "probs", "class")
  if (type == "probs") {
    fda.usc::predict.classif(object = .model$learner.model, new.fdataobj = nd, type = type)$prob.group
  } else {
    fda.usc::predict.classif(object = .model$learner.model, new.fdataobj = nd, type = type)
  }
}
