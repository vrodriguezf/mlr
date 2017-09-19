#' @title Classification of functional data by Generalized Additive Models.
#'
#' @description
#' Learner for classification using Generalized Additive Models and basis 
#' representation. If formula contains non-functional feature, standard glm is 
#' fitted. As smoothing functions for the features smoothing splines (\code{s}) 
#' and tensor product smoothing \code{te} and \code{t2} are supported as in GAM formulae. The classification
#' functionality calls the regression function \code{fregre.gasm}.
#'
#' @export
makeRLearner.classif.fdauscgsam = function() {
  makeRLearnerClassif(
    cl = "classif.fdauscgsam",
    package = "fda.usc",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "binomial", values = list("binomial", 
        "gaussian", "Gamma", "inverse.gaussian", "poisson", "quasi", "quasibinomial", 
        "quasipoisson")),
      makeUntypedLearnerParam(id = "basis.x"),
      makeUntypedLearnerParam(id = "basis.b"),
      makeLogicalLearnerParam(id = "CV", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "basistype", default = "s", values = list("s", "te"), tunable = FALSE),
      makeUntypedLearnerParam(id = "basisargs", tunable = FALSE)
    ),
    properties = c("numerics", "twoclass", "multiclass", "prob", "functionals"),
    name = "Generalized Additive Models classification on FDA",
    short.name = "fdauscgsam",
    note = "model$C[[1]] is set to quote(classif.gsam); added parameters 'basistype' 
      and 'basisarg' to specify GAM formula"
  )
}


#' @export
trainLearner.classif.fdauscgsam = function(.learner, .task, .subset, .weights = NULL, ...) {

  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, functionals.as = "matrix")
  fd = getFunctionalFeatures(d$data)
  # transform the data into fda.usc:fdata class type.
  data.fdclass = fda.usc::fdata(mdata = as.matrix(fd))
  # transform the data into fda.usc:fdata class type and save in a list
  dat = list(df = data.frame(d$target), x = data.fdclass)

  currargs = getHyperPars(.learner)
  basis = .learner$par.set$pars$basistype$default
  if ("basistype" %in% names(currargs))
    basis = currargs$basistype
  
  if ("basisargs" %in% names(currargs)) {
    addargs = sprintf("%s = %s", names(currargs$basisargs), currargs$basisargs)
    form = as.formula(sprintf("d.target~%s(x, %s)", basis, paste0(addargs, collapse = ", ")))
  } else {
    form = as.formula(sprintf("d.target~%s(x)", basis))
  }
  model = fda.usc::classif.gsam(form, data = dat, ...)
  # Fix bug in package
  model$C[[1]] = quote(classif.gsam)
  return(model)
}

#' @export
predictLearner.classif.fdauscgsam = function(.learner, .model, .newdata, ...) {
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
