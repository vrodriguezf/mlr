#' @title Classification of functional data by maximum depth.
#'
#' @description
#' Learner for classification using maximum depth methods.
#' FIXME: the fda.usc.depth classifier call is somehow strange. in the package, the training and prediction is called with the same function call only adding the newfdataobj argument in the function call in the latter case. For us this means that in the predict call, the predict function needs to extract the training data again and do a "second" training iteration. The trainLearner is somewhat useless.
#'
#' @export
makeRLearner.classif.fdauscdepth = function() {
  makeRLearnerClassif(
    cl = "classif.fdauscdepth",
    package = "fda.usc",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "depth", default = "RP", values = c("RP", "FM", "mode", "RT", "RPD")),
      makeDiscreteLearnerParam(id = "CV", default = "none", values = c("none", "TRUE", "FALSE")),
      # par.depth are the pars for depth
      makeUntypedLearnerParam(id = "par.depth", default = NULL, tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "weights", "single.functional"),
    name = "fdauscdepth",
    short.name = "fdauscdepth"
  )
}

#' @export
trainLearner.classif.fdauscdepth = function(.learner, .task, .subset, .weights = NULL, par.depth, ...) {

  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, functionals.as = "matrix")
  fd = getFunctionalFeatures(d$data)

  # transform the data into fda.usc:fdata class type.
  data.fdclass = fda.usc::fdata(mdata = as.matrix(fd))
  par.d = learnerArgsToControl(list, par.depth)
  if(length(par.d) == 1)
    par.d = par.depth
  #browser()
  fda.usc::classif.depth(group = d$target, fdataobj = data.fdclass,
    par.depth = par.d, ...)
}

#' @export
predictLearner.classif.fdauscdepth = function(.learner, .model, .newdata, ...) {

  # transform the new data into fda.usc:fdata class type.
  fd = getFunctionalFeatures(.newdata)
  nd = fda.usc::fdata(mdata = as.matrix(fd))

  # transform the training data into fda.usc:fdata class type
  tdat = .model$learner.model$fdataobj
  tgroup = .model$learner.model$group
  par.d = .learner$par.vals
  fda.usc::classif.depth(group = tgroup, fdataobj = tdat, newfdataobj = nd,
    par.depth = par.d[["par.depth"]], ...)$group.pred

}
