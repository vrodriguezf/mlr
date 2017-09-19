#' @title Classification of functional data by rpart.
#'
#' @description
#' Learner for classification using rpart model.
#'
#' @export
makeRLearner.classif.fdausctree = function() {
  makeRLearnerClassif(
    cl = "classif.fdausctree",
    package = "fda.usc",
    par.set = makeParamSet(
      makeUntypedLearnerParam(id = "basis.x"),
      makeUntypedLearnerParam(id = "basis.b"),
      makeLogicalLearnerParam(id = "CV", default = FALSE)
    ),
    properties = c("twoclass", "multiclass", "weights", "functionals"),
    name = "fdausctree",
    short.name = "fdausctree"
  )
}

#' @export
trainLearner.classif.fdausctree = function(.learner, .task, .subset, .weights = NULL, ...) {

  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, functionals.as = "matrix")
  fd = getFunctionalFeatures(d$data)
  # transform the data into fda.usc:fdata class type.
  data.fdclass = fda.usc::fdata(mdata = as.matrix(fd))
  # transform the data into fda.usc:fdata class type and save in a list
  dat = list(df = data.frame(d$target), x = data.fdclass)

  model = fda.usc::classif.tree(d.target ~ x, data = dat)
  # Fix bug in package
  model$C[[1]] = quote(classif.tree)
  return(model)
}

#' @export
predictLearner.classif.fdausctree = function(.learner, .model, .newdata, ...) {

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
