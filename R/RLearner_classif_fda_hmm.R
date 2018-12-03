# FIXME Complete documentation of every parameter (ParamHelpers?)
# FIXME Bigger note
# FIXME Add "kmeans" as special.val for parms.emission
# FIXME Add "uniform" as special.val for parms.emission (split dataset and compute parms)
# FIXME Mstep discrete with Rcpp
# FIXME Problemas al usar arma::accu() debido a su bjaa precisión?
# FIXME missing values? missing values in mstep.multinom?
# FIXME Warnings neen la prediccion?
# FIXME Define correctly the forbidden region of each param
#'@export
#'
makeRLearner.classif.fda.hmm = function() {
  makeRLearnerClassif(
    cl = "classif.fda.hmm",
    package = "mhsmm",
    par.set = makeBaseParamSet.hmm(),
    par.vals = list(
      J = 3,
      init = "equal",
      trans = 0.8,
      family.emission = "multinomial",
      parms.emission = "equal",
      missing.value.symbol = "."
    ),
    properties = c("single.functional", "twoclass", "prob", "numerics", "missings"),
    name = "Hidden Markov Models (HMMs) on FDA",
    short.name = "fda.hmm",
    note = paste(getMhsmmNote(), "As a restriction, all the HMMs in the classifier
      must have the same number of states"),
    callees = c("hmmspec", "hmmfit", "dnorm.hsmm", "mstep.hsmm",
      "rnorm.hsmm", "createTransition")
  )
}

# Random place to put the necessary attributes for Rcpp
#' @useDynLib mlr
#' @importFrom Rcpp sourceCpp
#'@export
trainLearner.classif.fda.hmm = function(.learner, .task, .subset, .weights = NULL,
  J, init, trans, family.emission, parms.emission, dens.emission, rand.emission,
  mstep, missing.value.symbol, ...) {

  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, functionals.as = "matrix")
  fd = getFunctionalFeatures(d$data)
  fd.matrix = fd[[1]]

  if (ncol(fd) > 1)
    stop("Only 1 functional feature is allowed.")

  #
  # Create one hmm for each of the classes (The first one always correspond to the positive class)
  #
  td = getTaskDesc(.task)
  class.levels.ordered = c(td$positive, td$negative) # It is assumed that the first model is the positive class
  nclasses = length(class.levels.ordered)
  #names(J) = class.levels.ordered

  # Listify parameters that are common for all the HMMs in the classifier
  #FIXME Make the number of states flexible
  J = setNames(purrr::rerun(nclasses, J), class.levels.ordered)
  init = setNames(purrr::rerun(nclasses, init), class.levels.ordered)
  trans = setNames(purrr::rerun(nclasses, trans), class.levels.ordered)
  parms.emission = setNames(purrr::rerun(nclasses, parms.emission), class.levels.ordered)
  # if (length(init) == 1L)
  #   init = setNames(rep(init, length(class.levels.ordered)), class.levels.ordered)
  # if (length(trans) == 1L) trans = setNames(rep(trans, length(class.levels.ordered)), class.levels.ordered)
  # if (length(parms.emission) == 1L) parms.emission = setNames(rep(parms.emission, length(class.levels.ordered)), class.levels.ordered)

  # Parameters for the specification of the HMMs
  mods = purrr::imap(
    class.levels.ordered,
    function(class.level, index) {
      fd.matrix.class.level = fd.matrix[d$target == class.level, ]

      # Create the hmm specification
      mod.start = hmmspecWrapper(fd.matrix.class.level, J[[index]], init[[index]],
        trans[[index]], parms.emission[[index]], family.emission, dens.emission,
        rand.emission, mstep, ...)

      # Create the hsmm.data object with the sequences of a class.level
      hsmm.data.class = convertFDMatrixToMhsmmData(fd.matrix.class.level,
        mod.start$factor.levels)

      # Fit the hmm
      mod.fit = mhsmm::hmmfit(x = hsmm.data.class, start.val = mod.start,
        mstep = mod.start$mstep, ...)
      if (!is.null(mod.start$factor.levels))
        mod.fit$factor.levels = mod.start$factor.levels

      mod.fit$initial = mod.start
      mod.fit
    }

  )
  return(setNames(mods, class.levels.ordered))
}

#' @export
predictLearner.classif.fda.hmm = function(.learner, .model, .newdata, ...) {

  fd = getFunctionalFeatures(.newdata)
  fd.matrix = fd[[1]]

  # Get the likelihood of each sequence in the object .newdata for each hmm in
  # the object .model
  posterior.likelihoods = apply(fd.matrix, MARGIN = 1, FUN = function(case) {
    purrr::map(.model$learner.model, function(mod.class) {
      prediction = suppressWarnings(
        mhsmm::predict.hmm(
          object = mod.class,
          newdata = {
            if (is.null(mod.class$factor.levels))
              na.omit(case)
            else
              na.omit(factor(
                case, # Categorical data
                levels = mod.class$factor.levels,
                labels = seq(mod.class$factor.levels)
              ))
          },
          method = "smoothed"
        )
      )
      return(prediction$loglik)
    })
  })

  if (.learner$predict.type == "response") {
    return(as.factor(purrr::map_chr(posterior.likelihoods, ~ names(which.max(purrr::simplify(.))))))
  } else {
    posterior.likelihoods.m = matrix(unlist(posterior.likelihoods, use.names = FALSE),
      ncol = length(posterior.likelihoods[[1]]), byrow = TRUE)
    colnames(posterior.likelihoods.m) = names(posterior.likelihoods[[1]])
    return(posterior.likelihoods.m)
  }
}


