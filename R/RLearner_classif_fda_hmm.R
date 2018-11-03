# FIXME Complete documentation of every parameter (ParamHelpers?)
# FIXME Bigger note
# FIXME Add "kmeans" as special.val for parms.emission
# FIXME Add "uniform" as special.val for parms.emission (split dataset and compute parms)
# FIXME Mstep discrete with Rcpp
# FIXME Problemas al usar arma::accu() debido a su bjaa precisión?
# FIXME missing values? missing values in mstep.multinom?
# FIXME Warnings neen la prediccion?
# FIXME Define correctly the forbidden region of each param
# FIXME Convertir los parametros init, trans en vector y en matriz respectivamente
#'@export
#'
makeRLearner.classif.fda.hmm = function() {
  makeRLearnerClassif(
    cl = "classif.fda.hmm",
    package = "mhsmm",
    par.set = c(
      dropParams(makeBaseParamSet.hmm(), "J"),
      makeParamSet(
        makeIntegerVectorLearnerParam(id = "J", len = 2L, lower = 2L, upper = 50L),
        # Prediction method
        makeDiscreteLearnerParam(
          id = "method",
          values = c("viterbi", "smoothed"),
          default = "viterbi",
          when = "predict",
          tunable = FALSE
        ),
        keys = c("J"),
        forbidden = expression(any(J <= 1))
      )
    ),
    par.vals = list(
      J = c(3, 4),
      init = "equal",
      trans = "createTransition",
      family.emission = "multinomial",
      parms.emission = "equal",
      missing.value.symbol = "."
    ),
    properties = c("single.functional", "twoclass", "prob", "numerics", "missings"),
    name = "Hidden Markov Models (HMMs) on FDA",
    short.name = "fda.hmm",
    note = getMhsmmNote(),
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
  names(J) = class.levels.ordered

  # Listify parameters that are common for all the HMMs in the classifier
  if (is.character(init)) init = setNames(rep(init, length(class.levels.ordered)), class.levels.ordered)
  if (is.character(trans)) trans = setNames(rep(trans, length(class.levels.ordered)), class.levels.ordered)
  if (is.character(parms.emission)) parms.emission = setNames(rep(parms.emission, length(class.levels.ordered)), class.levels.ordered)

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


