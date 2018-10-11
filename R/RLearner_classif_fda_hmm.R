# FIXME Complete documentation of every parameter (ParamHelpers?)
# FIXME Big note
# FIXME Add "kmeans" as special.val for parms.emission
# FIXME Add "uniform" as special.val for parms.emission (split dataset and compute parms)
# FIXME Change the indentation of Rstudio?
# FIXME Remove NA's from the end of the sequences
# FIXME Check that missing values are well treated
# FIXME Mstep discrete with Rcpp
# FIXME Parms.emission for discrete case are better transposing columns ? (i.e, with states as columns)
#'@export
makeRLearner.classif.fda.hmm = function() {
  makeRLearnerClassif(
    cl = "classif.fda.hmm",
    package = "mhsmm",
    par.set = makeParamSet(
      # Number of states in the model(s)
      makeIntegerVectorLearnerParam(id = "J", len = 2L, lower = 1L, upper = 50L),
      # Initial state probabilities (A list with a N-sized vector for each class)
      makeUntypedLearnerParam(id = "init", default = "equal", special.vals = list("equal")),
      # Initial transition probabilities (A list with a NxN matrix for each class)
      makeUntypedLearnerParam(
        id = "trans",
        default = "equal",
        special.vals = list("equal", "createTransition")
      ),
      # Distribution family for the state emissions (one for all the states)
      makeDiscreteLearnerParam(
        id = "family.emission",
        values = c("normal", "poisson", "multinomial", "custom"),
        default = "multinomial"
      ),
      # Starting values for the parameters of the emission distribution
      # (A list containing the proper values for each model, depending on the param family.emission)
      makeUntypedLearnerParam(
        id = "parms.emission",
        default = "equal",
        special.vals = list("equal", "uniform")
      ),
      # Density function of the emission distribution (for custom families)
      makeFunctionLearnerParam(
        id = "dens.emission",
        requires = quote(family.emission == "custom"),
        tunable = FALSE,
        when = "train"
      ),
      # The function used to generate observations from the emission distribution (for custom families)
      makeFunctionLearnerParam(
        id = "rand.emission",
        requires = quote(family.emission == "custom"),
        tunable = FALSE,
        when = "train"
      ),
      # Re-estimates the parameters of density function on each iteratio (M step of the EM algorithm)
      makeFunctionLearnerParam(
        id = "mstep",
        requires = quote(family.emission == "custom"),
        tunable = FALSE,
        when = "train"
      ),
      # Missing value symbol (Different than NA, NA is used to make the length of each input sequence equal)
      makeUntypedLearnerParam(
        id = "missing.value.symbol",
        default = ".",
        tunable = FALSE,
        when = "both"
      ),
      # Maximum number of iterations of the EM algorithm
      makeIntegerLearnerParam(
        id = "maxit",
        lower = 1L,
        default = 1000L,
        when = "train"
      ),
      # Convergence tolerance of the EM algorithm
      makeNumericLearnerParam(
        id = "tol",
        lower = 0,
        default = 1e-08,
        when = "train"
      ),
      # If TRUE will not re-estimate the transition matrix
      makeLogicalLearnerParam(
        id = "lock.transition",
        default = FALSE,
        when = "train",
        tunable = TRUE
      ),
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
    ),
    par.vals = list(
      J = c(3, 4),
      init = "equal",
      trans = "createTransition",
      family.emission = "multinomial",
      parms.emission = "equal",
      missing.value.symbol = "."
    ),
    properties = c("functionals", "twoclass", "prob"),
    name = "Hidden Markov Models (HMMs) on FDA",
    short.name = "fda.hmm",
    note = "Only functional features are allowed. Only homogeneous HMMs are
    considered, i.e., every state in the model emits observations under the same
    family of distribution. Missing values inside a sequence need of a different symbol
    to NA. The first HMM in the trained model corresponds to the positive class.",
    callees = c("hmmspec", "hmmfit", "predict.hmm", "dnorm.hsmm", "mstep.hsmm",
                "rnorm.hsmm", "createTransition")
  )
}

#'@export
trainLearner.classif.fda.hmm = function(.learner, .task, .subset, .weights = NULL,
                                        J, init, trans, family.emission,
                                        parms.emission, missing.value.symbol, ...) {

  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, functionals.as = "matrix")
  fd = getFunctionalFeatures(d$data)
  fd.matrix = fd[[1]]

  factor.levels = NULL
  if (is.integer(fd.matrix)) {
    # Integer matrices are considered as categorical functional data
    factor.levels = levels(as.factor(fd.matrix))
  }

  if (ncol(fd) > 1)
    stop("Only 1 functional feature is allowed.")

  # Create one hmm for each of the classes (The first one always correspond to the positive class)
  dots = list(...)
  td = getTaskDesc(.task)
  # fd.matrix.by.class = purrr::imap(td$class.distribution, ~ fd.matrix[d$target == .y, ])

  # Integer data is considered as a discrete (categorical variable)

  # Parameters for the specification of the HMMs
  class.levels.ordered = c(td$positive, td$negative) # It is assumed that the first model is the positive class
  names(J) = class.levels.ordered
  if (is.character(init)) {
    init = switch(init,
      equal = purrr::map(class.levels.ordered,
        ~ rep(1/J[.], J[.]))
    )
  }
  if (is.character(trans)) {
    trans = switch(trans,
      equal = purrr::map(class.levels.ordered,
        ~ matrix(rep(1/J[.], J[.]^2), J[.])),
      createTransition = purrr::map(class.levels.ordered,
        ~ mhsmm::createTransition(0.9, J[.]))
    )
  }
  if (is.character(parms.emission)) {
    parms.emission = switch(parms.emission,
      equal = purrr::map(
        class.levels.ordered,
        ~ switch(
          family.emission,
          normal = list(mu = mean(fd.matrix.by.class[[.]],
            na.rm = TRUE),
            sigma = sd(fd.matrix.by.class[[.]],
              na.rm = TRUE)
          ),
          poisson = stop("Not implemented yet."), #FIXME
          multinomial = {
            if (is.null(factor.levels))
              stop("Cannot model countinous data with a multinomial distribution")
            else
              list(pmf = matrix(rep(1/length(factor.levels),
                J[.]*length(factor.levels)),
                ncol = J[.])
              )
          }
        ),
        uniform = stop("uniform parms.emission not implemented yet"),
        stop("Wrong special value for parms.emission")
      )
    )
  }
  dens.emission = switch(family.emission,
    normal = mhsmm::dnorm.hsmm,
    poisson = mhsmm::dpois.hsmm,
    multinomial = dmultinom.hsmm,
    custom = dots$dens.emission
  )
  rand.emission = switch(family.emission,
    normal = mhsmm::rnorm.hsmm,
    poisson = mhsmm::rpois.hsmm,
    multinomial = rmultinom.hsmm,
    custom = dots$rand.emission
  )
  mstep = switch(family.emission,
    normal = mhsmm::mstep.norm,
    poisson = mhsmm::mstep.pois,
    multinomial = mstep.multinom,
    custom = dots$mstep
  )
  mods = purrr::imap(
    class.levels.ordered,
    function(class.level, index) {
      # Create the hmm specification
      mod.start = mhsmm::hmmspec(init = init[[index]], trans = trans[[index]],
        parms.emission = parms.emission[[index]], dens.emission = dens.emission,
        rand.emission = rand.emission, mstep = mstep)
      # Create the hsmm.data object with the sequences of a class.level
      fd.matrix.class.level = fd.matrix[d$target == class.level, ]
      factor.levels = NULL
      if (is.integer(fd.matrix.class.level)) {
        # Integer matrices are considered as categorical functional data
        factor.levels = levels(as.factor(fd.matrix.class.level))
      }

      hsmm.data.class = list(
        x = {
          if (is.null(factor.levels))
            t(fd.matrix.class.level) # Countinous data
          else
            factor(
              t(fd.matrix.class.level), # Categorical data
              levels = factor.levels,
              labels = seq(factor.levels)
            )
        },
        N = rep(ncol(fd.matrix.class.level), nrow(fd.matrix.class.level)) # FIXME we can replace sum(...) by class.disttribution or nrow
      )
      class(hsmm.data.class) = "hsmm.data"
      mod.fit = mhsmm::hmmfit(x = hsmm.data.class, start.val = mod.start, mstep = mstep, ...)
      if (!is.null(factor.levels))
        mod.fit$factor.levels = factor.levels
      mod.fit
    }
  )
  return(setNames(mods, class.levels.ordered))
}

##
# HELPERS
#
dmultinom.hsmm = function(x, j, model) {
  ret <- model$parms.emission$pmf[x, j]
  ret[is.na(ret)] = 1
  ret
}

# dmultinom.hsmm = function(x,j,model) model$parms.emission$pmf[x, j]

rmultinom.hsmm <- function(j,model)
  which(c(rmultinom(1, size = 1, prob = model$parms.emission$pmf[, j])) == 1)

mstep.multinom = function(x, wt) {
  J = ncol(wt) # Number of states
  size.discrete.space = length(unique(x[!is.na(x)]))
  ans <- matrix(nrow = size.discrete.space, ncol = J)
  for (i in 1:size.discrete.space)
    for (j in 1:J)
      ans[i, j] <- sum(wt[which(x[!is.na(x)] == i), j])/sum(wt[!is.na(x), j])
  list(pmf = ans)
}
