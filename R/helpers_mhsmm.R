makeBaseParamSet.hmm = function() {
  makeParamSet(
    # Number of states in the model(s)
    makeIntegerLearnerParam(id = "J", lower = 2L, upper = 50L),
    # Initial state probabilities (A N-sized vector)
    makeNumericVectorLearnerParam(id = "init", lower = 0L, upper = 1L,
      default = "equal", special.vals = list("equal", "random")),
    # Initial transition probabilities (A NxN matrix for each class)
    # If trans it is a vector (of numbers smaller than 1) then these are taken to be
    # the diagonal of the transition matrix and the off–diagonal elements are then,
    # within each row, taken to be identical so that the rows sum to 1.  Elements of
    # trans are recycled so as to make the dimensions match.  Under the hood, the
    # matrix is created as, for example:
    # createTransition(c(0.8,0.9),2)
    # [,1] [,2]
    # [1,]  0.8  0.2
    # [2,]  0.1  0.9
    makeNumericVectorLearnerParam(
      id = "trans",
      default = "equal",
      special.vals = list("equal", "random")
    ),
    # Distribution family for the state emissions (one for all the states)
    makeDiscreteLearnerParam(
      id = "family.emission",
      values = c("normal", "poisson", "multinomial", "custom"),
      default = "multinomial",
      tunable = FALSE
    ),
    # Starting values for the parameters of the emission distribution
    # (A list containing the proper values for each model, depending on the param family.emission)
    makeUntypedLearnerParam(
      id = "parms.emission",
      default = "equal",
      special.vals = list("equal", "uniform", "random")
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
      when = "train",
      special.vals = list(NULL),
      default = NULL
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
    )
  )
}

# Wrapper for converting the parameters of the ParamSet in mlr to the aprropiate
# format of mhsmm
# fd.matrix contains the functional data, as a matrix
hmmspecWrapper = function(fd.matrix, J, init, trans, parms.emission, family.emission,
  dens.emission, rand.emission, mstep, ...) {
  factor.levels = NULL
  if (is.integer(fd.matrix)) {
    # Integer matrices are considered as categorical functional data
    factor.levels = levels(as.factor(fd.matrix))
  }

  if (is.character(init)) {
    init = switch(init,
      equal = rep(1/J, J),
      random = {
        aux = runif(J, 0, 1)
        aux/sum(aux) # Sum 1
      }
    )
  }
  if (!is.numeric(trans)) {
    trans = switch(trans,
      equal = matrix(rep(1/J, J^2), J),
      random = generateRandomTransitionMatrix(J)
    )
  } else if (length(trans) == 1L | length(trans) == J) {
    # integer to control the argument of the diagonal values
    trans = mhsmm::createTransition(trans, J)
  } else {
    # The parameter trans is given as a J^2 numeric vector, byBY COLUMN
    trans = matrix(trans, nrow = J, ncol = J)
  }
  if (is.character(parms.emission)) {
    parms.emission = switch(parms.emission,
      equal = switch(
        family.emission,
        normal = list(mu = mean(fd.matrix, na.rm = TRUE), #FIXME Improve efficiency
          sigma = sd(fd.matrix, na.rm = TRUE)
        ),
        poisson = stop("Not implemented yet."), #FIXME
        multinomial = {
          if (is.null(factor.levels))
            stop("Cannot model countinous data with a multinomial distribution")
          else
            list(pmf = matrix(rep(1/length(factor.levels),
              J*length(factor.levels)),
              ncol = J)
            )
        }
      ),
      uniform = stop("uniform parms.emission not implemented yet"),
      random = switch(
        family.emission,
        normal = list(mu = runif(J, min(fd.matrix), max(fd.matrix)),
          sd = runif(J, min(fd.matrix), max(fd.matrix))),
        poisson = stop("Not implemented yet."), #FIXME
        multinomial = list(pmf = generateRandomPMFMatrix(length(factor.levels), J))
      ),
      stop("Wrong special value for parms.emission")
    )
  }

  dens.emission = switch(family.emission,
    normal = mhsmm::dnorm.hsmm,
    poisson = mhsmm::dpois.hsmm,
    multinomial = dmultinom.hsmm,
    custom = dens.emission
  )
  rand.emission = switch(family.emission,
    normal = mhsmm::rnorm.hsmm,
    poisson = mhsmm::rpois.hsmm,
    multinomial = rmultinom.hsmm,
    custom = rand.emission
  )
  mstep = switch(family.emission,
    normal = mhsmm::mstep.norm,
    poisson = mhsmm::mstep.pois,
    multinomial = MstepMultinomial,
    custom = mstep
  )

  mod.start = mhsmm::hmmspec(init = init, trans = trans,
    parms.emission = parms.emission, dens.emission = dens.emission,
    rand.emission = rand.emission, mstep = mstep)

  mod.start$factor.levels = factor.levels

  return(mod.start)
}

# This is needed because of a bug in mhsmm package that sets worngly the default
# value of mstep. the default value of mstep vmust be NULL
hmmfitWrapper = function(x, start.val, mstep = NULL, ...) {
  mhsmm::hmmfit(x, start.val = start.val, mstep = mstep, ...)
}

# Note for every learner implemented
getMhsmmNote = function() {
  "Only functional features are allowed. Only homogeneous HMMs are
    considered, i.e., every state in the model emits observations under the same
  family of distribution. Missing values inside a sequence need of a different symbol
  to NA. The first HMM in the trained model corresponds to the positive class."
}

# format a matrix of functional data (single variable) into the proper format
# for the mhsmm package. Use factor.levels in case of categorical data
# NOTE: It is considered that a NA symbol inside a matrix row represents the
# end of the sequence. If missing symbols are present in the sequence, please
# impute different values on them before calling this function
# FIXME: Dealing with missing values here!
convertFDMatrixToMhsmmData = function(fd.matrix, factor.levels = NULL) {
  mhsmm.data = list(
    x = {
      if (is.null(factor.levels))
        na.omit(c(t(fd.matrix))) # Countinous data
      else
        na.omit(factor(
          t(fd.matrix), # Categorical data
          levels = factor.levels,
          labels = seq(factor.levels) # Interal data are symbols from 1 to the total number of symbols
        ))
    },
    N = apply(fd.matrix, 1, function(x) sum(x %in% factor.levels, na.rm = T))
  )
  class(mhsmm.data) = "hsmm.data"
  return(mhsmm.data)
}


#
# Expectation-Maximization functions
#
dmultinom.hsmm = function(x, j, model) {
  ret <- model$parms.emission$pmf[x, j]
  ret[is.na(ret)] = 1
  ret
}


# dmultinom.hsmm = function(x,j,model) model$parms.emission$pmf[x, j]

#Useful?
rmultinom.hsmm <- function(j,model)
  which(c(rmultinom(1, size = 1, prob = model$parms.emission$pmf[, j])) == 1)

# Deprecated - Use CPP version instead
mstep.multinom = function(x, wt) {
  J = ncol(wt) # Number of states
  size.discrete.space = length(unique(x[!is.na(x)]))
  ans <- matrix(nrow = size.discrete.space, ncol = J)
  for (i in 1:size.discrete.space)
    for (j in 1:J)
      ans[i, j] <- sum(wt[which(x == i), j])/sum(wt[, j], na.rm = TRUE)
  list(pmf = ans)
}

# Generate random right sotchastic matrices
generateRandomTransitionMatrix <- function(J) {
  aux = matrix(runif(J^2), ncol = J)
  t(apply(aux, 1, function(x) x/sum(x)))
}

# Generate random matrix where each column represents the probabily mass function (pmf)
# of each state of a discrete H(S)MM
generateRandomPMFMatrix = function(sizeDiscreteSpace, J) {
  aux = matrix(runif(sizeDiscreteSpace*J), ncol = J)
  apply(aux, 2, function (x) x/sum(x))
}
