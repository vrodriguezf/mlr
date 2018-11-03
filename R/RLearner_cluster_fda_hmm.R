#'@export
makeRLearner.cluster.fda.hmm = function() {
  makeRLearnerCluster(
    cl = "cluster.fda.hmm",
    package = "mhsmm",
    par.set = makeBaseParamSet.hmm(),
    par.vals = list(
      J = 3,
      init = "equal",
      trans = "createTransition",
      family.emission = "multinomial",
      parms.emission = "equal",
      missing.value.symbol = ".",
      rand.emission = NULL
    ),
    properties = c("single.functional", "prob", "numerics", "missings"),
    name = "A single Hidden Markov Model (HMM) to cluster multiple observation sequences",
    short.name = "fda.hmm",
    note = getMhsmmNote(),
    callees = c("hmmspec", "hmmfit", "dnorm.hsmm", "mstep.hsmm",
      "rnorm.hsmm", "createTransition")
  )
}

#'@export
trainLearner.cluster.fda.hmm = function(.learner, .task, .subset, .weights = NULL,
  J, init, trans, family.emission, parms.emission, dens.emission, rand.emission,
  mstep, missing.value.symbol, ...) {

  # Get and transform functional data
  d = getTaskData(.task, subset = .subset, target.extra = TRUE, functionals.as = "matrix")
  fd = getFunctionalFeatures(d$data)
  fd.matrix = fd[[1]]

  if (ncol(fd) > 1) stop("Only 1 functional feature is allowed.")

  # Model specification
  mod.start = hmmspecWrapper(fd.matrix, J, init, trans, parms.emission, family.emission,
    dens.emission, rand.emission, mstep, ...)

  # Formatting input data
  mhsmm_data = convertFDMatrixToMhsmmData(fd.matrix, mod.start$factor.levels)

  mod.fit = mhsmm::hmmfit(mhsmm_data, mod.start, mstep = mod.start$mstep, ...)
  if (!is.null(mod.start$factor.levels))
    mod.fit$factor.levels = mod.start$factor.levels
  mod.fit
}

# Each sequence is assigned to the most frequent state (cluster) in the viterbi
# decoding
#'@export
predictLearner.cluster.fda.hmm = function(.learner, .model, .newdata, ...) {
  fd = getFunctionalFeatures(.newdata)
  fd.matrix = fd[[1]]
  if (ncol(fd) > 1) stop("Only 1 functional feature is allowed.")
  # Format newdata for package mhsmm
  newdata.mhsmm = convertFDMatrixToMhsmmData(fd.matrix,
    .model$learner.model$factor.levels)
  # Call the viterbi algorithm
  viterbi = mhsmm::predict.hmm(object = .model$learner.model,
    newdata = newdata.mhsmm,
    method = "viterbi")
  # Get the most frequent state for each sequence in .newdata
  viterbi.aux = split(viterbi$s, rep(1:length(viterbi$N), viterbi$N))
  switch(.learner$predict.type,
    response = purrr::map_int(viterbi.aux, ~ which.max(table(.))),
    prob = t(sapply(viterbi.aux, function(x) as.numeric(table(x)/length(x))))
  )
}
