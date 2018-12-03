########################################
### MHSMM MODELS
########################################

#' @export mhsmm.trainingIterations
#' @rdname measures
#' @format none
mhsmm.trainingIterations = makeMeasure(
  id = "mhsmm.trainingIterations",
  minimize = FALSE,
  properties = c("req.model", "req.task"),
  fun = function(task, model, pred, feats, extra.args) {
    measureMhsmmTrainingIterations(model)
  },
  best = Inf,
  worst = 1,
  name = "Number of iterations needed to converge",
  note = "It is weird that we tryu to maximize this value. We want to maximize it to ensure
  that the model is being fitted correctly."
)

#' @export measureMhsmmTrainingIterations
#' @rdname measures
#' @format none
measureMhsmmTrainingIterations = function(model) {
  assert(checkClass(model$learner.model, "hmm"), checkClass(model$learner.model, "list"), combine = "or")
  if (class(model$learner.model) == "list")
    min(purrr::map_int(model$learner.model, ~ length(.$loglik)))
  else
    length(model$learner.model$loglik)
}

#' @export mhsmm.loglik
#' @rdname measures
#' @format none
mhsmm.loglik = makeMeasure(
  id = "mhsmm.loglik",
  minimize = FALSE,
  properties = c("req.model"),
  fun = function(task, model, pred, feats, extra.args) {
    measureMhsmmLogLik(model)
  },
  best = 0,
  worst = -Inf,
  name = "Log-likelihood of the fitted model",
  note = "Computed on training data, not on test data"
)

#' @export mhsmm.loglik
#' @rdname measures
#' @format none
measureMhsmmLogLik = function(model) {
  assert(checkClass(model$learner.model, "hmm"), checkClass(model$learner.model, "list"), combine = "or")
  if (class(model$learner.model) == "list")
    min(purrr::map_int(model$learner.model, ~ .$loglik))
  else
    model$learner.model$loglik
}

#' @export mhsmm.sll
#' @rdname measures
#' @format none
mhsmm.sll = makeMeasure(
  id = "mhsmm.sll",
  minimize = FALSE,
  properties = c("req.model"),
  fun = function(task, model, pred, feats, extra.args) {
    NULL
  },
  extra.args = list(),
  worst = -Inf,
  best = 0.0,
  name = "Sequence Log-Likelihood",
  note = "See 'Using hidden semi-Markov models for effective online failure prediction' for the original description"
)

#' @export mhsmm.agp
#' @rdname measures
#' @format none
mhsmm.agp = makeMeasure(
  id = "mhsmm.agp",
  minimize = FALSE,
  properties = c("req.model"),
  fun = function(task, model, pred, feats, extra.args) {
    NULL
  },
  extra.args = list(),
  worst = 0,
  best = 1,
  name = "Accuracy of Generated Predictions (AGP)",
  note = "See 'Modelling Behaviour in UAV Operations Using Higher Order Double Chain Markov Models'"
)

#' @export mhsmm.mpgp
#' @rdname measures
#' @format none
mhsmm.mpgp = makeMeasure(
  id = "mhsmm.mpgp",
  minimize = FALSE,
  properties = c("req.model"),
  fun = function(task, model, pred, feats, extra.args) {
    NULL
  },
  extra.args = list(),
  aggr = test.mean,
  best = 1,
  worst = 0,
  name = "Minimum Precision of Generated Predictions (MPGP)",
  note = "See 'Modelling Behaviour in UAV Operations Using Higher Order Double Chain Markov Models'"
)

#' @export mhsmm.bic
#' @rdname measures
#' @format none
mhsmm.bic = makeMeasure(
  id = "mhsmm.bic",
  minimize = TRUE,
  properties = c("req.model", "req.task"),
  fun = function(task, model, pred, feats, extra.args) {
    NULL
  },
  extra.args = list(),
  aggr = train.mean,
  best = Inf,
  worst = 0,
  name = "Bayesian Information Criterion (BIC)",
  note = "Nothing to declare."
)

#' @export mhsmm.chpt
#' @rdname measures
#' @format none
mhsmm.chpt = makeMeasure(
  id = "mhsmm.chpt",
  minimize = FALSE,
  properties = c("req.model"),
  fun = function(task, model, pred, feats, extra.args) {
    NULL
  },
  extra.args = list(),
  aggr = train.mean,
  best = 1,
  worst = 0,
  name = "Coefficient of High Probability Transitions (CHPT)"
)


