# @export
# @rdname TuneControl
makeTuneMultiCritControlMBO = function(same.resampling.instance = TRUE,
  learner, mbo.control, continue = FALSE, log.fun = "default",
  final.dw.perc = NULL, budget = NULL, mbo.keep.result = FALSE, mbo.design = NULL) {

  assertClass(learner, classes = "Learner")
  assertClass(mbo.control, "MBOControl")
  assertFlag(continue)
  assertFlag(mbo.keep.result)

  if (!is.null(budget) && !is.null(mbo.design) && nrow(mbo.design) > budget)
    stopf("The size of the initial design (init.design.points = %i) exceeds the given budget (%i).",
      nrow(mbo.design), budget)
  else if (!is.null(budget)) {
    if (!is.null(mbo.control$stop.conds))
      warning("The mbo.control object already has a stopping condition. However we add another one respecting the budget.", mbo.control$init.design.points, budget)
    setMBOControlTermination = get("setMBOControlTermination", envir = getNamespace("mlrMBO")) # FIXME: Remove if mlrMBO hits CRAN
    mbo.control = setMBOControlTermination(mbo.control, max.evals = budget)
  }

  x = makeTuneMultiCritControl(same.resampling.instance = same.resampling.instance,
    start = NULL, cl = "TuneMultiCritControlMBO", log.fun = log.fun,
    final.dw.perc = final.dw.perc, budget = budget)
  x$learner = learner
  x$mbo.control = mbo.control
  x$continue = continue
  x$mbo.keep.result = mbo.keep.result
  x$mbo.design = mbo.design
  return(x)
}
