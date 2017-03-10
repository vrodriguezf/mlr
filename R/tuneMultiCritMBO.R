tuneMultiCritMBO = function(learner, task, resampling, measures, par.set, control,
  opt.path, show.info) {

  # requirePackages("mlrMBO", why = "tuneMBO", default.method = "load")
  mbo.control = control$mbo.control

  # set final evals to 0 to save time. we dont really need final evals in this context.
  mbo.control$final.evals = 0L

  # put all required info into the function env
  force(learner); force(task); force(resampling); force(measures); force(par.set); force(control); force(opt.path); force(show.info)
  n.objectives = mbo.control$n.objectives
  tff = tunerMultiCritSmoofFun(learner = learner, task = task, resampling = resampling, measures = measures,
    par.set = par.set, ctrl = control, n.objectives = n.objectives, opt.path = opt.path, show.info = show.info,
    convertx = convertXIdentity, remove.nas = TRUE)

  state = mbo.control$save.file.path
  if (control$continue && file.exists(state)) {
    messagef("Resuming previous MBO run using state in '%s'...", state)
    # FIXME: remove this when mbo on cran
    mbofun = get("mboContinue", envir = getNamespace("mlrMBO"))
    or = mbofun(state)
  } else {
    # FIXME: remove this when mbo on cran
    mbofun = get("mbo", envir = getNamespace("mlrMBO"))
    or = mbofun(tff, design = control$mbo.design, learner = control$learner, control = mbo.control, show.info = FALSE)
  }

  j = getOptPathParetoFront(opt.path, index = TRUE)
  els = lapply(j, getOptPathEl, op = opt.path)
  xs = extractSubList(els, "x", simplify = FALSE)
  xs = lapply(xs, trafoValue, par = par.set)
  xs = lapply(xs, removeMissingValues)
  ys = extractSubList(els, "y", simplify = "rows")
  colnames(ys) = opt.path$y.names

  # we take the point that mbo proposes and its estimated y
  # FIXME: threshold
  if (!control$mbo.keep.result)
    or = NULL
  res = makeTuneMultiCritResult(learner, j, xs, ys, control, opt.path, measures)
  res

}
