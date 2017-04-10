computeAverageMarginalEffects2 = function(model, task,
  features = getTaskFeatureNames(task), gridsize = getTaskSize(task)) {

  data = getTaskData(task)
  tt = getTaskType(task)
  target = if(tt == "classif") "Probability" else getTaskTargetNames(task)
  res = namedList(features)
  pdat = generatePartialDependenceData(model, task, features = features,
    gridsize = gridsize, unique.values = TRUE, derivative = TRUE, method = "simple")$data
  num.feat = vlapply(getTaskData(mtcars.task), is.numeric)
  num.feat = intersect(names(num.feat)[num.feat], features)

  # calculate AMEs for all numerical features
  for (f in num.feat) {
    x.vals = data[[f]]
    pdat.feat = pdat[!is.na(pdat[[f]]), c(target, f)]
    ind = match(x.vals, pdat.feat[[f]])
    y = pdat.feat[[target]][ind]

    # FIXME: remvoe preds here
    r = namedList(c("type", "values", "preds", "effects", "reflev"))

    if (is.numeric(data[, f])) { # handle numeric features
      r$type = "numeric"
      r$values = x.vals
      r$effects = mean(y)
    }
    res[[f]] = r
  }

  cat.feat = setdiff(features, num.feat)
  pdat = generatePartialDependenceData(model, task, features = features, gridsize = gridsize)$data
  # calculate AMEs for all features
  for (f in cat.feat) {
    vals = pdat[, f]
    valsok = !is.na(vals)
    vals = vals[valsok]
    nvals = length(vals)
    y = pdat[, target]
    y = y[valsok]

    r = namedList(c("type", "values", "preds", "effects", "reflev"))
    r$preds = y

    if (is.factor(data[, f])) { # handle factors
      # FIXME: user wants to select reflev from outside
      reflev = vals[1L]
      r$type = "factor"
      r$values = as.character(vals)
      r$reflev = as.character(reflev)
      r$effects = y - y[reflev]
      r$effects[reflev] = NA_real_
    }

    # if (is.numeric(data[, f])) { # handle numeric features
    #   r$type = "numeric"
    #   r$values = vals
    #   # FIXME: the ame effect should be interpretable as what happens if x increases by one,
    #   #        i.e. using only mean(diff(y)) can be wrong as this does not take into account the "grid-step-size" of the x-values
    #   r$effects = mean(diff(y)/diff(vals))
    # }
    res[[f]] = r
  }
  res = addClasses(res, "AverageMarginalEffects")
  return(res)
}

# FIXME: WE HAVE TO TELL generatePartialDependenceData the VALS!!!! FROM THE OUTSIDE
# OR STUFF COULD CHANGE DUE TO THE BOOTSTRAP!
computeAverageMarginalEffectsResampled = function(model, task, features = getTaskFeatureNames(task), resampling = NULL) {
  if (is.null(resampling)) {
    resample.iters = 2L
    resampling = makeResampleDesc("Bootstrap", iters = resample.iters)
  } else {
    resample.iters = if (inherits(reampling, "ResampleDesc"))
      resampling$iters
    else
      resampling$desc$iters
  }

  z = resample(lrn, task, resampling, show.info = FALSE, extract = function(model) {
    computeAverageMarginalEffects(model, task, features)
  })
  ex = z$extract
  res = namedList(features)
  getErr = function(xsd) qnorm(0.975) * xsd
  getPValue = function(xmu, xsd) 2 * pt(-abs(xmu / xsd), df = resampling.iters - 1L)
  setResamplingAggr = function(x) {
    mat = asMatrixRows(lapply(ex, function(r) r[[f]][[x]]))
    xmu = colMeans(mat)
    xsd = apply(mat, 2L, sd)
    # FIXME: we should expose alpha here
    xerr = qnorm(0.975) * xsd
    r[[x]] <<- xmu
    r[[paste0(x, ".sd")]] <<- xsd
    r[[paste0(x, ".err")]] <<- xerr
    # FIXME: check this formula! what is best for boostrap?
    r[[paste0(x, ".p")]] <<- 2 * pt(-abs(xmu / xsd), df = resample.iters - 1L)
    r[[paste0(x, ".ci.low")]] <<- xmu - xerr
    r[[paste0(x, ".ci.upp")]] <<- xmu + xerr
  }
  for (f in features) {
    # FIXME: this might go wrong!!
    r = ex[[1L]][[f]]
    setResamplingAggr("preds")
    setResamplingAggr("effects")
    res[[f]] = r
  }
  res = addClasses(res, "AverageMarginalEffectsResampled")
  return(res)
}


print.AverageMarginalEffectsResampled = function(x, ...) {
  feats = names(x)
  d = data.frame()
  for (f in feats) {
    xf = x[[f]]
    if (xf$type == "numeric") {
      drow = data.frame(mu = xf[["effects"]], l = xf[["effects.ci.low"]], u = xf[["effects.ci.upp"]], p = xf[["effects.p"]])
      d = rbind(d, drow)
      rownames(d)[nrow(d)] = f
    } else if (xf$type == "factor") {
      for (j in seq_along(xf$values)) {
        lev = xf$values[j]
        if (lev != xf$reflev) {
          drow = data.frame(mu = xf[["effects"]][j], l = xf[["effects.ci.low"]][j], u = xf[["effects.ci.upp"]][j], p = xf[["effects.p"]][j])
          d = rbind(d, drow)
          rownames(d)[nrow(d)] = paste0(f, ".", lev)
        }
      }
    }
  }
  print(d)
  # print(eff.mats)
  # e = rbind(x$)
}
