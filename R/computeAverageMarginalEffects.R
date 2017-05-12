computeAverageMarginalEffects = function(model, task,
  features = getTaskFeatureNames(task), gridsize = NULL,
  uniform = FALSE, deriv.method = "simple") {

  data = getTaskData(task)
  tt = getTaskType(task)
  target = if (tt == "classif") "Probability" else getTaskTargetNames(task)
  res = namedList(features)
  cat.feat = intersect(features, names(getTaskFactorLevels(task)))
  num.feat = setdiff(features, names(getTaskFactorLevels(task)))

  if (length(cat.feat) != 0)
    pdat = generatePartialDependenceData(obj = model, input = task, features = cat.feat,
      gridsize = gridsize, uniform = uniform)$data
  # FIXME: possible speedup if we use uniform = FALSE and the derivative function below

  if (length(num.feat) != 0)
    deriv = generatePartialDependenceData(obj = model, input = task, features = num.feat,
      gridsize = gridsize, uniform = uniform, derivative = TRUE, method = deriv.method)$data

  # calculate AMEs for all features
  for (f in features) {
    r = namedList(c("type", "values", "effects", "reflev"))

    if (is.factor(data[, f])) { # handle factors
      vals = pdat[, f]
      valsok = !is.na(vals)
      vals = vals[valsok]
      y = pdat[, target]
      y = y[valsok]

      r$type = "factor"
      r$values = levels(vals)
      #r$preds = y
      # FIXME: user wants to select reflev from outside
      reflev = levels(vals)[1L]
      r$reflev = as.character(reflev)
      y = setNames(y, vals)
      r$effects = y - y[reflev]
      r$effects = r$effects[!duplicated(r$effects)][levels(vals)]
      r$effects[reflev] = NA_real_
    }

    if (is.numeric(data[, f])) { # handle numeric features
      vals = deriv[, f]
      valsok = !is.na(vals)
      vals = vals[valsok]
      y = deriv[, target]
      y = y[valsok]

      r$type = "numeric"
      r$values = vals
      r$effects = mean(y)
    }
    res[[f]] = r
  }
  res = addClasses(res, "AverageMarginalEffects")
  return(res)
}

splitPD = function(x, y, max.splits) {
  # FIXME: How to solve http://stackoverflow.com/questions/27862280/tree-sizes-given-by-cp-table-in-rpart
  mod = rpart::rpart(y ~ x, cp = 0, maxcompete = 0,
    minsplit = 1, minbucket = 1, xval = 0, maxsurrogate = 0)
  cp.ind = max(which(mod$cptable[,"nsplit"] <= max.splits))
  mod = rpart::prune(mod, cp = mod$cptable[cp.ind, "CP"])
  unname(mod$splits[,"index"])
}

computeAMEwithLM = function(model, task,
  features = getTaskFeatureNames(task), gridsize = NULL,
  uniform = FALSE, weights = TRUE) {

  data = getTaskData(task)
  tt = getTaskType(task)
  target = if (tt == "classif") "Probability" else getTaskTargetNames(task)
  res = namedList(features)

  pdat = generatePartialDependenceData(obj = model, input = task, features = features,
    gridsize = gridsize, uniform = uniform)$data

  # calculate AMEs for all features
  for (f in features) {
    r = namedList(c("type", "values", "effects", "reflev"))
    vals = pdat[, f]
    valsok = !is.na(vals)
    vals = vals[valsok]
    y = pdat[, target]
    y = y[valsok]

    if (is.factor(data[, f])) { # handle factors
      r$type = "factor"
      r$values = as.character(vals)
      #r$preds = y
      # FIXME: user wants to select reflev from outside
      reflev = levels(vals)[1L]
      r$reflev = as.character(reflev)
      y = setNames(y, vals)
      r$effects = y - y[reflev]
      r$effects = r$effects[!duplicated(r$effects)][levels(vals)]
      r$effects[reflev] = NA_real_
    }

    if (is.numeric(data[, f])) { # handle numeric features
      # FIXME: this is not the same as averaging tangents...
      if (weights) w = sum(vals)/vals else w = rep(1, length(vals))
      slope = lm.wfit(x = as.matrix(cbind(mean(y), vals)), y = y, w = w)$coefficients[2]

      r$type = "numeric"
      r$values = vals
      r$effects = unname(slope)
    }
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

effects.AverageMarginalEffects = function(x) {
  unlist(lapply(x, function(l) l$effects))
}

derivative = function(model, task, feature, eps = 1e-8){
  setstep = function(x) {
    x + (max(abs(x), 1, na.rm = TRUE) * sqrt(eps)) - x
    #sqrt(eps)*x
  }
  data = getTaskData(task)
  feature.vals = data[[feature]]
  tt = getTaskType(task)
  getPred = if (tt == "regr") getPredictionResponse else getPredictionProbabilities
  # calculate numerical derivative
  feat0 = feature.vals - setstep(feature.vals)
  feat1 = feature.vals + setstep(feature.vals)
  # FIXME: should also work with multiclass
  pred0 = getPred(predict(model,
    newdata = replace(data, list = which(colnames(data) == feature), values = feat0)))
  pred1 = getPred(predict(model,
    newdata = replace(data, list = which(colnames(data) == feature), values = feat1)))
  out = (pred1 - pred0) / (feat1 - feat0)
  return(out)
}
