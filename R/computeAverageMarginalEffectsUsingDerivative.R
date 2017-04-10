computeAverageMarginalEffectsUsingDerivative = function(model, task,
  features = getTaskFeatureNames(task), gridsize = 10L) {

  data = getTaskData(task)
  tt = getTaskType(task)
  target = if(tt == "classif") "Probability" else getTaskTargetNames(task)
  res = namedList(features)
  pdat = generatePartialDependenceData(model, task, features = features,
    gridsize = gridsize, unique.values = TRUE, derivative = TRUE, method = "simple")$data

  # calculate AMEs for all features
  for (f in features) {
    x.vals = data[[f]]
    pdat.feat = pdat[!is.na(pdat[[f]]), c(target, f)]
    ind = match(x.vals, pdat.feat[[f]])
    y = pdat.feat[[target]][ind]

    # FIXME: remvoe preds here
    r = namedList(c("type", "values", "preds", "effects", "reflev"))
    r$preds = y

    if (is.factor(data[, f])) { # handle factors
      # FIXME: user wants to select reflev from outside
      reflev = levels(x.vals)[1L]
      r$type = "factor"
      r$values = as.character(x.vals)
      r$reflev = as.character(reflev)
      r$effects = y - y[reflev]
      r$effects[reflev] = NA_real_
    }

    if (is.numeric(data[, f])) { # handle numeric features
      r$type = "numeric"
      r$values = x.vals
      # FIXME: the ame effect should be interpretable as what happens if x increases by one,
      #        i.e. using only mean(diff(y)) can be wrong as this does not take into account the "grid-step-size" of the x-values
      #weights = table(data[,f])
      r$effects = mean(y)
    }
    res[[f]] = r
  }
  res = addClasses(res, "AverageMarginalEffects")
  return(res)
}
