makeImpactFeatureWrapper = function(learner, fun = mean, cols = NULL) {
  learner = checkLearner(learner)
  args = list(fun = fun, cols = cols)
  rm(list = names(args))

  trainfun = function(data, target, args) {
    }
  }

}
