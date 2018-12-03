context("RLearner_cluster_fda_hmm")

test_that("cluster.fda.hmm works with the smooth.discrete example of the mhsmm vignette", {
  requirePackagesOrSkip("mhsmm", default.method = "load")

  # Extracted from the mhsmm vignete "Smoothing discrete sequences, part 1"
  y1 <- as.integer(c(1,1,1,1,2,1,1,NA,1,1,2,1,1,1,2,1,1,1,1,1,2,2,2,2,1,2,2,2,1,2,2,2,1,1,1,1,1,1,1,1,2,2,2,1,1))
  mod.compare.mhsmm = mhsmm::smooth.
  discrete(na.omit(y1))

  y1.fdf = makeFunctionalData(
    data = data.frame(t(data.frame(na.omit(y1)))),
    fd.features = list(y1 = 1:length(na.omit(y1)))
  )
  y1.task = makeClusterTask("test_mhsmm_smooth_discrete_in_mlr", y1.fdf)
  mod.compare.mlr = train(
    learner = makeLearner("cluster.fda.hmm", J = mod.compare.mhsmm$model$model$J,
      init = mod.compare.mhsmm$initial$init, trans = mod.compare.mhsmm$initial$trans,
      family.emission = "multinomial", parms.emission = mod.compare.mhsmm$initial$parms.emission),
    task = y1.task
  )

  expect_equal(mod.compare.mhsmm$model$model$init, mod.compare.mlr$learner.model$model$init, tolerance = 1e-4)
  expect_equal(mod.compare.mhsmm$model$model$trans, mod.compare.mlr$learner.model$model$trans, tolerance = 1e-4)
  # The mlr implementation of .dmpf and .mstep.pmf puts the states in the columns instead of
  # the rows. Thus, the parms.emission matrix must be transposed for the comparison
  expect_equal(t(mod.compare.mhsmm$model$model$parms.emission$pmf), mod.compare.mlr$learner.model$model$parms.emission$pmf, tolerance = 1e-4)
})
