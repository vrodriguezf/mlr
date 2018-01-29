test_that("test plotSpatialResampling() works with various resample inputs and reps and 5 folds", {

  data(spatial.task, package = "mlr", envir = environment())

  lrn.ksvm = makeLearner("classif.ksvm",
                         predict.type = "prob",
                         kernel = "rbfdot")

  ps = makeParamSet(makeNumericParam("C", lower = 1, upper = 1),
                    makeNumericParam("sigma", lower = 1, upper = 1))

  ctrl = makeTuneControlRandom(maxit = 1)
  inner = makeResampleDesc("SpCV", iters = 2)

  wrapper.ksvm = makeTuneWrapper(lrn.ksvm, resampling = inner, par.set = ps,
                                 control = ctrl, show.info = FALSE, measures = list(auc))

  outer = makeResampleDesc("SpRepCV", folds = 5, reps = 2)

  set.seed(12)
  out = resample(wrapper.ksvm, spatial.task,
                 resampling = outer, show.info = TRUE,
                 measures = list(auc))

  # single unnamed input, nfold = 5, rep = 1
  plotSpatialResampling(spatial.task, out, 32630, repetitions = 1,
                 filename = "~/Downloads/cowplot.png")

  # single named input, nfold = 5, rep = 1
  plotSpatialResampling(spatial.task, list("test" = out), 32630,
                 repetitions = 1,
                 filename = "~/Downloads/cowplot.png")

  # single unnamed input, nfold = 5, rep = 2
  plotSpatialResampling(spatial.task, out, 32630, repetitions = 2,
                 filename = "~/Downloads/cowplot.png")

  # single named input, nfold = 5, rep = 2
  plotSpatialResampling(spatial.task, list("test" = out), 32630,
                 repetitions = 2,
                 filename = "~/Downloads/cowplot.png")

  # multiple inputs, nfold = 5, rep = 1
  plotSpatialResampling(spatial.task, list("Spcv1" = out, "Spcv2" = out),
                 32630, repetitions = 1,
                 filename = "~/Downloads/cowplot.png")

  # multiple inputs, nfold = 5, rep = 2
  plotSpatialResampling(spatial.task, list("Spcv1" = out, "Spcv2" = out),
                 32630, repetitions = 2,
                 filename = "~/Downloads/cowplot.png")

  # unnamed lists are not allowed
  expect_error(plotSpatialResampling(spatial.task, list(out, out)))
})

test_that("test plotSpatialResampling() works with various resample inputs and reps and > 5 folds", {

  data(spatial.task, package = "mlr", envir = environment())

  lrn.ksvm = makeLearner("classif.ksvm",
                         predict.type = "prob",
                         kernel = "rbfdot")

  ps = makeParamSet(makeNumericParam("C", lower = 1, upper = 1),
                    makeNumericParam("sigma", lower = 1, upper = 1))

  ctrl = makeTuneControlRandom(maxit = 1)
  inner = makeResampleDesc("SpCV", iters = 2)

  wrapper.ksvm = makeTuneWrapper(lrn.ksvm, resampling = inner, par.set = ps,
                                 control = ctrl, show.info = FALSE, measures = list(auc))

  outer = makeResampleDesc("SpRepCV", folds = 7, reps = 2)

  set.seed(12)
  out = resample(wrapper.ksvm, spatial.task,
                 resampling = outer, show.info = TRUE, measures = list(auc))

  # single input, nfold = 7, rep = 1
  plotSpatialResampling(spatial.task, out, 32630, repetitions = 1,
                 filename = "~/Downloads/cowplot.png")

  # single input, nfold = 7, rep = 2
  plotSpatialResampling(spatial.task, out, 32630, repetitions = 2,
                 filename = "~/Downloads/cowplot.png")

  # multiple input, nfold = 7, rep = 1
  plotSpatialResampling(spatial.task, list("Spcv1" = out, "Spcv2" = out),
                 32630, repetitions = 1,
                 filename = "~/Downloads/cowplot.png")

  # multiple input, nfold = 7, rep = 2
  plotSpatialResampling(spatial.task, list("Spcv1" = out, "Spcv2" = out),
                 32630, repetitions = 2,
                 filename = "~/Downloads/cowplot.png")

  # unnamed lists are not allowed
  expect_error(plotSpatialResampling(spatial.task, list(out, out)))

})

