context("Rlearner_classif_fdauscdepth")

test_that("classif_fdauscdepth behaves like original api", {
  requirePackagesOrSkip("fda.usc", default.method = "load")

  data(phoneme, package = "fda.usc")
  mlearn = phoneme[["learn"]]

  # Use only 10 obs. for 5 classes, as knn training is really slow
  index = c(1:10, 50:60, 100:110, 150:160, 200:210)
  mlearn$data = mlearn$data[index, ]
  glearn = phoneme[["classlearn"]][index]

  mtest = phoneme[["test"]]
  gtest = phoneme[["classtest"]]
  set.seed(getOption("mlr.debug.seed"))
  a1 = fda.usc::classif.depth(glearn, mlearn, par.depth = list("proj" = 5))
  set.seed(getOption("mlr.debug.seed"))
  p1 = fda.usc::classif.depth(glearn, mlearn, mtest, par.depth = list("proj" = 5))


  ph = as.data.frame(mlearn$data)
  ph[, "label"] = glearn

  phtst = as.data.frame(mtest$data)
  phtst[, "label"] = gtest

  lrn = makeLearner("classif.fdauscdepth", par.vals = list(par.depth = list("proj" = 5)))
  fdata = makeFunctionalData(ph, fd.features = NULL, exclude.cols = "label")
  ftest = makeFunctionalData(phtst, fd.features = NULL, exclude.cols = "label")
  task = makeClassifTask(data = fdata, target = "label")
  set.seed(getOption("mlr.debug.seed"))
  m = train(lrn, task)

  set.seed(getOption("mlr.debug.seed"))
  cp = predict(m, newdata = ftest)
  cp = unlist(cp$data$response, use.names = FALSE)

  # check if the output from the original API matches the mlr learner's output
  expect_equal(as.character(a1$group.est), as.character(m$learner.model$group.est))
  expect_equal(as.character(a1$group.pred), as.character(m$learner.model$group.pred))

  expect_equal(as.character(cp), as.character(p1$group.pred))

})

test_that("resampling fdauscdepth", {
  requirePackagesOrSkip("fda.usc", default.method = "load")
  lrn = makeLearner("classif.fdauscdepth", par.vals = list(par.depth = list("proj" = 5)))

  set.seed(getOption("mlr.debug.seed"))
  r = resample(lrn, fda.binary.gp.task.small, cv2)
  expect_class(r, "ResampleResult")
})
