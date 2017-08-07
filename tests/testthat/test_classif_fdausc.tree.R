context("Rlearner_classif_fdausc.tree")

test_that("classif_fdausc.tree behaves like original api", {
  requirePackagesOrSkip("fda.usc", default.method = "load")

  data(phoneme, package = "fda.usc")
  mlearn = phoneme[["learn"]]
  # Use only 10 obs. for 5 classes, as knn training is really slow
  index = c(1:10, 50:60, 100:110, 150:160, 200:210)
  mlearn$data = mlearn$data[index, ]
  glearn = phoneme[["classlearn"]][index]

  mtest = phoneme[["test"]]
  gtest = phoneme[["classtest"]]

  dataf = data.frame(glearn)
  dat = list("df" = dataf, "x" = mlearn)

  a1 = fda.usc::classif.tree(glearn ~ x, data = dat)
  # this fixes bug in fda.usc. The C parameter needs to contain the classifier name
  a1$C[[1]] = quote(classif.tree)
  # this strange code is necessary for fda.usc to predict from a tree classifier
  p1 = predict(a1, list("x" = mtest))
  p2 = predict(a1, list("x" = mlearn))


  ph = as.data.frame(mlearn$data)
  ph[, "label"] = glearn
  phtst = as.data.frame(mtest$data)
  phtst[, "label"] = gtest

  lrn = makeLearner("classif.fdausc.tree")
  fdata = makeFunctionalData(ph, fd.features = NULL, exclude.cols = "label")
  ftest = makeFunctionalData(phtst, fd.features = NULL, exclude.cols = "label")
  task = makeClassifTask(data = fdata, target = "label")
  m = train(lrn, task)

  cp1 = predict(m, newdata = ftest)
  cp1 = unlist(cp1$data$response, use.names = FALSE)

  cp2 = predict(m, newdata = fdata)
  cp2 = unlist(cp2$data$response, use.names = FALSE)

  # check if the output from the original API matches the mlr learner's output
  expect_equal(as.character(cp2), as.character(p2))
  expect_equal(as.character(cp1), as.character(p1))

})

test_that("resampling fdausc.tree", {
  requirePackagesOrSkip("fda.usc", default.method = "load")
  lrn = makeLearner("classif.fdausc.tree")

  set.seed(getOption("mlr.debug.seed"))
  r = resample(lrn, fda.binary.gp.task.small, cv2)
  expect_class(r, "ResampleResult")
})
