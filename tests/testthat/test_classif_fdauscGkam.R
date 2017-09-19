context("RLearner_classif_fdauscgkam")

test_that("classif_fdauscgkam behaves like original api", {
  requirePackagesOrSkip("fda.usc", default.method = "load")
  
  data(phoneme, package = "fda.usc")
  # Use only 5 obs. for all classes, as gkam  is really slow
  index = c(1:5, 50:55, 100:105, 150:155, 200:205)
  mlearn = phoneme[["learn"]]
  mlearn$data = mlearn$data[index, ]
  glearn = as.numeric(phoneme[["classlearn"]][index])
  mtest = phoneme[["test"]]
  gtest = as.numeric(phoneme[["classtest"]])
  
  dataf = data.frame(glearn)
  dat = list("df" = dataf, "x" = mlearn)
  
  # smoothing with GAM formulae smoothing splines
  a1 = suppressWarnings(fda.usc::classif.gkam(glearn ~ x, data = dat))
  # this fixes bug in fda.usc. The C parameter needs to contain the classifier name
  a1$C[[1]] = quote(classif.gkam)
  # this strange code is necessary for fda.usc to predict from a gsam classifier
  p1 = predict(a1, list("x" = mtest))
  p2 = predict(a1, list("x" = mlearn))
  
  
  ph = as.data.frame(mlearn$data)
  ph[, "label"] = phoneme[["classlearn"]][index]
  phtst = as.data.frame(mtest$data)
  phtst[, "label"] = phoneme[["classtest"]]
  
  lrn = makeLearner("classif.fdauscgkam")
  fdata = makeFunctionalData(ph, fd.features = NULL, exclude.cols = "label")
  ftest = makeFunctionalData(phtst, fd.features = NULL, exclude.cols = "label")
  task = makeClassifTask(data = fdata, target = "label")
  
  set.seed(getOption("mlr.debug.seed"))
  # glm sometimes does not converge, we dont want to see that
  m = suppressWarnings(train(lrn, task))
  cp = predict(m, newdata = ftest)
  cp = unlist(cp$data$response, use.names = FALSE)
  
  cp2 = predict(m, newdata = fdata)
  cp2 = unlist(cp2$data$response, use.names = FALSE)
  # check if the output from the original API matches the mlr learner's output
  expect_equal(as.character(cp2), as.character(p2))
  expect_equal(as.character(cp), as.character(p1))
})
