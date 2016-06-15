context("hyperparameterValidation")

test_that("1 numeric hyperparam", {
  # generate data
  ps = makeParamSet(makeDiscreteParam("C", values = 2^(-5:5)))
  ctrl = makeTuneControlRandom(maxit = 50)
  rdesc = makeResampleDesc("CV", iters = 3L)
  res = tuneParams("classif.ksvm", task = pid.task, resampling = rdesc, 
                   par.set = ps, control = ctrl, measures = acc)
  orig = as.data.frame(res$opt.path)
  orig$C = as.numeric(as.character(orig$C))
  new = generateValidationData(res, include.diagnostics = TRUE)
  expect_equivalent(new$data, orig)
  
  # make sure plot is created and can be saved
  plt = plotValidation(new, x.axis = "iteration", y.axis = "acc.test.mean", 
                 plot.type = "line")
  print(plt)
  dir = tempdir()
  path = stri_paste(dir, "/test.svg")
  ggsave(path)
  
  # make sure plot has expected attributes
  expect_set_equal(sapply(plt$layers, function(x) class(x$geom)[1]), 
                   c("GeomPoint", "GeomLine"))
  expect_equal(plt$labels$x, "iteration")
  expect_equal(plt$labels$y, "Accuracy")
  
  # TODO: make sure plot looks as expected
})