context("hyperparameterValidation")

test_that("hyperparameterValidation", {
  # 1 numeric hyperparam
  ps = makeParamSet(makeDiscreteParam("C", values = 2^(-5:5)))
  ctrl = makeTuneControlRandom(maxit = 50)
  rdesc = makeResampleDesc("CV", iters = 3L)
  res = tuneParams("classif.ksvm", task = pid.task, resampling = rdesc, 
                   par.set = ps, control = ctrl, measures = acc)
  orig = as.data.frame(res$opt.path)
  orig$C = as.numeric(as.character(orig$C))
  new = generateValidationData(res, include.diagnostics = TRUE)
  expect_equivalent(new$data, orig)
  
  # plotting
  # TODO: comparing pixel data? Looks like this requires a package?
  plotValidation(new, x.axis = "iteration", y.axis = "acc.test.mean", 
                 plot.type = "line")
  dir = tempdir()
  path = stri_paste(dir, "/test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
})