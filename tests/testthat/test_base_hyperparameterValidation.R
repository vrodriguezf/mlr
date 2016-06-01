context("hyperparameterValidation")

test_that("hyperparameterValidation", {
  # 1 numeric hyperparam
  ps = makeParamSet(makeDiscreteParam("C", values = 2^(-4:4)))
  ctrl = makeTuneControlGrid()
  rdesc = makeResampleDesc("CV", iters = 3L)
  res = tuneParams("classif.ksvm", task = pid.task, resampling = rdesc, 
                   par.set = ps, control = ctrl)
  orig = as.data.frame(res$opt.path)
  orig$C = as.numeric(as.character(orig$C))
  new = generateValidationData(res, include.diagnostics = TRUE)
  expect_equivalent(new$data, orig)
})