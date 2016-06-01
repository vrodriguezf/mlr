#' Generate validation data from a tuning result.
#'
#' @description 
#' Generate cleaned validation from a tuning result. The object returned
#' can be used for custom visualization or passed downstream to an out of the
#' box mlr method, \code{\link{plotValidationData}}.
#' @param TuneResult \code{\link{TuneResult}} & \code{\link{OptResult}}\cr
#'  The tuning results, also containing the optimizer results
#' @param include.diagnostics [\code{logical(1)}]\cr
#'  Should diagnostic info (eol and error msg) be included?
#'
#' @return \code{\link{ValidationData}}
#'  Object containing the validation dataframe, the tuning performance measures
#'  used, the hyperparameters used, a flag for including diagnostic info, and 
#'  the optimization algorithm used.
#'
#' @examples
#' \dontrun{
#' res = tuneParams("classif.ksvm", task = pid.task, resampling = rdesc,
#' par.set = ps, control = ctrl)
#' valid = generateValidationData(res)
#' valid$data
#' }
#' @export
generateValidationData = function(TuneResult, include.diagnostics = FALSE) {
  checkmate::assertClass(TuneResult, classes = c("TuneResult", "OptResult"))
  d = as.data.frame(TuneResult$opt.path)
  
  # what if we have numerics that were discretized upstream
  num_hypers = length(TuneResult$opt.path$par.set$pars)
  for (hyp in 1:num_hypers) {
    if (!is.numeric(d[, hyp]))
      d[, hyp] = type.convert(as.character(d[, hyp]))
  }
    
  # off by default unless needed by user
  if (include.diagnostics == FALSE)
    d = within(d, rm(eol, error.message))
  
  # users might not know what dob means, so let's call it iteration
  names(d)[names(d) == "dob"] = "iteration"
  
  makeS3Obj("ValidationData", data = d, measures = TuneResult$opt.path$y.names,
            hyperparams = names(TuneResult$x), 
            diagnostics = include.diagnostics, 
            optimization = getClass1(TuneResult$control))
}

#' @export
print.ValidationData = function(x, ...) {
  catf("ValidationData:")
  catf("Hyperparameters: %s", collapse(x$hyperparams))
  catf("Measures: %s", collapse(x$measures))
  catf("Optimizer: %s", collapse(x$optimization))
  print("Snapshot of data:")
  print(head(x$data))
}

plotValidation = function(ValidationData, x.axis = NULL, y.axis = NULL, 
                          plot.type = "scatter", local.optima = TRUE, 
                          facet = NULL, pretty.names = TRUE, title = NULL) {
  checkmate::assertClass(ValidationData, classes = "ValidationData")
  if (is.null(x.axis) || is.null(y.axis))
    stopf("x.axis and y.axis must both be specified!")
  # TODO later once functionality implemented
  if (length(x.axis) > 1 || length(y.axis) > 1)
    stopf("Greater than 1 length x.axis or y.axis not yet supported")
  if (!is.null(facet))
    stopf("Facet argument is not yet supported")
  
  
  
  d = ValidationData$data
  
  
}