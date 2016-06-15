#' Generate hyperparameter validation data from a tuning result.
#'
#' @description 
#' Generate cleaned hyperparameter validation from a tuning result. The object 
#' returned can be used for custom visualization or passed downstream to an out 
#' of the box mlr method, \code{\link{plotValidation}}.
#' @param TuneResult \code{\link{TuneResult}} & \code{OptResult}\cr
#'  The tuning results, also containing the optimizer results
#' @param include.diagnostics \code{logical(1)}\cr
#'  Should diagnostic info (eol and error msg) be included?
#'
#' @return \code{ValidationData}
#'  object containing the validation dataframe, the tuning performance measures
#'  used, the hyperparameters used, a flag for including diagnostic info, and 
#'  the optimization algorithm used.
#'
#' @examples \dontrun{
#' ps = makeParamSet(makeDiscreteParam("C", values = 2^(-4:4)))
#' ctrl = makeTuneControlRandom()
#' rdesc = makeResampleDesc("CV", iters = 3L)
#' res = tuneParams("classif.ksvm", task = pid.task, resampling = rdesc,
#' par.set = ps, control = ctrl)
#' valid = generateValidationData(res)
#' valid$data
#' }
#' @export
#' @importFrom utils type.convert
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
    d = within(d, rm("eol", "error.message"))
  
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
  catf("Snapshot of $data:")
  print(head(x$data))
}

#' Plot the hyperparameter validation data
#' 
#' @description 
#' Plot hyperparameter validation path. Automated plotting method for 
#' \code{ValidationData} object. Useful for determining the importance
#' or effect of a particular hyperparameter on some performance measure and/or
#' optimizer.
#'
#' @param ValidationData \code{ValidationData}\cr
#' @param x.axis \code{character(1)}\cr
#'  Specify what should be plotted on the x axis. Must be a column from
#'  \code{ValidationData$data}
#' @param y.axis \code{character(1)}\cr
#'  Specify what should be plotted on the y axis. Must be a column from
#'  \code{ValidationData$data}
#' @param extra.axis \code{character(1)}\cr
#'  Specify what should be used as the extra axis for a particular geom. This
#'  could be for the fill on a heatmap or color aesthetic for a line. Must be a 
#'  column from \code{ValidationData$data}
#' @param plot.type \code{character(1)}\cr
#'  Specify the type of plot: scatter, heatmap, or line
#' @param loess.smooth \code{logical(1)}\cr
#'  If TRUE, will add loess smoothing line to plots where possible
#' @param facet \code{character(1)}\cr
#'  TODO: not yet implemented
#' @param pretty.names \code{logical(1)}\cr
#'  Whether to use the \code{\link{Measure}} name instead of the id in the plot
#' @param title \code{character(1)}\cr
#'  A custom title for the returned plot
#'
#' @return
#'  ggplot2 plot object
#' @export
#'
#' @examples \dontrun{
#' ps = makeParamSet(makeDiscreteParam("C", values = 2^(-4:4)))
#' ctrl = makeTuneControlRandom()
#' rdesc = makeResampleDesc("CV", iters = 3L)
#' res = tuneParams("classif.ksvm", task = pid.task, resampling = rdesc,
#' par.set = ps, control = ctrl, measures = acc)
#' valid = generateValidationData(res)
#' plotValidation(valid, x.axis = "C", y.axis = "acc.test.mean")
#' }
plotValidation = function(ValidationData, x.axis = NULL, y.axis = NULL, 
                          extra.axis = NULL, plot.type = NULL, 
                          loess.smooth = FALSE, facet = NULL, 
                          pretty.names = TRUE, title = NULL) {
  checkmate::assertClass(ValidationData, classes = "ValidationData")
  if (is.null(x.axis) || is.null(y.axis))
    stopf("x.axis and y.axis must both be specified!")
  if (is.null(plot.type))
    stopf("plot type must be one of: scatter, line, heatmap")
  # TODO later once functionality implemented
  if (!is.null(facet))
    stopf("Facet argument is not yet supported")
  if (length(x.axis) > 1 || length(y.axis) > 1 || length(extra.axis) > 1)
    stopf("Greater than 1 length x.axis, y.axis or fill not yet supported")
  
  d = ValidationData$data
  d_names = names(d)
  if (!(x.axis %in% d_names) || !(y.axis %in% d_names))
    stopf("x.axis, y.axis and fill must match column names in the data!")
  
  # x, y, "third" axis
  if ((length(x.axis) == 1) && (length(y.axis) == 1) && (!is.null(extra.axis))  
      && (!is.null(plot.type))){
    if (plot.type == "heatmap"){
      # ensure we get a "tabled" heatmap
      d[, x.axis] = as.factor(d[, x.axis])
      d[, y.axis] = as.factor(d[, y.axis])
      d[, extra.axis] = round(d[, extra.axis], 3)
      plt = ggplot(d, aes_string(x = x.axis, y = y.axis, fill = extra.axis)) + 
        geom_tile() + geom_text(aes_string(label = extra.axis), color = "white")
    } else if (plot.type == "line"){
      plt = ggplot(d, aes_string(x = x.axis, y = y.axis)) + geom_point()
      plt = plt + geom_line(aes_string(color = extra.axis))
    } else if (plot.type == "scatter"){
      plt = ggplot(d, aes_string(x = x.axis, y = y.axis)) + 
        geom_point(aes_string(color = extra.axis))
    }
  # just x, y  
  } else if ((length(x.axis) == 1) && (length(y.axis) == 1) && 
             (is.null(extra.axis)) && (!is.null(plot.type))){
    if (plot.type == "scatter")
      plt = ggplot(d, aes_string(x = x.axis, y = y.axis)) + geom_point()
    else if (plot.type == "line")
      plt = ggplot(d, aes_string(x = x.axis, y = y.axis)) + geom_point() +
        geom_line()
      if (loess.smooth)
        plt = plt + geom_smooth()
  }
  
  # pretty name changing
  if (pretty.names) {
    if (x.axis %in% ValidationData$measures)
      plt = plt + 
        xlab(eval(as.name(stri_split_fixed(x.axis, ".test.mean")[[1]][1]))$name)
    if (y.axis %in% ValidationData$measures)
      plt = plt + 
        ylab(eval(as.name(stri_split_fixed(y.axis, ".test.mean")[[1]][1]))$name)
    if (!is.null(extra.axis))
      if (extra.axis %in% ValidationData$measures)
        plt = plt +
          labs(fill = eval(as.name(stri_split_fixed(extra.axis, 
                                                ".test.mean")[[1]][1]))$name) 
  }
  
  if (!is.null(title))
    plt = plt + ggtitle(title)
  
  return(plt)
}