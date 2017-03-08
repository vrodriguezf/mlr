#' @export
#' @rdname Task
makeAnomalyDetectionTask = function(id = deparse(substitute(data)), data, target, 
  weights = NULL, blocking = NULL, 
  positive = NA_character_, fixup.data = "warn", 
  check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  #% assertString(target)
  # some code on cran passed stuff like positive=1, we can live with the convert here
  
  ###% Delete, only need in binary classification
  #% if (isScalarNumeric(positive))
  #%  positive = as.character(positive)
  assertString(positive, na.ok = TRUE)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)
  
  
  ###% delete if-fct about "fixedup.data" , as there is no target variable in unsupervised
  #% if (fixup.data != "no") {
  #%   x = data[[target]]
  #%   if (is.character(x) || is.logical(x) || is.integer(x)) {
  #%     data[[target]] = as.factor(x)
  #%   } else if (is.factor(x) && fixup.data == "warn" && any(table(x) == 0L)) {
  #%    warningf("Target column '%s' contains empty factor levels", target)
  #%    data[[target]] = droplevels(x)
  #%  }
  #% }
  
  ###% are weights possible for one class? I think yes
  ###% delete "target" as input variable
  task = mlr:::makeUnsupervisedTask("anomalydetection", data, weights, blocking, 
    fixup.data = fixup.data, check.data = check.data)
  
  ###% delete if-fct, , as there is no target variable in unsupervised
  #% if (check.data) {
  #%   assertFactor(data[[target]], any.missing = FALSE, empty.levels.ok = FALSE, .var.name = target)
  #% }
  addClasses(task, "AnomalyDetectionTask") 
  task$task.desc = makeTaskDesc.AnomalyDetectionTask(task, id)
 
}

makeTaskDesc.AnomalyDetectionTask = function(task, id) {
  
  ###% no target variable in one class classification, set to character(OL)
  target = character(0L)
  
  ##% no target in one class, therfore no levels 
  #% levs = levels(task$env$data[[target]])
  #% m = length(levs)
  
  ###%  positive variable only for binary classification
  #% if (is.na(positive)) {
  #%   if (m <= 2L)
  #%     positive = levs[1L]
  #% } else {
  #%   if (m > 2L)
  #%     stop("Cannot set a positive class for a multiclass problem!")
  #%   assertChoice(positive, choices = levs)
  #% }
  
  td = mlr:::makeTaskDescInternal(task, "anomalydetection", id, target)
  
  ##% no addtional description
  #% td$class.levels = levs
  #% td$positive = positive
  #% td$negative = NA_character_
  #% if (length(td$class.levels) == 1L)
  #%   td$negative = stri_paste("not_", positive)
  #% else if (length(td$class.levels) == 2L)
  #%   td$negative = setdiff(td$class.levels, positive)
  
  return(addClasses(td, c("TaskDescAnomalyDetection", "TaskDescUnsupervised")))
}

#' @export
print.AnomalieDetectionTask = function(x, ...) {
  mlr:::print.UnsupervisedTask(x)
}
