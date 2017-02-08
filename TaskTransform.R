# fun1: task --> task + data
# fun2: task --> task


#' @title Constructor for TaskTransform
#'
#' @param [\code\link{function()}]\cr
#'   Function to transfrom a task.
#' @param [\code\link{function()}]\cr
#'   Function to transfrom a task.
#' @param [\code\link{ParamSet}]\cr
#   Description of control parameters for TaskTransform.
#' @return [\code{TaskTransform}].
#'
makeTaskTransform = function(fun1, fun2, par.set) {
  assertFunction(fun1, c("task"))
  assertFunction(fun2, c("task", "retrafo"))
  assertClass(par.set, "ParamSet")
  makeS3Obj("TaskTransform",
    fun1 = fun1,
    fun2 = fun2,
    par.set = par.set
  )
}

#' @title Transform task with a TaskTransform.
#'
#' @param [\code\link{TaskTransform}]\cr
#'   Function to transfrom a task.
#' @param [\code\link{Task}]\cr
#'   Function to transform a task.
#' @param [\code{list}]\cr
#    Control parameters for TaskTransform.
#' @return [\code{TaskTransform}].
#'
transformTask = function(tt, task, par.vals = list()) {
  assertClass(tt, "TaskTransform")
  assertClass(task, "Task")
  arglist = list(task = task)
  arglist = insert(arglist, par.vals)
  do.call(tt$fun1, arglist)
}

#' @title Transform task with a TaskTransform.
#'
#' @param [\code\link{TaskTransform}]\cr
#'   Function to transfrom a task.
#' @param [\code\link{Task}]\cr
#'   Function to transform a task.
#' @param [\code{list}]\cr
#    Control parameters for TaskTransform.
#' @return [\code{TaskTransform}].
#'
retransform = function(tt, task, retrafo) {
  tt$fun2(task, retrafo)
}

join2TaskTransforms = function(tt1, tt2) {
  assertClass(tt1, "TaskTransform")
  assertClass(tt2, "TaskTransform")
  fun1 = function(task, ...) {
    z1 = tt1$fun1(task, ...)
    z2 = tt2$fun1(z1$task, ...)
    retrafo = c(z1$retrafo, z2$retrafo)
    list(task = z2$task, retrafo = retrafo)
  }
  fun2 = function(task, retrafo, ...) {
    tt2$fun2(tt1$fun2(task, retrafo, ...), retrafo, ...)
  }
  ps = c(tt1$par.set, tt2$par.set)
  makeTaskTransform(fun1, fun2, ps)
}


joinTaskTransforms = function(..., trafos = list()) {
  # merge list and dotargs
  arglist = list(...)
  trafos = c(trafos, arglist)
  assertList(trafos, "TaskTransform", min.len = 1L)
  # only 1 trafo? we are done
  if (length(trafos) == 1L)
    return(trafos[[1L]])
  # recursion to join all with binary join
  join2TaskTransforms(trafos[[1L]], joinTaskTransforms(trafos = trafos[-1]))
}
