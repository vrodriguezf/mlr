#' @title Plot resampling objects.
#'
#' @description Visualize distribution of observations in (spatial) cross-validation.
#' @import ggplot2
#' @importFrom cowplot plot_grid save_plot
#' @import sf
#' @importFrom purrr map imap is_list flatten map_int
#' @import hrbrthemes
#' @family plot
#' @author Patrick Schratz
#' @param task [Task()] \cr
#'   Task object.
#' @param resample [ResampleResult()]\cr
#'   As returned by [resample()].
#' @param crs [integer()]\cr
#'   Specify a custom coordinate reference system.
#' @param repetitions [integer()]\cr
#'   How many repetitions should be visualized?
#' @examples
#'
#' data(spatial.task, package = "mlr", envir = environment())
#'
#' lrn.ksvm = makeLearner("classif.ksvm",
#'                        predict.type = "prob",
#'                        kernel = "rbfdot")
#'
#' ps = makeParamSet(makeNumericParam("C", lower = 1, upper = 1),
#'                   makeNumericParam("sigma", lower = 1, upper = 1))
#'
#' ctrl = makeTuneControlRandom(maxit = 1)
#' inner = makeResampleDesc("SpCV", iters = 2)
#'
#' wrapper.ksvm = makeTuneWrapper(lrn.ksvm, resampling = inner, par.set = ps,
#'                                control = ctrl, show.info = FALSE, measures = list(auc))
#'
#' outer = makeResampleDesc("SpRepCV", folds = 5, reps = 4)
#'
#' set.seed(12)
#' out = resample(wrapper.ksvm, spatial.task,
#'                resampling = outer, show.info = TRUE, measures = list(auc))
#'
#' plotPartitions(spatial.task, list(out, out), 32630, repetitions = 1, filename = "~/Downloads/cowplot.png")
#' @export
plotPartitions <- function (task = NULL, resample = NULL, crs = 4326,
                            repetitions = 1, filename = NULL) {

  # in case one suppliesonly one resample object
  if (!class(resample)[1] == "list") {
    resample = list(resample)
  }
  # how many resamp objects do we have?
  n.resamp = length(resample)

  # create plot list with length = folds
  nfolds = map_int(resample, ~ .x$pred$instance$desc$folds)[1]

  plot_list = map(resample, function(.r) {

    # bind coordinates to data
    data <- cbind(task$env$data, task$coordinates)

    # create 'sf' object
    data <- st_as_sf(data, coords = names(task$coordinates), crs = crs)
    # add ID column for subsetting of indices later
    #data$id = seq(1:nrow(data))

    # create plot list with length = folds
    plot_list = map(1:(nfolds * repetitions), ~ data)

    plot_list = imap(plot_list, ~ ggplot(.x) +
                       geom_sf(data = subset(.x, as.integer(rownames(.x)) %in%
                                               .r$pred$instance[["train.inds"]][[.y]]),
                               color = "#440154", size = 0.5, ) +
                       geom_sf(data = subset(.x,as.integer(rownames(.x)) %in%
                                               .r$pred$instance[["test.inds"]][[.y]]),
                               color = "#FDE725", size = 0.5, ) +
                       theme_ipsum_rc() +
                       theme(axis.text.x = element_text(size = 14),
                             axis.text.y = element_text(size = 14),
                             plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"))
    )
  })

  # if more than one element was supplied, flatten the resulting list
  if (n.resamp > 1) {
    plot_list = flatten(plot_list)
  }

  nrow = repetitions

  # account for nfolds
  if (nfolds > 5) {
    nrow = nrow + 1
    ncol = 5
  } else {
    ncol = nfolds
  }


  if (repetitions > 1) {
    labels = c(length = nfolds * repetitions)
    nfolds_reps = rep(seq_along(1:nfolds), repetitions)
    reps_nfolds = c()
    for (i in seq_along(1:repetitions)) {
      reps_nfolds = c(reps_nfolds, rep(i, nfolds))
    }
    labels = rep(sprintf("Fold %s (Rep %s)", nfolds_reps, reps_nfolds),
                 repetitions * nfolds)
    # account for multiple resamp objects
    if (n.resamp > 1) {
      labels = rep(labels, n.resamp)
    }
  } else {
    labels = sprintf("Fold %s", seq_along(1:nfolds))
    # account for multiple resamp objects
    if (n.resamp > 1) {
      labels = rep(labels, n.resamp)
    }
  }

  # are multiple tasks
  if (n.resamp > 1) {
    nrow = nrow * n.resamp
  }

  # create gridded plot
  grids <- plot_grid(plotlist = plot_list, nrow = nrow, ncol = ncol,
                     hjust= - 0.2, vjust = 2,
                     labels = labels)

  # optionally save file
  if (!is.null(filename)) {
    save_plot(filename, grids, nrow = nrow, ncol = ncol)
  } else {
    return(grids)
  }
}
