#' @title Plot resampling objects.
#'
#' @description
#  Visualize distribution of (spatial) partitioning in cross-validation.
#' @import ggplot2
#' @importFrom cowplot plot_grid save_plot
#' @import sf
#' @importFrom purrr map imap
#' @import hrbrthemes
#' @family plot
#' @author Patrick Schratz
#' @param task `Task`\cr
#'   Task object
#' @param resample `ResampleResult`\cr
#'   As returned by `resample`.
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
#' plot_partitions(spatial.task, out, 32630, repetitions = 4, filename = "~/Downloads/cowplot.png")
#' @export
plotPartitions <- function (task = NULL, resample = NULL, crs = NULL,
                             repetitions = NULL, filename = NULL) {

  # bind coordinates to data
  data <- cbind(task$env$data, task$coordinates)

  # create 'sf' object
  data <- st_as_sf(data, coords = names(task$coordinates), crs = crs)
  # add ID column for subsetting of indices later
  #data$id = seq(1:nrow(data))

  # create plot list with length = folds
  nfolds = resample$pred$instance$desc$folds
  plot_list = map(1:(nfolds*repetitions), ~ data)

  plot_list <- imap(plot_list, ~ ggplot(.x) +
                      geom_sf(data = subset(.x, as.integer(rownames(data)) %in%
                                              resample$pred$instance[["train.inds"]][[.y]]),
                              color = "#440154", size = 0.5, ) +
                      geom_sf(data = subset(.x, as.integer(rownames(data)) %in%
                                              resample$pred$instance[["test.inds"]][[.y]]),
                              color = "#FDE725", size = 0.5, ) +
                      theme_ipsum_rc() +
                      theme(axis.text.x=element_text(size = 14),
                            axis.text.y=element_text(size = 14),
                            plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"))
  )

  if (!is.null(repetitions)) {
    nrow = repetitions
  } else {
    nrow = 1
  }

  # account for nfolds
  if (nfolds > 5) {
    nrow = nrow + 1
    ncol = 5
  } else {
    ncol = nfolds
  }

  if (is.null(repetitions)) {
    repetitions = 1
  } else {
    repetitions = repetitions
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
  } else {
    labels = sprintf("Fold %s", seq_along(1:nfolds))
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
