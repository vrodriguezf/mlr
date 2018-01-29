#' @title Plot (spatial) resampling objects.
#'
#' @description Visualize partitioning of resample objects with spatial information.
#' @import ggplot2
#' @importFrom cowplot plot_grid save_plot
#' @import sf
#' @importFrom purrr map imap is_list flatten map_int
#' @import hrbrthemes
#' @family plot
#' @author Patrick Schratz
#' @param task [Task] \cr
#'   Task object.
#' @param resample [ResampleResult] or named `list` with (multiple) [ResampleResult]\cr
#'   As returned by [resample].
#' @param crs [integer]\cr
#'   Coordinate reference system (EPSG code number) for the supplied coordinates in the `Task`.
#' @param repetitions [integer]\cr
#'   Number of repetitions.
#' @param color.train Color for train set.
#' @param color.test Color for test set.
#' @param point.size Point size.
#' @param axis.text.size Font size of axis labels.
#'
#' @return [`plot_grid`] object.
#'
#' @details
#' If a named list is given to `resample`, names will appear in the title of each fold.
#' If multiple inputs are given to `resample`, these must be named.
#'
#' This function makes a hard cut at five columns of the resulting grid.
#' This means if the `resample` object consists of `folds > 5`, these folds will be put into the new row.
#'
#' For file saving, [`save_plot`] is used.
#' If you want to save the returned object manually, make sure to set `ncol` and `nrow` accordingly in [`save_plot`].
#'
#' @section CRS:
#'
#' The crs has to be suitable for the coordinates stored in the `Task`.
#' For example, if the coordinates are UTM, `crs` should be set to a UTM projection.
#' Due to a limited axis space in the resulting grid, the data will always be projected into a lat/lon projection, specifically EPSG 4326.
#' If projection (`crs`) and coordinates do not match, axis labels will be empty.
#'
#' @md
#' @examples
#'
#' rdesc = makeResampleDesc("SpRepCV", folds = 5, reps = 4)
#' r = resample(makeLearner("classif.qda"), spatial.task, rdesc)
#'
#' ##------------------------------------------------------------
#' ## single unnamed resample input with 5 folds and 2 repetitions
#' ##------------------------------------------------------------
#'
#' plotSpatialResampling(spatial.task, r, crs = 32717, repetitions = 2)
#'
#' ##------------------------------------------------------------
#' ## single named resample input with 5 folds and 1 repetition
#' ##------------------------------------------------------------
#'
#' plotSpatialResampling(spatial.task, list("Resamp" = r), crs = 32717, repetitions = 1)
#'
#' ##------------------------------------------------------------
#' ## multiple named resample inputs with 5 folds and 2 repetitions
#' ##------------------------------------------------------------
#'
#' rdesc1 = makeResampleDesc("SpRepCV", folds = 5, reps = 4)
#' r1 = resample(makeLearner("classif.qda"), spatial.task, rdesc1)
#' rdesc2 = makeResampleDesc("RepCV", folds = 5, reps = 4)
#' r2 = resample(makeLearner("classif.qda"), spatial.task, rdesc2)
#'
#' plotSpatialResampling(spatial.task, list("SpRepCV" = r1, "RepCV" = r2),
#'   crs = 32717, repetitions = 1)
#'
#' @export
plotSpatialResampling = function(task = NULL, resample = NULL, crs = NULL,
  repetitions = 1, filename = NULL, color.train = "#440154",
  color.test = "#FDE725", point.size = 0.5, axis.text.size = 14) {

  if (is.null(crs))
    stopf("Please specify a crs that matches the coordinates of the Task.")

  # in case one suppliesonly one resample object
  if (!class(resample)[1] == "list") {
    resample = list(resample)
  }
  # how many resamp objects do we have?
  n.resamp = length(resample)

  if (n.resamp > 1 && is.null(names(resample))) {
    stopf("Please name multiple resample inputs.")
  }

  # create plot list with length = folds
  nfolds = map_int(resample, ~ .x$pred$instance$desc$folds)[1]

  plot.list = map(resample, function(.r) {

    # bind coordinates to data
    data = cbind(task$env$data, task$coordinates)

    # create 'sf' object
    data = st_as_sf(data, coords = names(task$coordinates), crs = crs)

    # create plot list with length = folds
    plot.list = map(1:(nfolds * repetitions), ~ data)

    plot.list = imap(plot.list, ~ ggplot(.x) +
                       geom_sf(data = subset(.x, as.integer(rownames(.x)) %in%
                                               .r$pred$instance[["train.inds"]][[.y]]),
                               color = color.train, size = point.size, ) +
                       geom_sf(data = subset(.x,as.integer(rownames(.x)) %in%
                                               .r$pred$instance[["test.inds"]][[.y]]),
                               color = color.train, size = point.size) +
                       theme_ipsum_rc() +
                       theme(axis.text.x = element_text(size = axis.text.size),
                             axis.text.y = element_text(size = axis.text.size),
                             plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"))
    )
  })

  plot.list = flatten(plot.list)

  nrow = repetitions

  # account for nfolds: Hard restrict on more than 5 columns in plot grid
  if (nfolds > 5) {
    nrow = nrow + 1
    ncol = 5
  } else {
    ncol = nfolds
  }

  # more than 1 repetition?
  if (repetitions > 1) {
    labels = c(length = nfolds * repetitions)
    nfolds_reps = rep(seq_along(1:nfolds), repetitions)
    reps_nfolds = c()
    names.resample = c()
    for (i in seq_along(1:repetitions)) {
      reps_nfolds = c(reps_nfolds, rep(i, nfolds))
      if (!is.null(names(resample))) {
        names.resample = c(names.resample, rep(names(resample)[i], nfolds * repetitions))
      }
    }
    # account for multiple resamp objects
    if (n.resamp > 1) {
      labels = rep(rep(sprintf("[%s] Fold %s (Rep %s)", names.resample,
                               nfolds_reps, reps_nfolds)), n.resamp)
    } else {
      if (!is.null(names(resample))) {
        labels = sprintf("[%s] Fold %s (Rep %s)",
                         rep(names(resample), nfolds * repetitions),
                         seq_along(1:nfolds), reps_nfolds)
      } else {
        labels = rep(sprintf("Fold %s (Rep %s)", nfolds_reps, reps_nfolds))
      }
    }
  } else {
    # account for multiple resamp objects
    if (n.resamp > 1) {
      names.resample = c()
      for (i in seq_along(1:length(names(resample)))) {
        names.resample = c(names.resample, rep(names(resample)[i], nfolds))
      }
      labels = sprintf("[%s] Fold %s", names.resample, seq_along(1:nfolds))
      labels = rep(labels, n.resamp)
    } else {
      if (!is.null(names(resample))) {
        labels = sprintf("[%s] Fold %s", rep(names(resample), nfolds), seq_along(1:nfolds))
      } else {
        labels = sprintf("Fold %s", seq_along(1:nfolds))
      }
    }
  }

  # multiple resample inputs?
  if (n.resamp > 1) {
    nrow = nrow * n.resamp
  }

  # create gridded plot
  grids = plot_grid(plotlist = plot.list, nrow = nrow, ncol = ncol,
                     hjust= - 0.2, vjust = 2,
                     labels = labels)

  # optionally save file to disk
  if (!is.null(filename)) {
    save_plot(filename, grids, nrow = nrow, ncol = ncol)
    return(invisible(grids))
  } else {
    return(grids)
  }
}
