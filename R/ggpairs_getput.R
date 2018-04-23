#' Put Plot
#'
#' Function to place your own plot in the layout.
#'
#' @param pm ggally object to be altered
#' @param value ggplot object to be placed
#' @param i row from the top
#' @param j column from the left
#' @keywords hplot
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @export
#' @examples
#' custom_car <- ggpairs(mtcars[, c("mpg", "wt", "cyl")], upper = "blank", title = "Custom Example")
#' # ggplot example taken from example(geom_text)
#'   plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x=wt, y=mpg, label=rownames(mtcars)))
#'   plot <- plot +
#'     ggplot2::geom_text(ggplot2::aes(colour=factor(cyl)), size = 3) +
#'     ggplot2::scale_colour_discrete(l=40)
#' custom_car[1, 2] <- plot
#' personal_plot <- ggally_text(
#'   "ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"
#' )
#' custom_car[1, 3] <- personal_plot
#' # custom_car
#'
#' # remove plots after creating a plot matrix
#' custom_car[2,1] <- NULL
#' custom_car[3,1] <- "blank" # the same as storing null
#' custom_car[3,2] <- NULL
#' custom_car
putPlot <- function(pm, value, i, j){
  pos <- get_pos(pm, i, j)
  if (is.null(value)) {
    pm$plots[[pos]] <- make_ggmatrix_plot_obj(wrap("blank", funcArgName = "ggally_blank"))
  } else if (mode(value) == "character") {
    if (value == "blank") {
      pm$plots[[pos]] <- make_ggmatrix_plot_obj(wrap("blank", funcArgName = "ggally_blank"))
    } else {
      stop("character values (besides 'blank') are not allowed to be stored as plot values.")
    }
  } else {
    pm$plots[[pos]] <- value
  }

  pm
}

#' getPlot
#'
#' Retrieves the ggplot object at the desired location.
#'
#' @param pm ggmatrix object to select from
#' @param i row from the top
#' @param j column from the left
#' @keywords hplot
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @importFrom utils capture.output
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  plotMatrix2 <- ggpairs(tips[, 3:2], upper = list(combo = "denstrip"))
#'  plotMatrix2[1, 2]
getPlot <- function(pm, i, j){
  if (FALSE) {
    cat("i: ", i, " j: ", j, "\n")
  }

  pos <- get_pos(pm, i, j)

  if (pos > length(pm$plots)) {
    plotObj <- NULL
  } else {
    plotObj <- pm$plots[[pos]]
  }

  if (is.null(plotObj)) {
    p <- ggally_blank()
  } else {
    if (ggplot2::is.ggplot(plotObj)) {
      p <- plotObj

    } else if (inherits(plotObj, "ggmatrix_plot_obj")) {
      fn <- plotObj$fn
      p <- fn(pm$data, plotObj$mapping)

    } else if (inherits(plotObj, "legend_guide_box")) {
      p <- plotObj

    } else {
      firstNote <- str_c("Position: i = ", i, ", j = ", j, "\nstr(plotObj):\n", sep = "")
      strObj <- capture.output({
        str(plotObj)
      })
      stop(str_c("unknown plot object type.\n", firstNote, strObj))
    }

    p <- add_gg_info(p, pm$gg)
  }

  p
}


get_pos <- function(pm, i, j) {
  if (isTRUE(pm$byrow)) {
    pos <- j + (pm$ncol * (i - 1))
  } else {
    pos <- i + (pm$nrow * (j - 1))
  }
  pos
}

get_pos_rev <- function(pm, pos) {
  if (isTRUE(pm$byrow)) {
    i <- ceiling(pos / pm$ncol)
    j <- (pos - 1) %% pm$ncol + 1
  } else {
    i <- (pos - 1) %% pm$nrow + 1
    j <- ceiling(pos / pm$nrow)
  }
  c(i, j)
}


check_i_j <- function(pm, i, j) {
  if ( (length(i) > 1) || (mode(i) != "numeric")) {
    stop("'i' may only be a single numeric value")
  }
  if ( (length(j) > 1) || (mode(j) != "numeric")) {
    stop("'j' may only be a single numeric value")
  }

  if (i > pm$nrow || i < 1) {
    stop("'i' may only be in the range from 1:", pm$nrow, ". Received: ", i)
  }
  if (j > pm$ncol || j < 1) {
    stop("'j' may only be in the range from 1:", pm$ncol, ". Received: ", j)
  }

  invisible()
}





is.ggmatrix <- function(x) {
  inherits(x, "ggmatrix")
}

#' @rdname getPlot
#' @usage \method{[}{ggmatrix}(pm, i, j, ...)
#' @param ... ignored
#' @export
`[.ggmatrix` <- function(pm, i, j, ...) {
  # pm <- ggpairs(iris, 1:3)
  if (missing(j)) {
    print(list(x = i, j = "(missing)"))
    if (is.vector(i)) {
      # pm[1:2]
      j <- i
      i <- seq_len(pm$nrow)
      pm[i,j]
    } else {

      if (is.null(nrow(i))) {
        stop("`[.ggmatrix` does not know how to handle non-logical matrix subsets")
      }
      if (!(is.logical(i))) {
        stop("`[.ggmatrix` does not know how to handle non-logical matrix subsets")
      }
      # pm[array(c(T, T, F, T, T, T, F, F, F), c(3,3), byrow = TRUE)]
      # pm[matrix(c(T, T, F, T, T, T, F, F, F), 3, 3, byrow = TRUE)]
      pos_pairs <- data.frame(x = numeric(), y = numeric())
      for (i_ in seq_len(nrow(m))) {
        for (j_ in seq_len(nrow(m))) {
          if (m[i_, j_]) {
            pos_pairs[nrow(pos_pairs) + 1, ] <- c(i_, j_)
          }
        }
      }
      print(pos_pairs)
      get_sub_ggmatrix(pm, pos_pairs, ...)
    }
  } else {
    if (length(i) > 1 || length(j) > 1) {
      # pm[1:2,3:4]
      i_mat <- matrix(rep(i, ncol(pm)), length(i), ncol(pm), byrow = TRUE)
      j_mat <- matrix(rep(j, nrow(pm)), nrow(pm), length(j), byrow = FALSE)
      get_sub_ggmatrix(pm, i_mat, j_mat, ...)
    } else {
      # pm[2,3]
      check_i_j(pm, i, j)
      getPlot(pm, i, j)
    }
  }
}

#' @rdname putPlot
#' @usage \method{[}{ggmatrix}(pm, i, j, ...) <- value
#' @param ... ignored
#' @export
`[<-.ggmatrix` <- function(pm, i, j, ..., value) {
  # x = matrix
  # i = first subset
  # j = second subset
  # y = value
  check_i_j(pm, i, j)

  putPlot(pm, value, i, j)
}

#' @export
length.ggmatrix <- function(pm) {
  pm$nrow * pm$ncol
}
# #' @export
# `[[.ggmatrix` <- function(pm, i) {
#   pos <- get_pos_rev(pm, i)
#   pm[pos[1], pos[2]]
# }
# #' @export
# `[[<-.ggmatrix` <- function(pm, i, v) {
#   pos <- get_pos_rev(pm, i)
#   pm[pos[1], pos[2]] <- v
#   pm
# }

#' @export
dim.ggmatrix <- function(pm) {
  c(pm$nrow, pm$ncol)
}

#' @export
t <- function(pm) {
  legend <- pm$legend
  if (!is.null(legend)) {
    if (!inherits(legend, "legend_guide_box")) {
      # if the legend exists in the pairs being copied
      if (is.numeric(legend) && length(legend) == 1) {
        legend <- get_pos_rev(pm, legend)
      }
      legend <- c(legend[2], legend[1])
    }
  }

  new_pm <- ggmatrix(
    plots = list(),
    nrow = pm$ncol,
    ncol = pm$ncol,
    xAxisLabels = pm$yAxisLabels,
    yAxisLabels = pm$xAxisLabels,
    title = pm$title,
    xlab = pm$ylab,
    ylab = pm$xlab,
    byrow = pm$byrow,
    showStrips = pm$showStrips,
    showAxisPlotLabels = NULL,
    showXAxisPlotLabels = pm$showYAxisPlotLabels,
    showYAxisPlotLabels = pm$showXAxisPlotLabels,
    labeller = pm$labeller,
    switch = pm$switch,
    xProportions = pm$yProportions,
    yProportions = pm$xProportions,
    data = pm$data,
    gg = pm$gg,
    legend = legend,
    progress = pm$progress
  )

  # copy over plots in proper locations
  for (i in seq_len(nrow(pm))) {
    for (j in seq_len(ncol(pm))) {
      new_pm[j, i] <- pm[i,j]
    }
  }
  new_pm
}

#' @export
dimnames.ggmatrix <- function(pm) {
  list(
    pm$xAxisLabels,
    pm$yAxisLabels
  )
}
#' @export
`dimnames<-.ggmatrix` <- function(pm, value) {
  pm$xAxisLabels <- value[[1L]]
  pm$yAxisLabels <- value[[2L]]
  pm
}

ulapply <- function(...) {
  unlist(lapply(...))
}


bind_first_legend <- function(pms, ggmatrix_legend) {
  legends <- lapply(pms, `[[`, "legend")
  non_null_legend_pos <- which(!ulapply(legends, is.null))
  legend <- NULL
  if (length(non_null_legend_pos) > 0) {
    if (!is.null(ggmatrix_legend)) {
      legend <- legends[[ggmatrix_legend[1]]]
    } else {
      first_legend <- min(non_null_legend_pos)
      legend <- legends[[first_legend]]
    }
    if (!inherits(legend, "legend_guide_box")) {
      # if the legend exists
      if (is.numeric(legend) && length(legend) == 1) {
        legend <- get_pos_rev(pms[[first_legend]], legend)
      }
      legend <- grab_legend(pms[[first_legend]][legend[1], legend[2]])
    }
  }

  legend
}
bind_proportions <- function(pms, prop_name) {
  proportions <- ulapply(pms, `[[`, prop_name)
  if (any(ulapply(proportions, inherits, "unit"))) {
    proportions <- lapply(
      seq_along(proportions),
      function(props_i) {
        ifnull(proportions[[props_i]], grid::unit(rep(1, ncol(pms[[props_i]])), "null"))
      }
    )
  }
  proportions
}


#' cbind and rbind ggmatrix objects
#'
#' Labels are used from the first ggmatrix object.  The legend is returned from the first legend unless otherwise specified.
#'
#' @export
#' @param ggmatrix_legend If \code{NULL} (default), the first legend is found and returned in the final matrix.  If an positive integer mapping to the plot matrix position, the legend of that ggmatrix will be used.
#' @rdname cbind_rbind
#' @return ggmatrix object
cbind.ggmatrix <- function(..., deparse.level = 1, ggmatrix_legend = NULL) {
  # get all plot matrices
  pms <- list(...)

  # make sure all matrices have same number of rows
  nrows <- ulapply(pms, nrow)
  if (length(unique(nrows)) > 1) {
    stop("All plot matrices must have the same number of rows to be `cbind`ed together")
  }

  # get columns of matrices
  total_ncol <- sum(ulapply(pms, ncol))

  legend <- bind_first_legend(pms, ggmatrix_legend)

  x_proportions <- bind_proportions(pms, "xProportions")

  first_pm <- pms[[1]]
  new_pm <- ggmatrix(
    plots = lapply(seq_len(nrow(first_pm) * total_ncol), function(ignore) NULL),
    nrow = nrow(first_pm),
    ncol = total_ncol,
    xAxisLabels = ulapply(pms, rownames),
    yAxisLabels = colnames(first_pm),
    title = first_pm$title,
    xlab = first_pm$xlab,
    ylab = first_pm$ylab,
    byrow = first_pm$byrow,
    showStrips = first_pm$showStrips,
    # null as the regular 'show axis' args are used below
    showAxisPlotLabels = NULL,
    showXAxisPlotLabels = first_pm$showYAxisPlotLabels,
    showYAxisPlotLabels = first_pm$showXAxisPlotLabels,
    labeller = first_pm$labeller,
    switch = first_pm$switch,
    xProportions = x_proportions,
    yProportions = first_pm$yProportions,
    # null as plots are retrieved and turned into ggplot2 objects
    data = NULL,
    gg = NULL,
    legend = legend,
    progress = first_pm$progress
  )

  # store all the plots
  seen_cols <- 0
  for (pm in pms) {
    for (i in seq_len(nrow(pm))) {
      for (j in seq_len(ncol(pm))) {
        new_pm[i,j + seen_cols] <- pm[i, j]
      }
    }
    seen_cols <- seen_cols + ncol(pm)
  }

  new_pm
}

#' @export
#' @rdname cbind_rbind
rbind.ggmatrix <- function(..., deparse.level = 1, ggmatrix_legend = NULL) {
  # get all plot matrices
  pms <- list(...)

  # make sure all matrices have same number of rows
  ncols <- ulapply(pms, ncol)
  if (length(unique(ncols)) > 1) {
    stop("All plot matrices must have the same number of columns to be `rbind`ed together")
  }

  # get columns of matrices
  total_nrow <- sum(ulapply(pms, nrow))

  legend <- bind_first_legend(pms, ggmatrix_legend)

  y_proportions <- bind_proportions(pms, "yProportions")

  first_pm <- pms[[1]]
  new_pm <- ggmatrix(
    plots = lapply(seq_len(ncol(first_pm) * total_nrow), function(ignore) NULL),
    nrow = total_nrow,
    ncol = ncol(first_pm),
    xAxisLabels = rownames(first_pm),
    yAxisLabels = ulapply(pms, colnames),
    title = first_pm$title,
    xlab = first_pm$xlab,
    ylab = first_pm$ylab,
    byrow = first_pm$byrow,
    showStrips = first_pm$showStrips,
    # null as the regular 'show axis' args are used below
    showAxisPlotLabels = NULL,
    showXAxisPlotLabels = first_pm$showYAxisPlotLabels,
    showYAxisPlotLabels = first_pm$showXAxisPlotLabels,
    labeller = first_pm$labeller,
    switch = first_pm$switch,
    xProportions = first_pm$xProportions,
    yProportions = y_proportions,
    # null as plots are retrieved and turned into ggplot2 objects
    data = NULL,
    gg = NULL,
    legend = legend,
    progress = first_pm$progress
  )

  # store all the plots
  seen_rows <- 0
  for (pm in pms) {
    for (i in seq_len(nrow(pm))) {
      for (j in seq_len(ncol(pm))) {
        new_pm[i + seen_rows, j] <- pm[i, j]
      }
    }
    seen_rows <- seen_rows + nrow(pm)
  }

  new_pm
}



get_sub_ggmatrix <- function(pm, i_mat, j_mat, ...) {
  browser()
  print(pos_pairs)
  i_vals <- pos_pairs$x
  j_vals <- pos_pairs$y
  if (mode(i_vals) != "numeric") {
    stop("'i' may only be a single numeric value")
  }
  if (mode(j_vals) != "numeric") {
    stop("'j' may only be a single numeric value")
  }
  min_i_val <- min(i_vals)
  if (max(i_vals) > pm$nrow || min_i_val < 1) {
    stop("'i' may only be in the range from 1:", pm$nrow, ". Received: ", paste(i_vals, collapse = ", "))
  }
  min_j_val <- min(j_vals)
  if (max(j_vals) > pm$ncol || min_j_val < 1) {
    stop("'j' may only be in the range from 1:", pm$ncol, ". Received: ", paste(j_vals, collapse = ", "))
  }

  plot_list <- list()

  # copy old ggmatrix
  new_pm <- pm

  new_pm$xAxisLabels <- pm$xAxisLabels[j_vals]
  new_pm$yAxisLabels <- pm$yAxisLabels[i_vals]
  new_pm$xProportions <- pm$xProportions[j_vals]
  new_pm$yProportions <- pm$yProportions[i_vals]

  # overwrite the ncolumns and row counts
  new_pm$nrow <- diff(range(i_vals)) + 1
  new_pm$ncol <- diff(range(j_vals)) + 1

  legend <- pm$legend
  if (!is.null(legend)) {
    if (!inherits(legend, "legend_guide_box")) {
      # if the legend exists in the pairs being copied
      if (is.numeric(legend) && length(legend) == 1) {
        legend <- get_pos_rev(pm, legend)
      }
      legend <- grab_legend(pm[legend[1], legend[2]])
    }
  }
  # store a list so that NULL values do not remove the key
  # new_pm[["legend"]] <- list(legend = legend)
  new_pm[["legend"]] <- legend

  # initialize all cells as blank plots
  new_pm$plots <- lapply(seq_len(nrow(new_pm) * ncol(new_pm)), function(ignore) NULL)

  # set all new cells as existing plots
  for (row in seq_len(nrow(pos_pairs))) {
    i <- pos_pairs$x[row]
    j <- pos_pairs$y[row]
    # print(list(x_new = i, y_new = j))
    new_pm[i - min_i_val + 1, j - min_j_val + 1] <- pm[i, j]
  }

  # return
  new_pm
}
