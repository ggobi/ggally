#' \code{\link{ggmatrix}} \pkg{gtable} object
#'
#' Specialized method to print the \code{\link{ggmatrix}} object.
#'
#' @param pm \code{\link{ggmatrix}} object to be plotted
#' @param ... ignored
#' @param progress,progress_format `r lifecycle::badge("deprecated")` Please use the 'progress' parameter in your \code{\link{ggmatrix}}-like function.  See \code{\link{ggmatrix_progress}} for a few examples.
#' @author Barret Schloerke
#' @importFrom grid gpar grid.layout grid.newpage grid.text grid.rect popViewport pushViewport viewport grid.draw
#' @export
#' @examples
#' data(tips)
#' pm <- ggpairs(tips, c(1, 3, 2), mapping = ggplot2::aes(color = sex))
#' ggmatrix_gtable(pm)
ggmatrix_gtable <- function(
  pm,
  ...,
  progress = NULL,
  progress_format = formals(ggmatrix_progress)$format
) {
  # pm is for "plot matrix"

  # init progress bar handle
  if (missing(progress) && missing(progress_format)) {
    # only look at plot matrix for progress bar
    hasProgressBar <- !isFALSE(pm$progress)
    progress_fn <- pm$progress
  } else {
    lifecycle::deprecate_soft(
      when = "2.3.0",
      what = I("`progress` and `progress_format`"),
      details = "Please use the 'progress' parameter in your ggmatrix-like function call.  See ?ggmatrix_progress for a few examples."
    )

    # has progress variable defined
    # overrides pm$progress
    if (missing(progress_format)) {
      progress_fn <- as_ggmatrix_progress(progress)
    } else {
      progress_fn <- as_ggmatrix_progress(
        progress,
        pm$ncol * pm$nrow,
        format = progress_format
      )
    }
    hasProgressBar <- !isFALSE(progress_fn)
    ggmatrix_progress
  }
  if (hasProgressBar) {
    pb <- progress_fn(pm)
    # pb$tick(tokens = list(plot_i = 1, plot_j = 1))
  }

  # make a fake facet grid to fill in with proper plot panels
  get_labels <- function(labels, length_out, name) {
    if (is.expression(labels)) {
      stop(
        "'",
        name,
        "' can only be a character vector or NULL.",
        "  Character values can be parsed using the 'labeller' parameter."
      )
    }
    labels %||% as.character(seq_len(length_out))
  }
  fake_data <- expand.grid(
    Var1 = get_labels(pm$xAxisLabels, pm$ncol, "xAxisLabels"),
    Var2 = get_labels(pm$yAxisLabels, pm$nrow, "yAxisLabels")
  )
  fake_data$x <- 1
  fake_data$y <- 1

  # make the smallest plot possible so the guts may be replaced
  pm_fake <- ggplot(fake_data, mapping = aes(!!as.name("x"), !!as.name("y"))) +
    geom_point() +
    # make the 'fake' strips for x and y titles
    facet_grid(
      Var2 ~ Var1,
      labeller = pm$labeller %||% "label_value",
      switch = pm$switch
    ) +
    # remove both x and y titles
    labs(x = pm$xlab, y = pm$ylab)

  # add all custom ggplot2 things
  pm_fake <- add_gg_info(pm_fake, pm$gg)

  # add the title or remove the location completely
  if (is.null(pm$title)) {
    pm_fake <- pm_fake + theme(plot.title = element_blank())
  } else {
    pm_fake <- pm_fake + labs(title = pm$title)
  }

  # if there are no labels, then there should be no strips
  if (is.null(pm$xAxisLabels)) {
    pm_fake <- pm_fake + theme(strip.text.x = element_blank())
  }
  if (is.null(pm$yAxisLabels)) {
    pm_fake <- pm_fake + theme(strip.text.y = element_blank())
  }

  # if there is a legend, make a fake legend that will be replaced later
  if (!is.null(pm$legend)) {
    pm_fake <- pm_fake + geom_point(mapping = aes(color = !!as.name("Var1")))
  }

  # Suppress warnings such as:
  # ```r
  # ggbivariate(
  #   tips,
  #   "smoker",
  #   c("day", "time", "sex", "tip"),
  #   title = "Custom title"
  # ) +
  #   labs(fill = "Smoker ?")
  # ```
  # ```
  # Ignoring unknown labels:
  # * fill : "Smoker ?"
  # ````
  # as the plot / data does not exist yet
  suppressWarnings(
    # make a gtable of the plot matrix (to be filled in)
    pmg <- plot_gtable(pm_fake)
  )

  ###############
  ## Everything beyond this point is only to fill in the correct information.
  ## No grobs should be appended or removed.  It should be done with themes or geoms above
  ###############

  # help with grob positions
  pmg$layout$grob_pos <- seq_along(pmg$grobs)
  pmg_layout <- pmg$layout
  pmg_layout_name <- pmg_layout$name
  pmg_layout_grob_pos <- pmg_layout$grob_pos

  # zero out rest of the plotting area (just in case it is not replaced)
  zero_pos_vals <- pmg_layout_grob_pos[
    str_detect(
      pmg_layout_name,
      paste(c("panel", "axis-l", "axis-b", "guide-box"), collapse = "|")
    )
  ]
  for (zero_pos in zero_pos_vals) {
    pmg$grobs[[zero_pos]] <- ggplot2::zeroGrob()
  }
  pmg

  # insert legend
  if (!is.null(pm$legend)) {
    legend <- pm$legend
    if (is.numeric(legend)) {
      if (length(legend) == 1) {
        legend <- get_pos_rev(pm, legend)
      } else if (length(legend) > 2) {
        stop(
          "'legend' must be a single or double numberic value.  Or 'legend' must be an object produced from 'grab_legend()'"
        )
      }

      legend_obj <- grab_legend(pm[legend[1], legend[2]])
    } else if (inherits(legend, "legend_guide_box")) {
      legend_obj <- legend
    }

    legend_layout <- pmg_layout[grepl("guide-box", pmg_layout_name), ]
    class(legend_obj) <- setdiff(class(legend_obj), "legend_guide_box")
    index <- legend_layout$grob_pos[match(
      legend_obj$layout$name,
      legend_layout$name
    )]
    pmg$grobs[index] <- legend_obj$grobs

    if ("guide-box" %in% legend_layout$name) {
      legend_position <- pm_fake$theme$legend.position %||% "right"

      if (legend_position %in% c("right", "left")) {
        pmg$widths[[legend_layout$l]] <- legend_obj$widths[1]
      } else if (legend_position %in% c("top", "bottom")) {
        pmg$heights[[legend_layout$t]] <- legend_obj$heights[1]
      } else {
        stop(paste(
          "ggmatrix does not know how display a legend when legend.position with value: '",
          legend_position,
          "'. Valid values: c('right', 'left', 'bottom', 'top')",
          sep = ""
        ))
      }
    } else {
      # From ggplot 3.5.0 onwards, a plot can have multiple legends
      lr <- intersect(
        c("guide-box-left", "guide-box-right"),
        legend_obj$layout$name
      )
      if (length(lr) > 0) {
        width <- legend_obj$widths[legend_obj$layout$l[match(
          lr,
          legend_obj$layout$name
        )]]
        pmg$widths[legend_layout$l[match(lr, legend_layout$name)]] <- width
      }

      tb <- intersect(
        c("guide-box-bottom", "guide-box-right"),
        legend_obj$layout$name
      )
      if (length(tb) > 0) {
        height <- legend_obj$heights[legend_obj$layout$t[match(
          tb,
          legend_obj$layout$name
        )]]
        pmg$heights[legend_layout$t[match(tb, legend_layout$name)]] <- height
      }
    }
  }

  # Get all 'panel' grob_pos in the pmg
  panel_layout <- pmg_layout[str_detect(pmg_layout_name, "panel"), ]
  panel_locations_order <- order(
    panel_layout$t,
    panel_layout$l,
    decreasing = FALSE
  )
  panel_locations <- panel_layout[panel_locations_order, "grob_pos"]

  # init the axis sizes
  left_axis_sizes <- numeric(pm$nrow + 1)
  bottom_axis_sizes <- numeric(pm$ncol + 1)
  axis_l_grob_pos <- pmg_layout_grob_pos[str_detect(pmg_layout_name, "axis-l")]
  axis_b_grob_pos <- pmg_layout_grob_pos[str_detect(pmg_layout_name, "axis-b")]

  # change the plot size ratios
  x_proportions <- pm$xProportions
  if (!is.null(x_proportions)) {
    panel_width_pos <- sort(unique(panel_layout$l))
    if (!inherits(x_proportions, "unit")) {
      x_proportions <- grid::unit(x_proportions, "null")
    }
    pmg$widths[panel_width_pos] <- x_proportions
  }
  y_proportions <- pm$yProportions
  if (!is.null(y_proportions)) {
    panel_height_pos <- sort(unique(panel_layout$t))
    if (!inherits(y_proportions, "unit")) {
      y_proportions <- grid::unit(y_proportions, "null")
    }
    pmg$heights[panel_height_pos] <- y_proportions
  }

  # build and insert all plots and axis labels
  plot_number <- 0
  for (i in seq_len(pm$nrow)) {
    for (j in seq_len(pm$ncol)) {
      plot_number <- plot_number + 1
      grob_pos_panel <- panel_locations[plot_number]

      # update the progress bar is possible
      if (hasProgressBar) {
        pb$tick(tokens = list(plot_i = i, plot_j = j))
      }

      # retrieve plot
      p <- pm[i, j]

      # ignore all blank plots.  all blank plots do not draw anything else
      if (is_blank_plot(p)) {
        next
      }

      # if it's not a ggplot2 obj, insert it and pray it works
      if (!is_ggplot(p)) {
        pmg$grobs[[grob_pos_panel]] <- p
        next
      }

      # get the plot's gtable to slice and dice
      pg <- plot_gtable(p)

      # if the left axis should be added
      if (j == 1 && pm$showYAxisPlotLabels) {
        left_axis_sizes[i] <- axis_size_left(pg)

        pmg <- add_left_axis(
          pmg,
          pg,
          show_strips = ((i == 1) && is.null(pm$showStrips)) ||
            isTRUE(pm$showStrips),
          grob_pos = axis_l_grob_pos[i]
        )
      }
      # if the bottom axis should be added
      if (i == pm$nrow && pm$showXAxisPlotLabels) {
        bottom_axis_sizes[j] <- axis_size_bottom(pg)

        pmg <- add_bottom_axis(
          pmg,
          pg,
          show_strips = ((j == pm$ncol) && is.null(pm$showStrips)) ||
            isTRUE(pm$showStrips),
          grob_pos = axis_b_grob_pos[j]
        )
      }

      # grab plot panel and insert
      pmg$grobs[[grob_pos_panel]] <- plot_panel(
        pg = pg,
        row_pos = i,
        col_pos = j,
        matrix_show_strips = pm$showStrips,
        matrix_ncol = pm$ncol,
        plot_show_axis_labels = p$showLabels
      )
    }
  }

  # make sure the axes have enough room

  pmg <- set_max_axis_size(
    pmg,
    axis_sizes = left_axis_sizes,
    layout_name = "axis-l",
    layout_cols = c("l", "r"),
    pmg_key = "widths"
    # stop_msg = "left axis width issue!! Fix!"
  )
  pmg <- set_max_axis_size(
    pmg,
    axis_sizes = bottom_axis_sizes,
    layout_name = "axis-b",
    layout_cols = c("t", "b"),
    pmg_key = "heights"
    # stop_msg = "bottom axis height issue!! Fix!"
  )

  pmg
}
