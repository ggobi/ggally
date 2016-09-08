


#' Print ggpair object
#'
#' Specialized method to print the ggpair object-
#'
#' @param x ggpair object to be plotted
#' @param ... ignored
# ' @method ggprint ggmatrix
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @importFrom grid gpar grid.layout grid.newpage grid.text grid.rect popViewport pushViewport viewport grid.draw
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  pm <- ggpairs(tips, c(1,3,2), mapping = ggplot2::aes_string(color = "sex"))
#'  ggprint(pm) # calls print(pm), which calls ggprint.ggmatrix(pm)
ggprint <- function(
  x,
  ...,
  progress = interactive(),
  progress_wait = 5
) {

  pm <- x

  # make a fake facet grid to fill in with proper plot panels
  fake_data <- expand.grid(Var1 = pm$xAxisLabels, Var2 = pm$yAxisLabels)
  fake_data$x <- 1
  fake_data$y <- 1
  pm_fake <- ggplot(fake_data, mapping = aes_("x", "y")) +
    geom_point() +
    facet_grid(Var2 ~ Var1) + # make the 'fake' strips for x and y titles
    labs(title = pm$title) + # add title in
    labs(x = NULL, y = NULL) # remove both x and y titles

  # make a gtable of the plot matrix (to be filled in)
  pmg <- plot_gtable(pm_fake)

  # help with grob positions
  pmg$layout$grob_pos <- seq_along(pmg$grobs)

  # zero out rest of the plotting area
  zero_pos_vals <- pmg$layout$grob_pos[pmg$layout$name %in% c("panel", "axis-l", "axis-b")]
  for (zero_pos in zero_pos_vals) {
    pmg$grobs[[zero_pos]] <- ggplot2::zeroGrob()
  }
  pmg

  # Get all 'panel' grob_pos in the pmg
  panel_locations <- subset(pmg$layout, name == "panel")
  panel_locations_order <- order(panel_locations$l, panel_locations$t, decreasing = FALSE)
  panel_locations <- panel_locations[panel_locations_order, "grob_pos"]

  # init the axis sizes
  left_axis_sizes <- numeric(pm$nrow + 1)
  bottom_axis_sizes <- numeric(pm$ncol + 1)
  axis_l_grob_pos <- subset(pmg$layout, name == "axis-l", "grob_pos")$grob_pos
  axis_b_grob_pos <- subset(pmg$layout, name == "axis-b", "grob_pos")$grob_pos

  pb <- NULL
  start_time <- Sys.time()
  plot_count <- pm$ncol * pm$nrow

  plot_number <- 0
  for (j in seq_len(pm$ncol)) {
    for (i in seq_len(pm$nrow)) {
      plot_number <- plot_number + 1
      grob_pos <- panel_locations[plot_number]

      # update the progress bar is possible
      if (isTRUE(progress) && (Sys.time() - start_time > 5)) {
        if (is.null(pb)) {
          pb <- utils::txtProgressBar(initial = plot_number / plot_count, style = 3)
        } else {
          utils::setTxtProgressBar(pb, plot_number / plot_count)
        }
      }

      p <- pm[i,j]

      if (is_blank_plot(p)) {
        # ignore all blank plots.  all blank plots to not draw anything else
        next
      }

      # if it's not a ggplot2 obj, insert it and pray it works
      if (!is.ggplot(p)) {
        pmg$grobs[[grob_pos]] <- p
        next
      }

      pg <- plot_gtable(p)

      # if the axis
      if (j == 1 && pm$showYAxisPlotLabels) {
        left_axis_sizes[i] <- axis_size_left(pg)

        pmg <- add_left_axis(
          pmg, pg,
          show_strips = i == 1 || is.null(pm$showStrips) || isTRUE(pm$showStrips),
          grob_pos = axis_l_grob_pos[i]
        )
      }
      if (i == pm$nrow && pm$showXAxisPlotLabels) {
        bottom_axis_sizes[j] <- axis_size_bottom(pg)

        pmg <- add_bottom_axis(
          pmg, pg,
          show_strips = j == pm$ncol || is.null(pm$showStrips) || isTRUE(pm$showStrips),
          grob_pos = axis_b_grob_pos[j]
        )
      }

      # grab plot panel and insert
      pmg$grobs[[grob_pos]] <- plot_panel(
        pg = pg,
        row_pos = i, col_pos = j,
        matrix_show_strips = pm$showStrips,
        matrix_ncol = pm$ncol,
        maxtrix_show_legends = pm$showLegends,
        plot_show_axis_labels = p$showLabels
      )
    }
  }

  pmg <- set_max_axis_size(
    pmg,
    axis_sizes = left_axis_sizes,
    layout_name = "axis-l",
    layout_cols = c("l", "r"),
    pmg_key = "widths",
    stop_msg = "left axis width issue!! Fix!"
  )
  pmg <- set_max_axis_size(
    pmg,
    axis_sizes = bottom_axis_sizes,
    layout_name = "axis-b",
    layout_cols = c("t", "b"),
    pmg_key = "heights",
    stop_msg = "bottom axis height issue!! Fix!"
  )

  # close the progress bar
  if (!is.null(pb)) {
    # pb$term()
    close(pb)
  }

  grid.draw(pmg)

}
