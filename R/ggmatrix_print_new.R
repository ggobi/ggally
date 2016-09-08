


#' Print ggpair object
#'
#' Specialized method to print the ggpair object-
#'
#' @param x ggpair object to be plotted
#' @param ... ignored
#' @param progress boolean to determine if a progress bar should be displayed. This defaults to interactive sessions only
#' @param progress_wait how many seconds the progress bar will wait until appearing. Defaults to 5 seconds.
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

  pm <- x # pm is for "plot matrix"
  pb <- NULL # init progress bar handle
  start_time <- Sys.time() # init start time
  plot_count <- pm$ncol * pm$nrow # how many plots are produced


  # make a fake facet grid to fill in with proper plot panels
  fake_data <- expand.grid(
    Var1 = ifnull(pm$xAxisLabels, as.character(seq_len(pm$ncol))),
    Var2 = ifnull(pm$yAxisLabels, as.character(seq_len(pm$nrow)))
  )
  fake_data$x <- 1
  fake_data$y <- 1

  # make the smallest plot possible so the guts may be replaced
  pm_fake <- ggplot(fake_data, mapping = aes_("x", "y")) +
    geom_point() +
    facet_grid(Var2 ~ Var1) + # make the 'fake' strips for x and y titles
    labs(x = pm$xlab, y = pm$ylab) # remove both x and y titles

  # add all custom ggplot2 things
  if (!is.null(pm$gg)) {
    pm_fake <- pm_fake + pm$gg
  }

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
    pm_fake <- pm_fake + geom_point(mapping = aes_(color = "Var1"))
  }

  # make a gtable of the plot matrix (to be filled in)
  pmg <- plot_gtable(pm_fake)

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
  zero_pos_vals <- pmg_layout_grob_pos[pmg_layout_name %in% c("panel", "axis-l", "axis-b", "guide-box")]
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
        stop("'legend' must be a single or double numberic value.  Or 'legend' must be an object produced from 'grab_legend()'")
      }

      legend_obj <- grab_legend(pm[legend[1], legend[2]])

    } else if (inherits(legend, "legend_guide_box")) {
      legend_obj <- legend
    }


    legend_layout <- (pmg_layout[pmg_layout_name == "guide-box", ])[1, ]
    class(legend_obj) <- setdiff(class(legend_obj), "legend_guide_box")
    pmg$grobs[[legend_layout$grob_pos]] <- legend_obj

    legend_position <- ifnull(pm_fake$theme$legend.position, "right")

    if (legend_position %in% c("right", "left")) {
      pmg$widths[[legend_layout$l]] <- legend_obj$widths[1]
    } else if (legend_position %in% c("top", "bottom")) {
      pmg$heights[[legend_layout$t]] <- legend_obj$heights[1]
    } else {
      message("funny legend position")
      browser()
    }
  }



  # Get all 'panel' grob_pos in the pmg
  panel_locations <- pmg_layout[pmg_layout_name  == "panel", ]
  panel_locations_order <- order(panel_locations$l, panel_locations$t, decreasing = FALSE)
  panel_locations <- panel_locations[panel_locations_order, "grob_pos"]

  # init the axis sizes
  left_axis_sizes <- numeric(pm$nrow + 1)
  bottom_axis_sizes <- numeric(pm$ncol + 1)
  axis_l_grob_pos <- pmg_layout_grob_pos[pmg_layout_name == "axis-l"]
  axis_b_grob_pos <- pmg_layout_grob_pos[pmg_layout_name == "axis-b"]


  # build and insert all plots and axis labels
  plot_number <- 0
  for (j in seq_len(pm$ncol)) {
    for (i in seq_len(pm$nrow)) {
      plot_number <- plot_number + 1
      grob_pos <- panel_locations[plot_number]

      # update the progress bar is possible
      if (isTRUE(progress) && (Sys.time() - start_time > progress_wait)) {
        if (is.null(pb)) {
          pb <- utils::txtProgressBar(initial = plot_number / plot_count, style = 3)
        } else {
          utils::setTxtProgressBar(pb, plot_number / plot_count)
        }
      }

      # retrieve plot
      p <- pm[i,j]

      # ignore all blank plots.  all blank plots do not draw anything else
      if (is_blank_plot(p)) {
        next
      }

      # if it's not a ggplot2 obj, insert it and pray it works
      if (!is.ggplot(p)) {
        pmg$grobs[[grob_pos]] <- p
        next
      }

      # get the plot's gtable to slice and dice
      pg <- plot_gtable(p)

      # if the left axis should be added
      if (j == 1 && pm$showYAxisPlotLabels) {
        left_axis_sizes[i] <- axis_size_left(pg)

        pmg <- add_left_axis(
          pmg, pg,
          show_strips = ((i == 1) && is.null(pm$showStrips)) || isTRUE(pm$showStrips),
          grob_pos = axis_l_grob_pos[i]
        )
      }
      # if the bottom axis should be added
      if (i == pm$nrow && pm$showXAxisPlotLabels) {
        bottom_axis_sizes[j] <- axis_size_bottom(pg)

        pmg <- add_bottom_axis(
          pmg, pg,
          show_strips = ((j == pm$ncol) && is.null(pm$showStrips)) || isTRUE(pm$showStrips),
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

  # make sure the axes have enough room
  pmg <- set_max_axis_size(
    pmg,
    axis_sizes = left_axis_sizes,
    layout_name = "axis-l",
    layout_cols = c("l", "r"),
    pmg_key = "widths"
    #stop_msg = "left axis width issue!! Fix!"
  )
  pmg <- set_max_axis_size(
    pmg,
    axis_sizes = bottom_axis_sizes,
    layout_name = "axis-b",
    layout_cols = c("t", "b"),
    pmg_key = "heights"
    #stop_msg = "bottom axis height issue!! Fix!"
  )

  # close the progress bar
  if (!is.null(pb)) {
    close(pb)
  }

  # draw the giant gtable obj
  grid.draw(pmg)
  # returns nothing

  invisible(pm)
}
