


#' Print ggpair object
#'
#' Specialized method to print the ggpair object-
#'
#' @param x ggpair object to be plotted
#' @param leftWidthProportion proportion of a plot area devoted to left axis labels
#' @param bottomHeightProportion proportion of a plot area devoted to bottom axis labels
#' @param spacingProportion proportion of a plot area devoted to the space between plots
#' @param xProportions proportion of a plot's horizontal space. This size defaults to 1
#' @param yProportions proportion of a plot's vertical space. This size defaults to 1
#' @param gridNewPage boolean that determines if a \code{\link[grid]{grid.newpage}()} should be executed before printing. Defaults to \code{TRUE}
#' @param ... ignored
#' @method print ggmatrix
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @importFrom grid gpar grid.layout grid.newpage grid.text grid.rect popViewport pushViewport viewport grid.draw
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  pMat <- ggpairs(tips, c(1,3,2), mapping = ggplot2::aes_string(color = "sex"))
#'  pMat # calls print(pMat), which calls print.ggmatrix(pMat)
#'
#'  ## defaults; (prints strips on top and right edges of matrix)
#'  # print(pMat, left = 0.2, spacing = 0.03, bottom = 0.1)
#'
#'  ## give the left axis labels area a proportion of 3 plot size
#'  # print(pMat, leftWidthProportion = 3)
#'
#'  ## give the bottom axis labels area a proportion of 1 plot size
#'  # print(pMat, bottomHeightProportion = 1)
#'
#'  ## give the spacing between plots a proportion of 1 plot size
#'  # print(pMat, spacing = 1)
print.ggmatrix <- function(
  x,
  leftWidthProportion = 0.2,
  bottomHeightProportion = 0.1,
  spacingProportion = 0.03,
  xProportions = rep(1, x$ncol),
  yProportions = rep(1, x$nrow),
  gridNewPage = TRUE,

  use_cache = TRUE,
  ...
) {

  pm <- x

  # browser()
  # library(ggplot2)
  # library(grid)
  # library(gtable)
  #
  # pm <- ggduo(iris, columnsX = 1:3, columnsY = 4:5, columnLabelsX = c("A Column", "B Column", "C Column"), columnLabelsY = c("FIRST row", "second ROW"))
  # pm[1,1] <- qplot(Species, data = iris) + coord_flip()

  fake_data <- expand.grid(Var1 = pm$xAxisLabels, Var2 = pm$yAxisLabels)
  fake_data$x <- 1
  fake_data$y <- 1

  # p <- qplot(Sepal.Length, Sepal.Width, data = iris)
  # p <- qplot(Species, data = iris) + coord_flip()
  # pa <- plot_gtable(p)
  # pa_left_labels <- gtable_filter(pa, "axis-l")$grobs
  #
  # z <- gtable_filter(pa, "axis-l")$grobs[[1]]
  # zz <- grid::unit(1, "grobwidth", data = z)


  pm_fake <- ggplot(fake_data, mapping = aes_("x", "y")) +
    geom_point() +
    facet_grid(Var2 ~ Var1) + # make the 'fake' strips for x and y titles
    labs(title = pm$title) + # add title in
    labs(x = NULL, y = NULL) # remove both x and y titles

  pmg <- plot_gtable(pm_fake)
  pmg$layout$grob_pos <- seq_along(pmg$grobs)

  # correct left axis label width
  axis_sizes <- numeric(pm$nrow)

  for(i in seq_len(pm$nrow)) {
    axis_sizes[i] <- axis_size_left(pm[i, 1])
  }
  axis_size_left <- max(axis_sizes)
  axis_l_grob_pos <- which(pmg$layout$name == "axis-l")
  width_pos <- pmg$layout[axis_l_grob_pos, c("l", "r")]
  width_pos <- unique(unlist(width_pos))
  if (length(width_pos) > 1) {
    stop("width issue!")
  }
  pmg$widths[width_pos] <- unit(axis_size_left, "cm")

  # correct bottom axis label height
  axis_sizes <- numeric(pm$ncol)
  for(j in seq_len(pm$ncol)) {
    axis_sizes[j] <- axis_size_bottom(pm[pm$nrow, j])
  }
  axis_size_bottom <- max(axis_sizes)
  axis_b_grob_pos <- which(pmg$layout$name == "axis-b")
  height_pos <- pmg$layout[pmg$layout$name == "axis-b", "t"]
  height_pos <- unique(unlist(height_pos))
  if (length(height_pos) > 1) {
    stop("height issue!")
  }
  pmg$heights[height_pos] <- unit(axis_size_bottom, "cm")


  # zero out rest of the plotting area
  zero_pos_vals <- pmg$layout$grob_pos[pmg$layout$name %in% c("panel", "axis-l", "axis-b")]
  for (zero_pos in zero_pos_vals) {
    pmg$grobs[[zero_pos]] <- ggplot2::zeroGrob()
  }
  pmg


  panel_locations <- subset(pmg$layout, name == "panel")
  panel_locations_order <- order(panel_locations$l, panel_locations$t, decreasing = FALSE)
  panel_locations <- panel_locations[panel_locations_order, "grob_pos"]


  plot_number <- 0
  for (j in seq_len(pm$ncol)) {
    for (i in seq_len(pm$nrow)) {
      plot_number <- plot_number + 1
      grob_pos <- panel_locations[plot_number]

      p <- pm[i,j]

      if (is_blank_plot(p)) {
        next
      }

      if (!is.ggplot(p)) {
        pmg$grobs[[grob_pos]] <- p
        next
      }

      if (j == 1 && pm$showYAxisPlotLabels) {
        pmg <- add_left_axis(
          pmg, pg, i,
          show_strips = is.null(pm$showStrips) || isTRUE(pm$showStrips),
          grob_pos = axis_l_grob_pos[i]
        )
      }
      if (i == pm$nrow && pm$showXAxisPlotLabels) {
        pmg <- add_bottom_axis(
          pmg, pg, j,
          show_strips = is.null(pm$showStrips) || isTRUE(pm$showStrips),
          grob_pos = axis_b_grob_pos[j]
        )
      }

      # grab plot panel and insert
      pmg$grobs[[grob_pos]] <- plot_panel(
        pg = plot_gtable(p),
        row_pos = i, col_pos = j,
        matrix_show_strips = pm$showStrips,
        matrix_ncol = pm$ncol,
        maxtrix_show_legends = pm$showLegends,
        plot_show_axis_labels = p$showLabels
      )
    }
  }


  grid.draw(pmg)

}
