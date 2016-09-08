

plot_gtable <- function(p) {
  ggplot_gtable(ggplot_build(p))
}


# axis_size_left(p)
# axis_size_bottom(p)
# axis_size_left(g)
# axis_size_bottom(g)
axis_label_size_wrapper <- function(is_width) {
  if (isTRUE(is_width)) {
    fn <- utils::getFromNamespace("width_cm", "ggplot2")
    filter_val <- "axis-l"
    select_val <- "widths"
  } else {
    fn <- utils::getFromNamespace("height_cm", "ggplot2")
    filter_val <- "axis-b"
    select_val <- "heights"
  }
  function(pg) {
    pg_axis <- gtable::gtable_filter(pg, filter_val)
    max(fn(pg_axis[[select_val]]))
  }
}
axis_size_left <- axis_label_size_wrapper(TRUE)
axis_size_bottom <- axis_label_size_wrapper(FALSE)


# add_correct_label <- function(pmg, pm,



plot_panel <- function(pg, row_pos, col_pos, matrix_show_strips, matrix_ncol, maxtrix_show_legends, plot_show_axis_labels) {

  # ask about strips
  layout_names <- c("panel")
  strip_right_name <- "strip-right"
  strip_top_name <- "strip-top"
  legend_name <- "guide-box"
  all_layout_names <- c(layout_names, strip_right_name, strip_top_name, legend_name)

  if (is.null(matrix_show_strips)) {
    # make sure it's on the outer right and top edge
    if (col_pos == (matrix_ncol)) {
      layout_names <- c(layout_names, strip_right_name)
    }
    if (row_pos == 1) {
      layout_names <- c(layout_names, strip_top_name)
    }

  } else if (matrix_show_strips) {
    layout_names <- c(layout_names, strip_right_name, strip_top_name)
  }

  # if they have a custom plot, make sure it shows up
  if (! is.null(plot_show_axis_labels)) {
    # pShowStrips <- ! identical(p$axisLabels, FALSE)

    # copied from old code.  want to replace it to something like above
    if (p_axis_labels %in% c("internal", "none")) {
      layout_names <- alllayout_names
    }
  }

  if (isTRUE(maxtrix_show_legends)) {
    layout_names <- c(layout_names, legend_name)
  }

  # get correct panel (and strips)
  layout_rows <- pg$layout$name %in% layout_names

  layout_info <- pg$layout[layout_rows, ]
  top_bottom <- layout_info[, c("t", "b")]
  left_right <- layout_info[, c("l", "r")]

  plot_panel <- pg[
    min(top_bottom):max(top_bottom),
    min(left_right):max(left_right)
  ]

  plot_panel
}






add_left_axis <- function(pmg, pg, show_strips, grob_pos) {
  # axis layout info
  al <- subset(pg$layout, name == "axis-l")

  if (show_strips) {
    alx <- subset(pg$layout, name %in% c("axis-l", "strip-top"))
  } else {
    alx <- al
  }

  # get only the axis left objects (and maybe strip top spacer)
  axis_panel <- pg[min(alx$b):max(alx$t), min(al$l)]

  # force to align left
  axis_panel <- gtable::gtable_add_cols(axis_panel, grid::unit(1, "null"), 0)
  pmg$grobs[[grob_pos]] <- axis_panel

  pmg
}


add_bottom_axis <- function(pmg, pg, show_strips, grob_pos) {
  # axis layout info
  al <- subset(pg$layout, name == "axis-b")

  if (show_strips) {
    alx <- subset(pg$layout, name %in% c("axis-b", "strip-right"))
  } else {
    alx <- al
  }

  # get only the axis left objects (and maybe strip top spacer)
  axis_panel <- pg[min(al$t), min(alx$l):max(alx$r)]

  # force to align top
  axis_panel <- gtable::gtable_add_rows(axis_panel, grid::unit(1, "null"), 1)
  pmg$grobs[[grob_pos]] <- axis_panel

  pmg
}



set_max_axis_size <- function(pmg, axis_sizes, layout_name, layout_cols, pmg_key, stop_msg) {
  m_axis_size <- max(axis_sizes, na.rm = TRUE)
  grob_pos_vals <- which(pmg$layout$name == layout_name)
  val_pos <- pmg$layout[grob_pos_vals, layout_cols]
  val_pos <- unique(unlist(val_pos))
  if (length(val_pos) > 1) {
    stop(stop_msg)
  }

  pmg[[pmg_key]][val_pos] <- unit(m_axis_size, "cm")
  pmg
}
