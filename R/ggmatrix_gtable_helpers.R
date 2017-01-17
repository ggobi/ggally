

plot_gtable <- function(p) {
  ggplot_gtable(ggplot_build(p))
}


# axis_size_left(p)
# axis_size_bottom(p)
# axis_size_left(g)
# axis_size_bottom(g)
axis_list <- (function(){
  axis_label_size_wrapper <- function(fn, filter_val, select_val, unitTo, valueOnly) {
    function(pg) {
      pg_axis <- gtable::gtable_filter(pg, filter_val)
      items <- pg_axis[[select_val]]
      if (!inherits(items, "unit.list")) {
        ret <- fn(items, unitTo = unitTo, valueOnly = valueOnly)
      } else {
        ret <- vapply(items, fn, numeric(1), unitTo = unitTo, valueOnly = valueOnly)
      }
      max(ret)
    }
  }

  axis_size_left <- axis_label_size_wrapper(
    grid::convertWidth,
    "axis-l",
    "widths",
    unitTo = "cm", valueOnly = TRUE
  )
  axis_size_bottom <- axis_label_size_wrapper(
    grid::convertHeight,
    "axis-b",
    "heights",
    unitTo = "cm", valueOnly = TRUE
  )

  list(axis_size_left, axis_size_bottom)
})()
axis_size_left <- axis_list[[1]]
axis_size_bottom <- axis_list[[2]]


# add_correct_label <- function(pmg, pm,



plot_panel <- function(
  pg,
  row_pos, col_pos,
  matrix_show_strips,
  matrix_ncol,
  plot_show_axis_labels
) {

  # ask about strips
  layout_names <- c("panel")
  strip_right_name <- "strip-r|strip-l"
  strip_top_name <- "strip-t|strip-b"
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
    if (plot_show_axis_labels %in% c("internal", "none")) {
      layout_names <- all_layout_names
    }
  }

  # get correct panel (and strips)
  layout_rows <- str_detect(pg$layout$name, paste(layout_names, collapse = "|"))

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
  layout <- pg$layout
  layout_name <- layout$name

  # axis layout info
  al <- layout[str_detect(layout_name, "axis-l"), ]

  if (show_strips) {
    alx <- layout[str_detect(layout_name, "axis-l|strip-t|strip-b"), ]
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
  layout <- pg$layout
  layout_name <- layout$name
  # axis layout info
  al <- layout[str_detect(layout_name, "axis-b"), ]

  if (show_strips) {
    alx <- layout[str_detect(layout_name, "axis-b|strip-r|strip-l"), ]
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



set_max_axis_size <- function(pmg, axis_sizes, layout_name, layout_cols, pmg_key) {
  m_axis_size <- max(axis_sizes, na.rm = TRUE)
  grob_pos_vals <- which(str_detect(pmg$layout$name, layout_name))
  val_pos <- pmg$layout[grob_pos_vals, layout_cols]
  val_pos <- unique(unlist(val_pos))
  # if (length(val_pos) > 1) {
  #   stop(stop_msg)
  # }

  pmg[[pmg_key]][[val_pos]] <- unit(m_axis_size, "cm")
  pmg
}
