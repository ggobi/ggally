# This file takes too long
testthat::skip_on_cran()
testthat::skip_if_not_installed("Hmisc")

# list of the different plot types to check
# continuous
#    points
#    smooth
#    smooth_loess
#    density
#    cor
#   blank

# combo
#   box
#   dot plot
#   facethist
#   facetdensity
#   denstrip
#   blank

# discrete
#   ratio
#   facetbar
#   blank

gn <- function(x) {
  fnName <- attr(x, "name")
  fnName %||% x
}

ggpairs_fn1 <- function(title, types, diag, ...) {
  ggpairs(
    tips,
    1:4,
    axisLabels = "show",
    title = paste(
      "upper = c(cont = ",
      gn(types$continuous),
      ", combo = ",
      gn(types$combo),
      ", discrete = ",
      gn(types$discrete),
      "); diag = c(cont = ",
      gn(diag$continuous),
      ", discrete = ",
      gn(diag$discrete),
      ")",
      sep = ""
    ),
    upper = types,
    lower = types,
    diag = diag,
    progress = FALSE,
    ...
  ) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 9))
}

ggpairs_fn2 <- function(...) {
  ggpairs_fn1(
    ...,
    mapping = ggplot2::aes(color = !!as.name("day")),
    legend = c(1, 3)
  )
}

ggduo_fn1 <- function(title, types, diag, ...) {
  types$comboHorizontal <- types$combo
  types$comboVertical <- types$combo
  types$combo <- NULL
  ggduo(
    tips,
    1:3,
    1:4,
    axisLabels = "show",
    title = paste(
      "types = c(cont = ",
      gn(types$continuous),
      ", combo = ",
      gn(types$comboHorizontal),
      ", discrete = ",
      gn(types$discrete),
      ")",
      sep = ""
    ),
    types = types,
    progress = FALSE,
    ...
  ) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 9))
}

ggduo_fn2 <- function(...) {
  ggduo_fn1(..., mapping = ggplot2::aes(color = .data$day), legend = 3) +
    theme(legend.position = "bottom")
}


# re ordered the subs so that density can have no binwidth param
conSubs <- list(
  "autopoint",
  "density",
  "points",
  "smooth",
  "smooth_lm",
  "smooth_loess",
  "cor",
  "blank"
)
comSubs <- list(
  "autopoint",
  "box",
  "dot",
  "box_no_facet",
  "dot_no_facet",
  wrap("facethist", binwidth = 1),
  "facetdensity",
  "facetdensitystrip",
  # "summarise_by", # Issues with grid printing
  wrap("denstrip", binwidth = 1),
  "blank"
)
disSubs <- list(
  "autopoint",
  "colbar",
  "count",
  "cross",
  "crosstable",
  "facetbar",
  "ratio",
  "rowbar",
  "table",
  # "trends", # Issues with grid printing
  "blank"
)

conDiagSubs <- c(
  "autopointDiag",
  "densityDiag",
  wrap("barDiag", binwidth = 1),
  "blankDiag"
)
disDiagSubs <- c(
  "autopointDiag",
  "barDiag",
  "countDiag",
  "tableDiag",
  "blankDiag"
)

# for (fn in list(ggpairs_fn1, ggpairs_fn2, ggduo_fn1, ggduo_fn2)) {
for (fn_info in list(
  list(fn = ggpairs_fn1, title = "ggpairs"),
  list(fn = ggpairs_fn2, title = "ggpairs_color"),
  list(fn = ggduo_fn1, title = "ggduo"),
  list(fn = ggduo_fn2, title = "ggduo_color")
)) {
  fn <- fn_info$fn
  fn_name <- fn_info$title
  for (i in 1:max(c(
    length(conSubs),
    length(comSubs),
    length(disSubs),
    length(conDiagSubs),
    length(disDiagSubs)
  ))) {
    conSub <- if (i <= length(conSubs)) conSubs[[i]] else "blank"
    comSub <- if (i <= length(comSubs)) comSubs[[i]] else "blank"
    disSub <- if (i <= length(disSubs)) disSubs[[i]] else "blank"

    diagConSub <- if (i <= length(conDiagSubs)) {
      conDiagSubs[[i]]
    } else {
      "blankDiag"
    }
    diagDisSub <- if (i <= length(disDiagSubs)) {
      disDiagSubs[[i]]
    } else {
      "blankDiag"
    }

    type_name <- function(x) {
      if (is.function(x)) {
        sub("ggally_", "", attr(x, "name"))
      } else {
        x
      }
    }
    type_names <- vapply(
      c(conSub, comSub, disSub, diagConSub, diagDisSub),
      type_name,
      character(1)
    )
    if (all(grepl("blank", type_names))) {
      # vdiffr can't handle blank plots
      next
    }
    pm_name <- paste0(type_names, collapse = "-")
    pm_name <- paste0(fn_name, "-", pm_name)

    test_that(paste0("subtypes", "-", pm_name), {
      # print(list(
      #   fn_num = fn_num,
      #   types = list(
      #     continuous = conSub,
      #     combo = comSub,
      #     discrete = disSub
      #   ),
      #   diag = list(
      #     continuous = diagConSub,
      #     discrete = diagDisSub
      #   )
      # ))
      #
      expect_silent({
        pm <- fn(
          types = list(
            continuous = conSub,
            combo = comSub,
            discrete = disSub
          ),
          diag = list(
            continuous = diagConSub,
            discrete = diagDisSub
          )
        )
      })

      # tryCatch(
      #   {
      set.seed(123456) # keep jitter consistent
      suppressWarnings({
        built_pm <- ggmatrix_gtable(pm)
      })
      hashed_name <- cli::hash_xxhash64(pm_name)
      ggally_expect_doppelganger(hashed_name, built_pm)
      #   },
      #   error = function(e) {
      #     if (interactive()) {
      #       assign("barret", pm, envir = globalenv())
      #     }
      #     # Rethrow error
      #     signalCondition(e)
      #   }
      # )
    })
  }
}


test_that("bad types", {
  skip_on_cran()

  expect_snapshot(
    ggpairs(
      tips,
      1:2,
      lower = "blank",
      diag = "blank",
      upper = list(continuous = "BAD_TYPE")
    ),
    error = TRUE
  )
})

# pm <- ggpairs(tips, upper = "blank")
# # pm

#  # Custom Example
#  pm <- ggpairs(
#    tips[, c(1, 3, 4, 2)],
#    upper = list(continuous = "density", combo = "box"),
#    lower = list(continuous = "points", combo = "dot")
#  )
#  # pm

#  # Use sample of the diamonds data
#  data(diamonds, package = "ggplot2")
#  diamonds.samp <- diamonds[sample(1:dim(diamonds)[1], 200), ]

#  # Custom Example
#  pm <- ggpairs(
#   diamonds.samp[, 1:5],
#   upper = list(continuous = "density", combo = "box"),
#   lower = list(continuous = "points", combo = "dot"),
#   color = "cut",
#   alpha = 0.4,
#   title = "Diamonds"
#  )
#  # pm

#  # Will plot four "Incorrect Plots"
#  bad_plots <- ggpairs(
#    tips[, 1:3],
#    upper = list(continuous = "wrongType1", combo = "wrongType2"),
#    lower = list(continuous = "IDK1", combo = "IDK2", discrete = "mosaic"),
#  )
#  # bad_plots

#  # Only Variable Labels on the diagonal (no axis labels)
#  pm <- ggpairs(tips[, 1:3], axisLabels = "internal")
#  # pm
#  # Only Variable Labels on the outside (no axis labels)
#  pm <- ggpairs(tips[, 1:3], axisLabels = "none")
#  # pm

#  # Custom Examples
#  custom_car <- ggpairs(mtcars[, c("mpg", "wt", "cyl")], upper = "blank", title = "Custom Example")
# #' # ggplot example taken from example(geom_text)
# #'   plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg, label = rownames(mtcars)))
# #'   plot <- plot +
# #'     ggplot2::geom_text(ggplot2::aes(colour = factor(cyl)), size = 3) +
# #'     ggplot2::scale_colour_discrete(l = 40)
# #' custom_car <- putPlot(custom_car, plot, 1, 2)
# #' personal_plot <- ggally_text(
# #'   "ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"
# #' )
# #' custom_car <- putPlot(custom_car, personal_plot, 1, 3)
# #' # custom_car
