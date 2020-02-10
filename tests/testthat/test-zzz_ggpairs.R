
context("ggpairs")
data(tips, package = "reshape")

expect_print <- function(p) {
  testthat::expect_silent(print(p))
}

facethistBindwidth1 <- list(combo = wrap("facethist", binwidth = 1))
facethistBindwidth1Duo <- list(
  comboHorizontal = wrap("facethist", binwidth = 1),
  comboVertical = wrap("facethist", binwidth = 1)
)

test_that("structure", {
  expect_null <- function(x) {
    expect_true(is.null(x))
  }

  expect_obj <- function(x) {
    expect_is(x$data, "data.frame")
    expect_is(x$plots, "list")
    expect_equivalent(length(x$plots), ncol(tips) ^ 2)
    expect_null(x$title)
    expect_null(x$xlab)
    expect_null(x$ylab)
    expect_is(x$xAxisLabels, "character")
    expect_is(x$yAxisLabels, "character")
    expect_is(x$showXAxisPlotLabels, "logical")
    expect_is(x$showYAxisPlotLabels, "logical")
    expect_null(x$legend)
    expect_is(x$byrow, "logical")
    expect_null(x$gg)
    expect_true("gg" %in% names(x))
  }

  expect_obj(ggduo(tips))
  expect_obj(ggpairs(tips))


})

test_that("columns", {
  expect_obj <- function(pm, columnsX, columnsY) {
    expect_equivalent(length(pm$plots), length(columnsX) * length(columnsY))
    expect_equivalent(pm$xAxisLabels, columnsX)
    expect_equivalent(pm$yAxisLabels, columnsY)
    expect_equivalent(pm$ncol, length(columnsX))
    expect_equivalent(pm$nrow, length(columnsY))
  }

  columnsUsed <- c("total_bill", "tip", "sex")
  pm <- ggpairs(tips, columns = columnsUsed)
  expect_obj(pm, columnsUsed, columnsUsed)

  columnsX <- c("total_bill", "tip", "sex")
  columnsY <- c("smoker", "day", "time", "size")
  pm <- ggduo(tips, columnsX, columnsY)
  expect_obj(pm, columnsX, columnsY)

})

test_that("column labels", {
  expect_obj <- function(pm, columnLabelsX, columnLabelsY) {
    expect_equivalent(pm$xAxisLabels, columnLabelsX)
    expect_equivalent(pm$yAxisLabels, columnLabelsY)
  }

  columnTitles <- c("A", "B", "C")
  pm <- ggpairs(tips, 1:3, columnLabels = columnTitles)
  expect_obj(pm, columnTitles, columnTitles)

  columnTitles <- c("Total Bill %", "Tip 123456", "Sex ( /a asdf)")
  pm <- ggpairs(tips, 1:3, columnLabels = columnTitles)
  expect_obj(pm, columnTitles, columnTitles)

  columnLabelsX <- c("Total Bill %", "Tip 123456", "Sex ( /a asdf)")
  columnLabelsY <- c("Smoker !#@", "Day 678", "1", "NULL")
  pm <- ggduo(tips, 1:3, 4:7, columnLabelsX = columnLabelsX, columnLabelsY = columnLabelsY)
  expect_obj(pm, columnLabelsX, columnLabelsY)

})

test_that("character", {
  expect_obj <- function(pm) {
    expect_true(is.factor(pm$data$sex))
    expect_true(is.factor(pm$data$smoker))
  }

  tips2 <- tips
  tips2$sex <- as.character(tips2$sex)
  tips2$smoker <- as.character(tips2$smoker)

  expect_obj(ggpairs(tips2))
  expect_obj(ggduo(tips2))
})


test_that("upper/lower/diag = blank", {
  columnsUsed <- 1:3
  au <- ggpairs(tips, columnsUsed, upper = "blank")
  ad <- ggpairs(tips, columnsUsed, diag = "blank")
  al <- ggpairs(tips, columnsUsed, lower = "blank")
  for (i in 1:3) {
    for (j in 1:3) {
      if (i < j) {
        expect_true(  is_blank_plot(au[i, j]))
        expect_false( is_blank_plot(ad[i, j]))
        expect_false( is_blank_plot(al[i, j]))
      }
      if (i > j) {
        expect_false( is_blank_plot(au[i, j]))
        expect_false( is_blank_plot(ad[i, j]))
        expect_true(  is_blank_plot(al[i, j]))
      }
      if (i == j) {
        expect_false( is_blank_plot(au[i, j]))
        expect_true(  is_blank_plot(ad[i, j]))
        expect_false( is_blank_plot(al[i, j]))
      }
    }
  }

  a <- ggpairs(tips, columnsUsed)
  a[1, 1] <- ggplot2::qplot(total_bill, data = tips)
  expect_false(is_blank_plot(a[1, 1]))

})

test_that("stops", {

  expect_warning({
    pm <- ggpairs(tips, axisLabels = "not_a_chosen", lower = facethistBindwidth1)
  }, "'axisLabels' not in ") # nolint
  expect_warning({
    pm <- ggduo(tips, axisLabels = "not_a_chosen", types = facethistBindwidth1Duo)
  }, "'axisLabels' not in ") # nolint

  expect_warning({
    pm <- ggpairs(tips, color = "sex")
  }, "Extra arguments: ") # nolint

  expect_warning({
    pm <- ggduo(tips, 2:3, 2:3, types = list(combo = "facetdensity"))
  }, "Setting:\n\ttypes") # nolint

  expect_error({
    ggpairs(tips, columns = c("tip", "day", "not in tips"))
  }, "Columns in 'columns' not found in data") # nolint
  expect_error({
    ggduo(tips, columnsX = c("tip", "day", "not in tips"), columnsY = "smoker")
  }, "Columns in 'columnsX' not found in data") # nolint
  expect_error({
    ggduo(tips, columnsX = c("tip", "day", "smoker"), columnsY = "not in tips")
  }, "Columns in 'columnsY' not found in data") # nolint

  expect_warning({
    pm <- ggpairs(tips, legends = TRUE)
  }, "'legends' will be deprecated") # nolint

  expect_error({
    ggpairs(tips, params = c(size = 2))
  }, "'params' is a deprecated") # nolint

  expect_error( {
    ggpairs(tips, columns = 1:10)
  }, "Make sure your numeric 'columns' values are less than or equal to") # nolint
  expect_error( {
    ggduo(tips, columnsX = 1:10)
  }, "Make sure your numeric 'columnsX' values are less than or equal to") # nolint
  expect_error( {
    ggduo(tips, columnsY = 1:10)
  }, "Make sure your numeric 'columnsY' values are less than or equal to") # nolint

  expect_error({
    ggpairs(tips, columns = -5:5)
  }, "Make sure your numeric 'columns' values are positive") # nolint
  expect_error({
    ggduo(tips, columnsX = -5:5)
  }, "Make sure your numeric 'columnsX' values are positive") # nolint
  expect_error({
    ggduo(tips, columnsY = -5:5)
  }, "Make sure your numeric 'columnsY' values are positive") # nolint

  expect_error({
    ggpairs(tips, columns = (2:10) / 2)
  }, "Make sure your numeric 'columns' values are integers") # nolint
  expect_error({
    ggduo(tips, columnsX = (2:10) / 2)
  }, "Make sure your numeric 'columnsX' values are integers") # nolint
  expect_error({
    ggduo(tips, columnsY = (2:10) / 2)
  }, "Make sure your numeric 'columnsY' values are integers") # nolint

  expect_error({
    ggpairs(tips, columns = 1:3, columnLabels = c("A", "B", "C", "Extra"))
  }, "The length of the 'columnLabels' does not match the length of the 'columns'") # nolint
  expect_error({
    ggduo(tips, columnsX = 1:3, columnLabelsX = c("A", "B", "C", "Extra"))
  }, "The length of the 'columnLabelsX' does not match the length of the 'columnsX'") # nolint
  expect_error({
    ggduo(tips, columnsY = 1:3, columnLabelsY = c("A", "B", "C", "Extra"))
  }, "The length of the 'columnLabelsY' does not match the length of the 'columnsY'") # nolint

  expect_error({
    ggpairs(tips, upper = c("not_a_list"))
  }, "'upper' is not a list") # nolint
  expect_error({
    ggpairs(tips, diag = c("not_a_list"))
  }, "'diag' is not a list") # nolint
  expect_error({
    ggpairs(tips, lower = c("not_a_list"))
  }, "'lower' is not a list") # nolint
  expect_error({
    ggduo(tips, types = c("not_a_list"))
  }, "'types' is not a list") # nolint

  # # couldn't get correct error message
  # #  variables: 'colour' have non standard format: 'total_bill + tip'.
  # expect_error({
  #   ggpairs(tips, mapping = ggplot2::aes(color = total_bill + tip))
  # }, "variables\\: \"colour\" have non standard format") # nolint
  # expect_error({
  #   ggduo(tips, mapping = ggplot2::aes(color = total_bill + tip))
  # }, "variables\\: \"colour\" have non standard format") # nolint

  errorString <- "'aes_string' is a deprecated element"
  expect_error({
    ggpairs(tips, upper = list(aes_string = ggplot2::aes(color = day)))
  }, errorString) # nolint
  expect_error({
    ggpairs(tips, lower = list(aes_string = ggplot2::aes(color = day)))
  }, errorString) # nolint
  expect_error({
    ggpairs(tips, diag = list(aes_string = ggplot2::aes(color = day)))
  }, errorString) # nolint
  expect_error({
    ggduo(tips, types = list(aes_string = ggplot2::aes(color = day)))
  }, errorString) # nolint


  expect_diag_warn <- function(key, value) {
    warnString <- str_c("Changing diag\\$", key, " from '", value, "' to '", value, "Diag'")
    diagObj <- list()
    diagObj[[key]] <- value
    expect_warning({
        pm <- ggpairs(tips, diag = diagObj)
      },
      warnString
    )
  }
  # diag
  #   continuous
  #     densityDiag
  #     barDiag
  #     blankDiag
  #   discrete
  #     barDiag
  #     blankDiag
  expect_diag_warn("continuous", "density")
  expect_diag_warn("continuous", "bar")
  expect_diag_warn("continuous", "blank")
  expect_diag_warn("discrete", "bar")
  expect_diag_warn("discrete", "blank")

})


test_that("cardinality", {
  expect_silent(stop_if_high_cardinality(tips, 1:ncol(tips), NULL))
  expect_silent(stop_if_high_cardinality(tips, 1:ncol(tips), FALSE))
  expect_error(
    stop_if_high_cardinality(tips, 1:ncol(tips), "not numeric"),
    "'cardinality_threshold' should"
  )
  expect_error(
    stop_if_high_cardinality(tips, 1:ncol(tips), 2),
    "Column 'day' has more levels"
  )
})

test_that("blank types", {
  columnsUsed <- 1:3
  pmUpper <- ggpairs(tips, columnsUsed, upper = "blank", lower = facethistBindwidth1)
  pmDiag <- ggpairs(tips, columnsUsed, diag = "blank", lower = facethistBindwidth1)
  pmLower <- ggpairs(tips, columnsUsed, lower = "blank")

  for (i in columnsUsed) {
    for (j in columnsUsed) {
      if (i < j) {
        # upper
        expect_true(is_blank_plot(pmUpper[i, j]))
        expect_false(is_blank_plot(pmDiag[i, j]))
        expect_false(is_blank_plot(pmLower[i, j]))

      } else if ( i > j) {
        # lower
        expect_false(is_blank_plot(pmUpper[i, j]))
        expect_false(is_blank_plot(pmDiag[i, j]))
        expect_true(is_blank_plot(pmLower[i, j]))

      } else {
        # diag
        expect_false(is_blank_plot(pmUpper[i, j]))
        expect_true(is_blank_plot(pmDiag[i, j]))
        expect_false(is_blank_plot(pmLower[i, j]))

      }
    }
  }

  columnsUsedX <- 1:3
  columnsUsedY <- 4:5
  pmDuo <- ggduo(tips, columnsUsedX, columnsUsedY, types = "blank")
  for (i in seq_along(columnsUsedX)) {
    for (j in seq_along(columnsUsedY)) {
      expect_true(is_blank_plot(pmDuo[j, i]))
    }
  }
})

test_that("axisLabels", {
  expect_obj <- function(pm, axisLabel) {
    expect_true(is.null(pm$showStrips))
    if (axisLabel == "show") {
      expect_true(pm$showXAxisPlotLabels)
      expect_true(pm$showYAxisPlotLabels)
      expect_false(is.null(pm$xAxisLabels))
      expect_false(is.null(pm$yAxisLabels))
    } else if (axisLabel == "internal") {
      for (i in 1:(pm$ncol)) {
        p <- pm[i, i]
        expect_true(inherits(p$layers[[1]]$geom, "GeomText"))
        expect_true(inherits(p$layers[[2]]$geom, "GeomText"))
        expect_equal(length(p$layers), 2)
      }
      expect_false(pm$showXAxisPlotLabels)
      expect_false(pm$showYAxisPlotLabels)
      expect_true(is.null(pm$xAxisLabels))
      expect_true(is.null(pm$yAxisLabels))
    } else if (axisLabel == "none") {
      expect_false(pm$showXAxisPlotLabels)
      expect_false(pm$showYAxisPlotLabels)
      expect_false(is.null(pm$xAxisLabels))
      expect_false(is.null(pm$yAxisLabels))
    }
    expect_print(pm)
  }

  fn <- function(axisLabels) {
    pm <- ggpairs(
      iris, c(3, 4, 5, 1),
      upper = "blank",
      lower = facethistBindwidth1,
      axisLabels = axisLabels,
      title = str_c("axisLabels = ", axisLabels),
      progress = FALSE
    )
    pm
  }
  for (axisLabels in c("show", "internal", "none")) {
    expect_obj(fn(axisLabels), axisLabels)
  }

  plots <- ggpairs(iris, 1:3)$plots
  for (val in c(TRUE, FALSE)) {
    pm <- ggmatrix(
      plots, 3, 3,
      showAxisPlotLabels = val
    )
    expect_equal(pm$showXAxisPlotLabels, val)
    expect_equal(pm$showYAxisPlotLabels, val)
  }


  fn <- function(axisLabels) {
    a <- ggduo(
      iris, c(4, 5), c(5, 1),
      types = facethistBindwidth1Duo,
      axisLabels = axisLabels,
      title = str_c("axisLabels = ", axisLabels)
    )
    a
  }
  for (axisLabels in c("show", "none")) {
    expect_obj(fn(axisLabels), axisLabels)
  }

})


test_that("strips and axis", {

  # axis should line up with left side strips
  pm <- ggpairs(
    tips, c(3, 1, 4),
    showStrips = TRUE,
    title = "Axis should line up even if strips are present",
    lower = list(combo = wrap("facethist", binwidth = 1))
  )
  expect_print(pm)
  # default behavior. tested in other places
  # expect_silent({
  #   pm <- ggpairs(tips, c(3, 1, 4), showStrips = FALSE)
  #   print(pm)
  # })
})


test_that("dates", {
  startDt <- as.POSIXct("2000-01-01", tz = "UTC")
  endDt   <- as.POSIXct("2000-04-01", tz = "UTC")

  dts <- seq(startDt, endDt, 86400) # 86400 = as.numeric(ddays(1))
  x <- data.frame(
    date = dts,
    x1 = rnorm(length(dts)),
    x2 = rnorm(length(dts)),
    cat = sample(c("a", "b", "c"), length(dts), replace = TRUE)
  )

  class(x) <- c("NOT_data.frame", "data.frame")

  a <- ggpairs(
    x, c(2, 1, 4, 3),
    mapping = ggplot2::aes(color = cat),
    lower = "blank",
    diag = list(continuous = "densityDiag"),
    upper = list(continuous = "cor")
  )
  p <- a[1, 2]
  expect_true(inherits(p$layers[[1]]$geom, "GeomText"))
  expect_true(inherits(p$layers[[2]]$geom, "GeomText"))
  expect_equal(length(p$layers), 2)


  a <- ggpairs(
    x, c(2, 1, 4, 3),
    mapping = ggplot2::aes(color = cat),
    lower = "blank",
    diag = list(continuous = "barDiag"),
    upper = list(continuous = "cor")
  )
  p <- a[1, 1]
  expect_true(inherits(p$layers[[1]]$geom, "GeomBar"))
  expect_equal(length(p$layers), 1)


})


test_that("mapping", {
  pm <- ggpairs(tips, mapping = 1:3)
  expect_equal(pm$xAxisLabels, names(tips)[1:3])

  pm <- ggpairs(tips, columns = 1:3)
  expect_equal(pm$xAxisLabels, names(tips)[1:3])

  expect_error({
    ggpairs(tips, columns = 1:3, mapping = 1:3)
  }, "'mapping' should not be numeric") # nolint
})

test_that("user functions", {

  p0 <- ggally_points(tips, ggplot2::aes(x = total_bill, y = tip))

  pm1 <- ggpairs(tips, 1:2, lower = list(continuous = "points"))
  p1 <- pm1[2, 1]
  expect_equivalent(p0, p1)

  pm2 <- ggpairs(tips, 1:2, lower = list(continuous = ggally_points))
  p2 <- pm2[2, 1]
  expect_equivalent(p0, p2)
})

test_that("NA data", {
  expect_is_na_plot <- function(p) {
    expect_true(identical(as.character(p$data$label), "NA"))
    expect_true(inherits(p$layers[[1]]$geom, "GeomText"))
    expect_equivalent(length(p$layers), 1)
  }
  expect_not_na_plot <- function(p) {
    expect_false(identical(as.character(p$data$label), "NA"))
  }
  expect_is_blank <- function(p) {
    expect_true(is_blank_plot(p))
  }

  dd <- data.frame(x = c(1:5, rep(NA, 5)), y = c(rep(NA, 5), 2:6), z = 1:10, w = NA)
  pm <- ggpairs(dd)

  test_pm <- function(pm, na_mat) {
    for (i in 1:4) {
      for (j in 1:4) {
        if (na_mat[i, j]) {
          expect_is_na_plot(pm[i, j])
        } else {
          if (j == 3 & i < 3) {
            expect_warning({
                p <- pm[i, j]
              },
              "Removed 5 rows"
            )
          } else {
            p <- pm[i, j]
          }
          expect_not_na_plot(p)
        }
      }
    }
  }

  na_mat <- matrix(FALSE, ncol = 4, nrow = 4)
  na_mat[1, 2] <- TRUE
  na_mat[2, 1] <- TRUE
  na_mat[1:4, 4] <- TRUE
  na_mat[4, 1:4] <- TRUE
  test_pm(pm, na_mat)

})

test_that("strip-top and strip-right", {


  data(tips, package = "reshape")

  double_strips <- function(data, mapping, ...) {
    dt <- count(data, c(mapping_string(mapping$x), mapping_string(mapping$y)))
    ggplot2::qplot(
      xmin = 0.25, xmax = 0.75,
      ymin = 1, ymax = freq,
      data = dt, geom = "rect"
    ) +
      ggplot2::facet_grid(paste0(mapping_string(mapping$y), " ~ ", mapping_string(mapping$x))) +
      ggplot2::scale_x_continuous(breaks = 0.5, labels = NULL)
  }

  pm <- ggpairs(
    tips, 3:6,
    lower = "blank", diag = "blank",
    upper = list(discrete = double_strips),
    progress = FALSE
  )
  expect_print(pm)
  pm <- ggpairs(
    tips, 3:6,
    lower = "blank", diag = "blank",
    upper = list(discrete = double_strips),
    showStrips = TRUE,
    progress = FALSE
  )
  expect_print(pm)

})


test_that("subtypes", {

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
    ifnull(fnName, x)
  }

  ggpairs_fn1 <- function(title, types, diag, ...) {
    ggpairs(
      tips, 1:4,
      axisLabels = "show",
      title = paste(
        "upper = c(cont = ", gn(types$continuous),
          ", combo = ", gn(types$combo),
          ", discrete = ", gn(types$discrete),
        "); diag = c(cont = ", gn(diag$continuous),
          ", discrete = ", gn(diag$discrete),
        ")", sep = ""),
      upper = types,
      lower = types,
      diag = diag,
      progress = FALSE,
      ...
    ) + ggplot2::theme(plot.title = ggplot2::element_text(size = 9))
  }

  ggpairs_fn2 <- function(...) {
    ggpairs_fn1(..., mapping = ggplot2::aes(color = day), legend = c(1, 3))
  }

  ggduo_fn1 <- function(title, types, diag, ...) {
    types$comboHorizontal <- types$combo
    types$comboVertical <- types$combo
    types$combo <- NULL
    ggduo(
      tips, 1:3, 1:4,
      axisLabels = "show",
      title = paste(
        "types = c(cont = ", gn(types$continuous),
          ", combo = ", gn(types$comboHorizontal),
          ", discrete = ", gn(types$discrete),
        ")", sep = ""),
      types = types,
      progress = FALSE,
      ...
    ) + ggplot2::theme(plot.title = ggplot2::element_text(size = 9))
  }

  ggduo_fn2 <- function(...) {
    ggduo_fn1(..., mapping = ggplot2::aes(color = day), legend = 3) +
      theme(legend.position = "bottom")
  }

  # re ordered the subs so that density can have no binwidth param
  conSubs <- list("density", "points", "smooth", "smooth_loess", "cor", "blank")
  comSubs <- list(
    "box", "dot", "box_no_facet", "dot_no_facet",
    wrap("facethist", binwidth = 1),
    "facetdensity", wrap("denstrip", binwidth = 1),
    "blank"
  )
  disSubs <- list("ratio", "facetbar", "blank")

  conDiagSubs <- c("densityDiag", wrap("barDiag", binwidth = 1), "blankDiag")
  disDiagSubs <- c("barDiag", "blankDiag")

  # for (fn in list(ggpairs_fn1, ggpairs_fn2, ggduo_fn1, ggduo_fn2)) {
  for (fn_num in 1:4) {
    fn <- list(ggpairs_fn1, ggpairs_fn2, ggduo_fn1, ggduo_fn2)[[fn_num]]
    for (i in 1:6) {
      conSub <- if (i <= length(conSubs)) conSubs[[i]] else "blank"
      comSub <- if (i <= length(comSubs)) comSubs[[i]] else "blank"
      disSub <- if (i <= length(disSubs)) disSubs[[i]] else "blank"

      diagConSub <- if (i <= length(conDiagSubs)) conDiagSubs[[i]] else "blankDiag"
      diagDisSub <- if (i <= length(disDiagSubs)) disDiagSubs[[i]] else "blankDiag"

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

      if (grepl("/Users/barret/", getwd(), fixed = TRUE)) {
        # only if on personal machine, do viz test
        expect_print(pm)
      }
    }
  }

  expect_error({
    ggpairs(tips, 1:2, lower = "blank", diag = "blank", upper = list(continuous = "BAD_TYPE"))
  })

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
#  data(diamonds, package="ggplot2")
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
#  pm <- ggpairs(tips[, 1:3], axisLabels="internal")
#  # pm
#  # Only Variable Labels on the outside (no axis labels)
#  pm <- ggpairs(tips[, 1:3], axisLabels="none")
#  # pm

#  # Custom Examples
#  custom_car <- ggpairs(mtcars[, c("mpg", "wt", "cyl")], upper = "blank", title = "Custom Example")
# #' # ggplot example taken from example(geom_text)
# #'   plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x=wt, y=mpg, label=rownames(mtcars)))
# #'   plot <- plot +
# #'     ggplot2::geom_text(ggplot2::aes(colour=factor(cyl)), size = 3) +
# #'     ggplot2::scale_colour_discrete(l=40)
# #' custom_car <- putPlot(custom_car, plot, 1, 2)
# #' personal_plot <- ggally_text(
# #'   "ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"
# #' )
# #' custom_car <- putPlot(custom_car, personal_plot, 1, 3)
# #' # custom_car
