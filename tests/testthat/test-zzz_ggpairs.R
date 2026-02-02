# This file takes too long
testthat::skip_on_cran()

data(tips)

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
    expect_s3_class(x$data, "data.frame")
    expect_type(x$plots, "list")
    expect_equal(length(x$plots), ncol(tips)^2)
    expect_null(x$title)
    expect_null(x$xlab)
    expect_null(x$ylab)
    expect_type(x$xAxisLabels, "character")
    expect_type(x$yAxisLabels, "character")
    expect_type(x$showXAxisPlotLabels, "logical")
    expect_type(x$showYAxisPlotLabels, "logical")
    expect_null(x$legend)
    expect_type(x$byrow, "logical")
    expect_null(x$gg)
  }

  expect_obj(ggduo(tips))
  expect_obj(ggpairs(tips))
})

test_that("columns", {
  expect_obj <- function(pm, columnsX, columnsY) {
    expect_equal(length(pm$plots), length(columnsX) * length(columnsY))
    expect_equal(pm$xAxisLabels, columnsX)
    expect_equal(pm$yAxisLabels, columnsY)
    expect_equal(pm$ncol, length(columnsX))
    expect_equal(pm$nrow, length(columnsY))
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
    expect_equal(pm$xAxisLabels, columnLabelsX)
    expect_equal(pm$yAxisLabels, columnLabelsY)
  }

  columnTitles <- c("A", "B", "C")
  pm <- ggpairs(tips, 1:3, columnLabels = columnTitles)
  expect_obj(pm, columnTitles, columnTitles)

  columnTitles <- c("Total Bill %", "Tip 123456", "Sex ( /a asdf)")
  pm <- ggpairs(tips, 1:3, columnLabels = columnTitles)
  expect_obj(pm, columnTitles, columnTitles)

  columnLabelsX <- c("Total Bill %", "Tip 123456", "Sex ( /a asdf)")
  columnLabelsY <- c("Smoker !#@", "Day 678", "1", "NULL")
  pm <- ggduo(
    tips,
    1:3,
    4:7,
    columnLabelsX = columnLabelsX,
    columnLabelsY = columnLabelsY
  )
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
        expect_true(is_blank_plot(au[i, j]))
        expect_false(is_blank_plot(ad[i, j]))
        expect_false(is_blank_plot(al[i, j]))
      }
      if (i > j) {
        expect_false(is_blank_plot(au[i, j]))
        expect_false(is_blank_plot(ad[i, j]))
        expect_true(is_blank_plot(al[i, j]))
      }
      if (i == j) {
        expect_false(is_blank_plot(au[i, j]))
        expect_true(is_blank_plot(ad[i, j]))
        expect_false(is_blank_plot(al[i, j]))
      }
    }
  }

  a <- ggpairs(tips, columnsUsed)
  a[1, 1] <- ggplot(tips, aes(total_bill)) +
    geom_histogram()
  expect_false(is_blank_plot(a[1, 1]))
})

test_that("stops", {
  expect_warning(
    {
      pm <- ggpairs(
        tips,
        axisLabels = "not_a_chosen",
        lower = facethistBindwidth1
      )
    },
    "`axisLabels` not in "
  )
  expect_warning(
    {
      pm <- ggduo(
        tips,
        axisLabels = "not_a_chosen",
        types = facethistBindwidth1Duo
      )
    },
    "`axisLabels` not in "
  )

  lifecycle::expect_deprecated(
    {
      pm <- ggpairs(tips, color = "sex")
    },
  )

  expect_warning(
    {
      pm <- ggduo(tips, 2:3, 2:3, types = list(combo = "facetdensity"))
    },
    "Setting:\n"
  )

  expect_snapshot(
    ggpairs(tips, columns = c("tip", "day", "not in tips")),
    error = TRUE
  )
  expect_snapshot(
    ggduo(tips, columnsX = c("tip", "day", "not in tips"), columnsY = "smoker"),
    error = TRUE
  )
  expect_snapshot(
    ggduo(tips, columnsX = c("tip", "day", "smoker"), columnsY = "not in tips"),
    error = TRUE
  )

  lifecycle::expect_deprecated(
    {
      pm <- ggpairs(tips, legends = TRUE)
    }
  )

  lifecycle::expect_deprecated(
    {
      ggpairs(tips, params = c(size = 2))
    }
  )

  expect_snapshot(ggpairs(tips, columns = 1:10), error = TRUE)
  expect_snapshot(ggduo(tips, columnsX = 1:10), error = TRUE)
  expect_snapshot(ggduo(tips, columnsY = 1:10), error = TRUE)

  expect_snapshot(ggpairs(tips, columns = -5:5), error = TRUE)
  expect_snapshot(ggduo(tips, columnsX = -5:5), error = TRUE)
  expect_snapshot(ggduo(tips, columnsY = -5:5), error = TRUE)

  expect_snapshot(ggpairs(tips, columns = (2:10) / 2), error = TRUE)
  expect_snapshot(ggduo(tips, columnsX = (2:10) / 2), error = TRUE)
  expect_snapshot(ggduo(tips, columnsY = (2:10) / 2), error = TRUE)

  expect_snapshot(
    ggpairs(tips, columns = 1:3, columnLabels = c("A", "B", "C", "Extra")),
    error = TRUE
  )
  expect_snapshot(
    ggduo(tips, columnsX = 1:3, columnLabelsX = c("A", "B", "C", "Extra")),
    error = TRUE
  )
  expect_snapshot(
    ggduo(tips, columnsY = 1:3, columnLabelsY = c("A", "B", "C", "Extra")),
    error = TRUE
  )

  expect_snapshot(ggpairs(tips, upper = c("not_a_list")), error = TRUE)
  expect_snapshot(ggpairs(tips, diag = c("not_a_list")), error = TRUE)
  expect_snapshot(ggpairs(tips, lower = c("not_a_list")), error = TRUE)
  expect_snapshot(ggduo(tips, types = c("not_a_list")), error = TRUE)

  # # couldn't get correct error message
  # #  variables: 'colour' have non standard format: 'total_bill + tip'.
  # expect_error({
  #   ggpairs(tips, mapping = ggplot2::aes(color = total_bill + tip))
  # }, "variables\\: \"colour\" have non standard format")
  # expect_error({
  #   ggduo(tips, mapping = ggplot2::aes(color = total_bill + tip))
  # }, "variables\\: \"colour\" have non standard format")

  expect_snapshot(
    ggpairs(tips, upper = list(aes_string = ggplot2::aes(color = .data$day))),
    error = TRUE
  )
  expect_snapshot(
    ggpairs(tips, lower = list(aes_string = ggplot2::aes(color = .data$day))),
    error = TRUE
  )
  expect_snapshot(
    ggpairs(tips, diag = list(aes_string = ggplot2::aes(color = .data$day))),
    error = TRUE
  )
  expect_snapshot(
    ggduo(tips, types = list(aes_string = ggplot2::aes(color = .data$day))),
    error = TRUE
  )

  expect_diag_warn <- function(key, value) {
    warnString <- sprintf("Changing `diag\\$%s` from", key)
    diagObj <- list()
    diagObj[[key]] <- value
    expect_warning(
      {
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
  expect_snapshot(
    stop_if_high_cardinality(tips, 1:ncol(tips), "not numeric"),
    error = TRUE
  )
  expect_snapshot(
    stop_if_high_cardinality(tips, 1:ncol(tips), 2),
    error = TRUE
  )
})

test_that("blank types", {
  columnsUsed <- 1:3
  pmUpper <- ggpairs(
    tips,
    columnsUsed,
    upper = "blank",
    lower = facethistBindwidth1
  )
  pmDiag <- ggpairs(
    tips,
    columnsUsed,
    diag = "blank",
    lower = facethistBindwidth1
  )
  pmLower <- ggpairs(tips, columnsUsed, lower = "blank")

  for (i in columnsUsed) {
    for (j in columnsUsed) {
      if (i < j) {
        # upper
        expect_true(is_blank_plot(pmUpper[i, j]))
        expect_false(is_blank_plot(pmDiag[i, j]))
        expect_false(is_blank_plot(pmLower[i, j]))
      } else if (i > j) {
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
  expect_axis_labels <- function(pm, prefix, axisLabel) {
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
    ggally_expect_doppelganger(
      paste0("axisLabels-", prefix, "-", axisLabel),
      pm
    )
  }

  fn <- function(axisLabels) {
    pm <- ggpairs(
      iris,
      c(3, 4, 5, 1),
      upper = "blank",
      lower = facethistBindwidth1,
      axisLabels = axisLabels,
      title = str_c("axisLabels = ", axisLabels),
      progress = FALSE
    )
    pm
  }
  for (axisLabels in c("show", "internal", "none")) {
    expect_axis_labels(fn(axisLabels), "ggpairs", axisLabels)
  }

  plots <- ggpairs(iris, 1:3)$plots
  for (val in c(TRUE, FALSE)) {
    pm <- ggmatrix(
      plots,
      3,
      3,
      showAxisPlotLabels = val
    )
    expect_equal(pm$showXAxisPlotLabels, val)
    expect_equal(pm$showYAxisPlotLabels, val)
  }

  fn <- function(axisLabels) {
    a <- ggduo(
      iris,
      c(4, 5),
      c(5, 1),
      types = facethistBindwidth1Duo,
      axisLabels = axisLabels,
      title = str_c("axisLabels = ", axisLabels)
    )
    a
  }
  for (axisLabels in c("show", "none")) {
    expect_axis_labels(fn(axisLabels), "ggduo", axisLabels)
  }
})


test_that("strips and axis", {
  # axis should line up with left side strips
  pm <- ggpairs(
    tips,
    c(3, 1, 4),
    showStrips = TRUE,
    title = "Axis should line up even if strips are present",
    lower = list(combo = wrap("facethist", binwidth = 1))
  )
  ggally_expect_doppelganger("show-strips", pm)
  # default behavior. tested in other places
  # expect_silent({
  #   pm <- ggpairs(tips, c(3, 1, 4), showStrips = FALSE)
  #   print(pm)
  # })
})


test_that("dates", {
  startDt <- as.POSIXct("2000-01-01", tz = "UTC")
  endDt <- as.POSIXct("2000-04-01", tz = "UTC")

  dts <- seq(startDt, endDt, 86400) # 86400 = as.numeric(ddays(1))
  x <- data.frame(
    date = dts,
    x1 = rnorm(length(dts)),
    x2 = rnorm(length(dts)),
    cat = sample(c("a", "b", "c"), length(dts), replace = TRUE)
  )

  class(x) <- c("NOT_data.frame", "data.frame")

  a <- ggpairs(
    x,
    c(2, 1, 4, 3),
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
    x,
    c(2, 1, 4, 3),
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

  expect_snapshot(ggpairs(tips, columns = 1:3, mapping = 1:3), error = TRUE)
})

test_that("user functions", {
  p0 <- ggally_points(tips, ggplot2::aes(x = total_bill, y = tip))

  pm1 <- ggpairs(tips, 1:2, lower = list(continuous = "points"))
  p1 <- pm1[2, 1]

  pm2 <- ggpairs(tips, 1:2, lower = list(continuous = ggally_points))
  p2 <- pm2[2, 1]

  expect_equal_plots <- function(x, y) {
    expect_equal(length(x$layers), 1)
    expect_equal(length(y$layers), 1)
    expect_true(
      "GeomPoint" %in% class(x$layers[[1]]$geom)
    )
    expect_true(
      "GeomPoint" %in% class(y$layers[[1]]$geom)
    )

    if (packageVersion("ggplot2") > "3.5.2") {
      x_built <- ggplot2::ggplot_build(x)
      y_built <- ggplot2::ggplot_build(y)
      expect_equal(
        x_built@plot@labels[c("x", "y")],
        list(x = "total_bill", y = "tip")
      )
      expect_equal(x_built@plot@labels, y_built@plot@labels)
    } else {
      expect_equal(x$labels, list(x = "total_bill", y = "tip"))
      expect_equal(x$labels, y$labels)
    }
  }
  expect_equal_plots(p0, p1)
  expect_equal_plots(p0, p2)
})

test_that("NA data", {
  expect_is_na_plot <- function(p) {
    expect_true(identical(as.character(p$data$label), "NA"))
    expect_true(inherits(p$layers[[1]]$geom, "GeomText"))
    expect_equal(length(p$layers), 1)
  }
  expect_not_na_plot <- function(p) {
    expect_false(identical(as.character(p$data$label), "NA"))
  }
  expect_is_blank <- function(p) {
    expect_true(is_blank_plot(p))
  }

  dd <- data.frame(
    x = c(1:5, rep(NA, 5)),
    y = c(rep(NA, 5), 2:6),
    z = 1:10,
    w = NA
  )
  pm <- ggpairs(dd)

  test_pm <- function(pm, na_mat) {
    for (i in 1:4) {
      for (j in 1:4) {
        if (na_mat[i, j]) {
          expect_is_na_plot(pm[i, j])
        } else {
          if (j == 3 && i < 3) {
            expect_warning(
              {
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
  data(tips)

  double_strips <- function(data, mapping, ...) {
    dt <- dplyr::count(
      data,
      .data[[mapping_string(mapping$x)]],
      .data[[mapping_string(mapping$y)]],
      name = "freq"
    )
    ggplot(dt, aes(xmin = 0.25, xmax = 0.75, ymin = 1, ymax = freq)) +
      geom_rect() +
      ggplot2::facet_grid(paste0(
        mapping_string(mapping$y),
        " ~ ",
        mapping_string(mapping$x)
      )) +
      ggplot2::scale_x_continuous(breaks = 0.5, labels = NULL)
  }

  pm <- ggpairs(
    tips,
    3:6,
    lower = "blank",
    diag = "blank",
    upper = list(discrete = double_strips),
    progress = FALSE
  )
  ggally_expect_doppelganger("nested-strips-default", pm)
  pm <- ggpairs(
    tips,
    3:6,
    lower = "blank",
    diag = "blank",
    upper = list(discrete = double_strips),
    showStrips = TRUE,
    progress = FALSE
  )
  ggally_expect_doppelganger("nested-strips-true", pm)
})
