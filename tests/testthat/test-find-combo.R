test_that("plotting_data_type()", {
  expect_equal(plotting_data_type(c(NA, NA)), "na")
  expect_equal(plotting_data_type(1:3), "continuous")
  expect_equal(plotting_data_type(Sys.Date() + 0:2), "continuous")
  expect_equal(plotting_data_type(factor(letters[1:3])), "discrete")
  expect_equal(plotting_data_type(letters[1:3]), "discrete")
  expect_equal(plotting_data_type(c(TRUE, FALSE)), "discrete")
})

test_that("find_plot_type()", {
  fpt <- function(a, b, t1, t2, isAllNa = FALSE, allowDiag = TRUE) {
    find_plot_type(a, b, t1, t2, isAllNa = isAllNa, allowDiag = allowDiag)
  }
  # diagonal
  expect_equal(fpt("x", "x", "na", "na"), "na-diag")
  expect_equal(fpt("x", "x", "continuous", "continuous"), "continuous-diag")
  expect_equal(fpt("x", "x", "discrete", "discrete"), "discrete-diag")
  # same column but diag not allowed
  expect_equal(
    fpt("x", "x", "continuous", "continuous", allowDiag = FALSE),
    "continuous"
  )
  # off diagonal
  expect_equal(fpt("x", "y", "na", "continuous"), "na")
  expect_equal(fpt("x", "y", "discrete", "discrete"), "discrete")
  expect_equal(fpt("x", "y", "discrete", "continuous"), "combo")
  expect_equal(fpt("x", "y", "continuous", "discrete"), "combo")
  expect_equal(fpt("x", "y", "continuous", "continuous"), "continuous")
  expect_equal(fpt("x", "y", "continuous", "continuous", isAllNa = TRUE), "na")
})

test_that("plot_types()", {
  dt <- data.frame(num = c(1, 2, 3), fct = factor(c("a", "b", "a")))
  info <- plot_types(dt, 1:2, 1:2)

  expect_equal(
    info$plotType,
    c("continuous-diag", "combo", "combo", "discrete-diag")
  )
  expect_equal(info$xVar, c("num", "fct", "num", "fct"))
  expect_equal(info$yVar, c(NA, "num", "fct", NA))
  expect_equal(info$posX, c(1L, 2L, 1L, 2L))
  expect_equal(info$posY, c(1L, 1L, 2L, 2L))
  # combo is vertical when the x column is discrete
  expect_equal(info$isVertical, c(NA, TRUE, FALSE, NA))

  info <- plot_types(dt, 1:2, 1:2, allowDiag = FALSE)
  expect_equal(info$plotType, c("continuous", "combo", "combo", "discrete"))
  expect_equal(info$yVar, c("num", "num", "fct", "fct"))
})
