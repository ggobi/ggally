
context("wrap")

test_that("errors", {

  fn <- ggally_points

  expect_error(wrap(fn, NA))
  expect_error(wrap(fn, NA, y = TRUE))
  expect_error(wrapp(fn, list(5)))
  expect_error(wrapp(fn, table(1:10, 1:10)))
  expect_error(wrapp(fn, list(A = 4, 5)))
})

test_that("wrap", {
  (regularPlot <- ggally_points(
    iris,
    ggplot2::aes(Sepal.Length, Sepal.Width),
    size = 5, color = "red"
  ))

  # Wrap ggally_points to have parameter values size = 5 and color = 'red'
  w_ggally_points <- wrap(ggally_points, size = 5, color = "red")
  (wrappedPlot <- w_ggally_points(
    iris,
    ggplot2::aes(Sepal.Length, Sepal.Width)
  ))

  # Double check the aes parameters are the same for the geom_point layer
  expect_true(identical(regularPlot$layers[[1]]$aes_params, wrappedPlot$layers[[1]]$aes_params))
})
