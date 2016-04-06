
context("wrap")

test_that("errors", {

  fn <- ggally_points

  # named params
  expect_error(wrap(fn, NA), "all parameters")
  expect_error(wrap(fn, y = TRUE, 5), "all parameters")

  # named params to wrapp
  expect_error(wrapp(fn, list(5)), "'params' must")
  expect_error(wrapp(fn, table(1:10, 1:10)), "'params' must")
  expect_error(wrapp(fn, list(A = 4, 5)), "'params' must")

  # if the character fn doesn't exist
  expect_error(wrap("does not exist", A = 5), "The following")
  expect_error(wrapp("does not exist", list(A = 5)), "The following")
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
