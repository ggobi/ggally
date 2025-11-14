test_that("errors", {
  fn <- ggally_points

  # named params
  expect_snapshot(wrap(fn, NA), error = TRUE)
  expect_snapshot(wrap(fn, y = TRUE, 5), error = TRUE)

  # named params to wrapp
  expect_snapshot(wrapp(fn, list(5)), error = TRUE)
  expect_snapshot(wrapp(fn, table(1:10, 1:10)), error = TRUE)
  expect_snapshot(wrapp(fn, list(A = 4, 5)), error = TRUE)

  # if the character fn doesn't exist
  expect_snapshot(wrap("does not exist", A = 5), error = TRUE)
  expect_snapshot(wrapp("does not exist", list(A = 5)), error = TRUE)
})

test_that("wrap", {
  (regularPlot <- ggally_points(
    iris,
    ggplot2::aes(Sepal.Length, Sepal.Width),
    size = 5,
    color = "red"
  ))

  # Wrap ggally_points to have parameter values size = 5 and color = 'red'
  w_ggally_points <- wrap(ggally_points, size = 5, color = "red")
  (wrappedPlot <- w_ggally_points(
    iris,
    ggplot2::aes(Sepal.Length, Sepal.Width)
  ))

  # Double check the aes parameters are the same for the geom_point layer
  expect_true(identical(
    regularPlot$layers[[1]]$aes_params,
    wrappedPlot$layers[[1]]$aes_params
  ))
})
