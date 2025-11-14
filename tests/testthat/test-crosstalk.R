test_that("crosstalk works with ggduo and ggpairs", {
  skip_if_not_installed("crosstalk")

  sd <- try(crosstalk::SharedData$new(iris[1:4]), silent = TRUE)
  if (inherits(sd, "try-error")) {
    skip("crosstalk data can not be initialized")
  }

  expect_silent({
    pm <- ggpairs(sd)
  })
  expect_snapshot(ggpairs(sd, 3:5), error = TRUE)
  expect_snapshot(
    ggpairs(sd, c("Petal.Length", "Petal.Width", crosstalk_key())),
    error = TRUE
  )

  expect_silent({
    pm <- ggduo(sd)
  })
  expect_snapshot(ggduo(sd, c(1:2, 5), 3:5), error = TRUE)
  expect_snapshot(
    ggduo(
      sd,
      c("Sepal.Length", "Sepal.Width", crosstalk_key()),
      c("Petal.Length", "Petal.Width")
    ),
    error = TRUE
  )
  expect_snapshot(
    ggduo(
      sd,
      c("Sepal.Length", "Sepal.Width"),
      c("Petal.Length", "Petal.Width", crosstalk_key())
    ),
    error = TRUE
  )
})
