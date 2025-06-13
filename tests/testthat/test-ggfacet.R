test_that("simple test with iris data", {
  p <- ggfacet(iris, columnsX = 1:2, columnsY = 3:4)
  expect_s3_class(p, "ggplot")
  expect_equal(dim(p$data), c(4L * nrow(iris), ncol(iris) + 4L))

  expect_equal(
    dim(ggfacet(mtcars, columnsX = 1:2, columnsY = 3:5)$data),
    c(6L * nrow(mtcars), ncol(mtcars) + 4L)
  )
})

test_that("warnings", {
  expect_warning(
    ggfacet(iris, columnsX = 1:5, columnsY = 1),
    "1 factor variables are being removed from X columns"
  )
  expect_warning(
    ggfacet(iris, columnsX = 1, columnsY = 1:5),
    "1 factor variables are being removed from Y columns"
  )
})

test_that("generally works", {
  skip_if_not_installed("chemometrics")

  data(NIR, package = "chemometrics")
  NIR_sub <- data.frame(NIR$yGlcEtOH, NIR$xNIR[, 1:3])

  # factor variables
  vdiffr::expect_doppelganger(
    "factor",
    ggfacet(
      NIR_sub,
      columnsY = 1:2,
      columnsX = 3:5,
      fn = ggally_smooth_loess
    )
  )

  vdiffr::expect_doppelganger(
    "pigs",
    ggts(
      pigs,
      "time",
      c("gilts", "profit", "s_per_herdsz", "production", "herdsz")
    )
  )
})
