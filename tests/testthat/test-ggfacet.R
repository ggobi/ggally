context("ggfacet")

expect_print <- function(p) {
  testthat::expect_silent(print(p))
}

if (requireNamespace("chemometrics", quietly = TRUE)) {
  data(NIR, package = "chemometrics")
  NIR_sub <- data.frame(NIR$yGlcEtOH, NIR$xNIR[, 1:3])

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
    # factor variables
    expect_print(
      ggfacet(
        NIR_sub,
        columnsY = 1:2, columnsX = 3:5,
        fn = ggally_smooth_loess
      )
    )

  })

  test_that("generally works", {
    # factor variables
    expect_print(
      ggfacet(
        NIR_sub,
        columnsY = 1:2, columnsX = 3:5,
        fn = ggally_smooth_loess
      )
    )


    expect_print(
      ggts(pigs, "time", c("gilts", "profit", "s_per_herdsz", "production", "herdsz"))
    )

  })
}
