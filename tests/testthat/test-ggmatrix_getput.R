
context("ggmatrix_getput")

data(tips, package = "reshape")

test_that("stops", {

  pm <- ggpairs(tips)
  p <- ggally_blankDiag()
  expect_error(pm["total_bill", 1], "'i' may only be a single")
  expect_error(pm[1, "total_bill"], "'j' may only be a single")
  expect_error(pm["total_bill", 1] <- p, "'i' may only be a single")
  expect_error(pm[1, "total_bill"] <- p, "'j' may only be a single")

  pm <- ggduo(tips, 1:3, 1:4)
  expect_error(pm[0, 1], "'i' may only be in the range")
  expect_error(pm[1, 0], "'j' may only be in the range")
  expect_error(pm[5, 1], "'i' may only be in the range")
  expect_error(pm[1, 4], "'j' may only be in the range")

  for (i in 1:4) {
    for (j in 1:3) {
      expect_silent({
        p <- pm[i, j]
      })
    }
  }


})


test_that("get", {
  a <- ggpairs(
    tips, 1:4,
    axisLabels = "show"
  )
  p <- a[2, 1]
  expect_equal(p$labels$x, "total_bill")
  expect_equal(p$labels$y, "tip")

  # test odd input and retrieve it
  a[2, 1] <- 1:4
  expect_error({
    a[2, 1]
  }, "unknown plot object type") # nolint

})

test_that("put", {
  a <- ggpairs(
    tips, 1:4,
    axisLabels = "show"
  )
  txt <- "My Custom Plot"
  a[2, 1] <- ggally_text(txt)
  p <- a[2, 1]
  expect_equal(get("aes_params", envir = p$layers[[1]])$label, txt)

})
