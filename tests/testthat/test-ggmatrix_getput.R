
context("ggmatrix_getput")

data(tips, package = "reshape")

test_that("stops", {

  a <- ggpairs(tips)
  p <- ggally_blankDiag()
  expect_error(a["total_bill", 1], "'i' may only be")
  expect_error(a[1, "total_bill"], "'j' may only be")
  expect_error(a["total_bill", 1] <- p, "'i' may only be")
  expect_error(a[1, "total_bill"] <- p, "'j' may only be")

})


test_that("get", {
  a <- ggpairs(
    tips, 1:4,
    axisLabels = "show"
  )
  p <- a[2, 1]
  expect_equal(p$labels$x, "total_bill")
  expect_equal(p$labels$y, "tip")

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
