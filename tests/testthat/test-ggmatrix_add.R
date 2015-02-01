
context("ggmatrix_add")

data(tips, package = "reshape")

test_that("add", {

  a <- ggpairs(tips)

  expect_true(is.null(a$gg))

  # first add
  a1 <- a + ggplot2::theme_bw()
  expect_true(! is.null(a1$gg))

  # second to nth add
  a2 <- a1 + ggplot2::theme_bw()
  expect_true(! is.null(a2$gg))

  # badd add
  expect_error(a + ggplot2::geom_abline(), "'ggmatrix' does not know how to add")

})
