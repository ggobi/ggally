
context("ggmatrix_add")

data(tips, package = "reshape")

test_that("add", {

  pm <- ggpairs(tips)

  expect_true(is.null(pm$title))
  expect_true(is.null(pm$xlab))
  expect_true(is.null(pm$ylab))
  pm1 <- pm + labs(title = "my title", x = "x label", y = "y label")
  expect_equivalent(pm1$title, "my title")
  expect_equivalent(pm1$xlab, "x label")
  expect_equivalent(pm1$ylab, "y label")

  expect_true(is.null(pm$gg))

  # first add
  pm2 <- pm + ggplot2::theme_bw()
  expect_true(! is.null(pm2$gg))

  # second to nth add
  pm3 <- pm + ggplot2::theme_bw()
  expect_true(! is.null(pm3$gg))

  # badd add
  expect_error(pm + ggplot2::geom_abline(), "'ggmatrix' does not know how to add")

})
