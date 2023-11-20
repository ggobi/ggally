
context("ggmatrix_add")

data(tips)

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

  # bad add
  expect_error(pm + 3, "'ggmatrix' does not know how to add")

  # adding scale
  pm4 <- pm + ggplot2::scale_fill_brewer()
  expect_false(identical(pm$plots[[1]], pm4$plots[[1]]))
  expect_false(identical(pm$plots[[2]], pm4$plots[[2]]))

  # change only some subplots
  pm5 <- add_to_ggmatrix(pm, ggplot2::coord_equal(), cols = 1)
  expect_false(identical(pm$plots[[1]], pm5$plots[[1]]))
  expect_true(identical(pm$plots[[2]], pm5$plots[[2]]))

})


test_that("add_list", {

  pm <- ggpairs(tips, 1:2)

  pm1 <- pm + list(
    ggplot2::labs(x = "x title"),
    ggplot2::labs(title = "list title")
  )

  expect_equal(pm1$xlab, "x title")
  expect_equal(pm1$title, "list title")

})

test_that("v1_ggmatrix_theme", {

  pm <- ggpairs(tips, 1:2)

  pm1 <- pm + v1_ggmatrix_theme()

  expect_true(is.null(pm$gg))
  expect_true(!is.null(pm1$gg))

})
