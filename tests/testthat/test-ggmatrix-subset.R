context("ggmatrix subset")

expect_print <- function(x, nr, nc) {
  testthat::expect_equal(nrow(x), nr)
  testthat::expect_equal(ncol(x), nc)
  testthat::expect_equal(length(x), nr * nc)
  testthat::expect_silent(print(x))
}

test_that("basic matrix subsetting work", {

  pm <- ggpairs(iris, 1:3)

  expect_print(pm[1:2, 1:2], 2, 2)
  expect_print(pm[1:2], 3, 2)
  expect_true(inherits(pm[[3]], "ggplot"))
  expect_print(
    pm[array(c(T, T, F, T, T, T, F, F, F), c(3,3), byrow = TRUE)],
    2, 3
  )
  expect_print(
    pm[matrix(c(T, T, F, T, T, T, F, F, F), 3, 3, byrow = TRUE)],
    2, 3
  )
  expect_print(
    pm[1:2, 3:4],
    2, 2
  )

  # fake the rbind
  expect_print(
    pm[c(1:2, 1:2), 3:4],
    4, 2
  )
  # fake the cbind
  expect_print(
    pm[1:2, c(3:4, 3:4)],
    2, 4
  )


})


context("ggmatrix cbind / rbind")

test_that("cbind and rbind", {

  pm <- ggpairs(iris, 1:2)

  expect_silent({
    pmc <- cbind(pm, pm, pm)
    pmr <- rbind(pm, pm, pm)
  })

  expect_print(pmc, 2, 6)
  expect_print(pmr, 6, 2)
})
