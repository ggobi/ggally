
context("ggscatmat")

data(flea)

test_that("example", {

  expect_warning(p <- ggscatmat(flea, 1:3), "Factor variables are omitted in plot")
  expect_true(is.null(p$labels$colour))
  # print(p)

  p <- ggscatmat(flea, columns = 2:4, color = "species")
  expect_true(!is.null(p$labels$colour))
  # print(p)


})

test_that("stops", {
  expect_error(ggscatmat(flea, columns = c(1, 2)), "Not enough numeric variables to")
  expect_error(ggscatmat(flea, columns = c(1, 1, 1)), "All of your variables are factors")
  expect_error(scatmat(flea, columns = c(1, 1, 1)), "All of your variables are factors")
})
