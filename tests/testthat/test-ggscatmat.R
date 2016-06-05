
context("ggscatmat")

data(flea)

test_that("example", {

  flea2 <- flea
  flea2$species2 <- as.character(flea2$species)
  expect_warning(p <- ggscatmat(flea2, c(1:3)), "Factor variables are omitted in plot")
  expect_warning(p <- ggscatmat(flea2, c(2:3, 8)), "Factor variables are omitted in plot")
  expect_true(is.null(p$labels$colour))
  # print(p)

  p <- ggscatmat(flea, columns = 2:4, color = "species")
  expect_true(!is.null(p$labels$colour))
  # print(p)

})

test_that("corMethod", {
  expect_silent({
    p <- ggscatmat(flea, columns = 2:3, corMethod = "pearson")
    p <- ggscatmat(flea, columns = 2:3, corMethod = "rsquare")
  })
})

test_that("stops", {
  expect_error(ggscatmat(flea, columns = c(1, 2)), "Not enough numeric variables to")
  expect_error(ggscatmat(flea, columns = c(1, 1, 1)), "All of your variables are factors")
  expect_error(scatmat(flea, columns = c(1, 1, 1)), "All of your variables are factors")
})
