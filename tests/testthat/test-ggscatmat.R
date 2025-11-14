data(flea)

test_that("example", {
  flea2 <- flea
  flea2$species2 <- as.character(flea2$species)
  expect_warning(
    p <- ggscatmat(flea2, c(1:3)),
    "Factor variables are omitted in plot"
  )
  expect_warning(
    p <- ggscatmat(flea2, c(2:3, 8)),
    "Factor variables are omitted in plot"
  )
  expect_true(is.null(get_labs(p)$colour))
  ggally_expect_doppelganger("flea", p)

  p <- ggscatmat(flea, columns = 2:4, color = "species")
  expect_true(!is.null(get_labs(p)$colour))
  ggally_expect_doppelganger("flea-color", p)
})

test_that("corMethod", {
  p <- ggscatmat(flea, columns = 2:3, corMethod = "pearson")
  ggally_expect_doppelganger("flea-pearson", p)
  p <- ggscatmat(flea, columns = 2:3, corMethod = "rsquare")
  ggally_expect_doppelganger("flea-rsquare", p)
})

test_that("stops", {
  expect_snapshot(ggscatmat(flea, columns = c(1, 2)), error = TRUE)
  expect_snapshot(ggscatmat(flea, columns = c(1, 1, 1)), error = TRUE)
  expect_snapshot(scatmat(flea, columns = c(1, 1, 1)), error = TRUE)
})
