test_that("all vignetts are accounted for", {
  testthat::skip_on_cran()

  vig_dir <- file.path("..", "..", "vignettes")

  # make sure vig dir exists
  testthat::skip_if_not(dir.exists(vig_dir))

  vigs <- dir(vig_dir, pattern = "\\.Rmd$")
  vigs <- sub(".Rmd", "", vigs, fixed = TRUE)
  expect_equal(vignettes_for_ggally, vigs)
})
