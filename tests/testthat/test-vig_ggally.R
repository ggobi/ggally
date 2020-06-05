test_that("all vignetts are accounted for", {
  # only run on CI
  testthat::skip_if_not(isTRUE(as.logical(Sys.getenv("CI"))))

  # make sure vig dir exists
  vig_dir <- file.path("..", "..", "vignettes")
  testthat::skip_if_not(dir.exists(vig_dir))

  vigs <- dir(vig_dir, pattern = "\\.Rmd$")
  vigs <- sub(".Rmd", "", vigs, fixed = TRUE)
  expect_equal(vignettes_for_ggally, vigs)
})
