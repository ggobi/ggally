test_that("as_ggmatrix_progress()", {
  expect_false(as_ggmatrix_progress(FALSE, total = 100))
  expect_true(is.function(as_ggmatrix_progress(TRUE, total = 1)))

  fn <- function(pm) NULL
  expect_identical(as_ggmatrix_progress(fn, total = 1), fn)

  expect_snapshot(as_ggmatrix_progress("bad", total = 1), error = TRUE)
})

test_that("as_ggmatrix_progress(NULL) is off when not interactive", {
  skip_if(interactive())
  expect_false(as_ggmatrix_progress(NULL, total = 100))
})

test_that("ggmatrix_progress() makes a progress bar", {
  pb_fn <- ggmatrix_progress()
  pb <- pb_fn(list(nrow = 2, ncol = 3))
  expect_s3_class(pb, "progress_bar")
})
