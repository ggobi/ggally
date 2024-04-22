data(tips)

test_that("ggally_cor_v1_5() works", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_silent(
    p <- ggally_cor_v1_5(tips, ggplot2::aes(!!as.name("total_bill"), !!as.name("tip")))
  )
})

test_that("v1_ggmatrix_theme() is deprecated", {
  expect_snapshot(
    v1_ggmatrix_theme()
  )
})

test_that("ggally_cor_v1_5() is deprecated", {
  expect_snapshot(
    p <- ggally_cor_v1_5(tips, ggplot2::aes(!!as.name("total_bill"), !!as.name("tip")))
  )
})
