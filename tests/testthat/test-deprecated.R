
data(tips)

test_that("ggally-cor", {
  expect_silent(
    p <- ggally_cor_v1_5(tips, ggplot2::aes(!!as.name("total_bill"), !!as.name("tip")))
  )
})

test_that("v1_ggmatrix_theme is deprecated", {
  expect_snapshot(
    v1_ggmatrix_theme()
  )
})
