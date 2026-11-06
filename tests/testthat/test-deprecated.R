data(tips)

test_that("ggally_cor_v1_5() works", {
  lifecycle::expect_deprecated(
    {
      p <- ggally_cor_v1_5(
        tips,
        ggplot2::aes(!!as.name("total_bill"), !!as.name("tip"))
      )
    }
  )
})

test_that("v1_ggmatrix_theme() is deprecated", {
  lifecycle::expect_deprecated(th <- v1_ggmatrix_theme())
  expect_equal(
    th,
    theme(
      strip.background = element_rect(fill = "white"),
      strip.placement = "outside"
    )
  )
})

test_that("ggally_cor_v1_5() is deprecated", {
  old_opts <- options(lifecycle_verbosity = "quiet")
  on.exit(options(old_opts), add = TRUE)

  expect_snapshot(
    p <- ggally_cor_v1_5(
      tips,
      ggplot2::aes(!!as.name("total_bill"), !!as.name("tip"))
    )
  )
})
