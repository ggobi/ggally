
context("deprecated")

data(tips, package = "reshape")

test_that("ggally-cor", {

  expect_silent(
    p <- ggally_cor_v1_5(tips, ggplot2::aes_string("total_bill", "tip"))
  )
})
