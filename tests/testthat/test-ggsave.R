


context("ggsave")

test_that("ggsave", {

  pm <- ggpairs(iris, 1:2)

  test_file <- "test.pdf"

  on.exit({
    unlink(test_file)
  })

  expect_true(!file.exists(test_file))

  expect_silent({
    ggsave(test_file, pm, width = 7, height = 7)
  })

  expect_true(file.exists(test_file))

})
