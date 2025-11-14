data(tips)

test_that("stops", {
  pm <- ggpairs(tips)
  p <- ggally_blankDiag()
  expect_snapshot(pm["total_bill", 1], error = TRUE)
  expect_snapshot(pm[1, "total_bill"], error = TRUE)
  expect_snapshot(pm["total_bill", 1] <- p, error = TRUE)
  expect_snapshot(pm[1, "total_bill"] <- p, error = TRUE)

  pm <- ggduo(tips, 1:3, 1:4)
  expect_snapshot(pm[0, 1], error = TRUE)
  expect_snapshot(pm[1, 0], error = TRUE)
  expect_snapshot(pm[5, 1], error = TRUE)
  expect_snapshot(pm[1, 4], error = TRUE)

  for (i in 1:4) {
    for (j in 1:3) {
      expect_silent({
        p <- pm[i, j]
      })
    }
  }
})


test_that("get", {
  a <- ggpairs(
    tips,
    1:4,
    axisLabels = "show"
  )
  p <- a[2, 1]
  labs <- get_labs(p)
  expect_equal(labs$x, "total_bill")
  expect_equal(labs$y, "tip")

  # test odd input and retrieve it
  a[2, 1] <- 1:4
  expect_snapshot(a[2, 1], error = TRUE)
})

test_that("put", {
  a <- ggpairs(
    tips,
    1:4,
    axisLabels = "show"
  )
  txt <- "My Custom Plot"
  a[2, 1] <- ggally_text(txt)
  p <- a[2, 1]
  expect_equal(get("aes_params", envir = p$layers[[1]])$label, txt)
})
