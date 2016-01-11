
context("ggmatrix")
data(tips, package = "reshape")

test_that("stops", {


  expect_error(ggmatrix(plots = matrix(), nrow = 2, ncol = 3), "'plots' must be a list()")

  expect_error(ggmatrix(plots = list(), nrow = "2", ncol = 3), "'nrow' must be a numeric value")
  expect_error(ggmatrix(plots = list(), nrow = 2, ncol = "3"), "'ncol' must be a numeric value")

  expect_error(
    ggmatrix(plots = list(), nrow = c(2, 3), ncol = 3),
    "'nrow' must be a single numeric value"
  )
  expect_error(
    ggmatrix(plots = list(), nrow = 2, ncol = c(2, 3)),
    "'ncol' must be a single numeric value"
  )

})

test_that("byrow", {
  plotList <- list()
  for (i in 1:6) {
    p <- ggally_text(paste("Plot #", i, sep = ""))
    p$ggally_check_val <- i
    plotList[[i]] <- p
  }
  a <- ggmatrix(
    plotList,
    2, 3,
    c("A", "B", "C"),
    c("D", "E"),
    byrow = TRUE
  )

  k <- 1
  for (i in 1:2) {
    for (j in 1:3) {
      expect_equal(a[i, j]$ggally_check_val, k)
      k <- k + 1
    }
  }

  a <- ggmatrix(
    plotList,
    2, 3,
    c("A", "B", "C"),
    c("D", "E"),
    byrow = FALSE
  )
  k <- 1
  for (j in 1:3) {
    for (i in 1:2) {
      expect_equal(a[i, j]$ggally_check_val, k)
      k <- k + 1
    }
  }
  a

})

test_that("missing plot", {
  plotList <- list()
  for (i in c(1, 3, 5)) {
    p <- ggally_text(paste("Plot #", i, sep = ""))
    p$ggally_check_val <- i
    plotList[[i]] <- p
  }
  a <- ggmatrix(
    plotList,
    2, 3,
    c("A", "B", "C"),
    c("D", "E"),
    byrow = TRUE
  )
  # reaches code where there are more cells than plots
  print(a)

  expect_equal(a[1, 1]$ggally_check_val, 1)
  expect_equal(a[1, 3]$ggally_check_val, 3)
  expect_equal(a[2, 2]$ggally_check_val, 5)


})


test_that("str.ggmatrix", {
  pm <- ggpairs(tips, 1:3, upper = "blank")
  pm[1, 1] <- pm[1, 1]
  txt <- capture.output({
    str(pm)
  })

  expect_true(any(str_detect(txt, "Custom str.ggmatrix output:")))

  txt <- capture.output({
    str(pm, raw = TRUE)
  })
  expect_false(any(str_detect(txt, "Custom str.ggmatrix output:")))
})
