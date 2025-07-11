data(tips)

test_that("stops", {
  expect_error(
    ggmatrix(plots = matrix(), nrow = 2, ncol = 3),
    "'plots' must be a list()"
  )

  expect_error(
    ggmatrix(plots = list(), nrow = "2", ncol = 3),
    "'nrow' must be a numeric value"
  )
  expect_error(
    ggmatrix(plots = list(), nrow = 2, ncol = "3"),
    "'ncol' must be a numeric value"
  )

  expect_error(
    ggmatrix(plots = list(), nrow = c(2, 3), ncol = 3),
    "'nrow' must be a single numeric value"
  )
  expect_error(
    ggmatrix(plots = list(), nrow = 2, ncol = c(2, 3)),
    "'ncol' must be a single numeric value"
  )
})


test_that("expression labels", {
  chars <- c("col1", "col2")
  exprs <- c("alpha[0]", "gamma[x + y ^ z]")

  p <- ggpairs(tips, 1:2, columnLabels = exprs, labeller = "label_parsed")
  ggally_expect_doppelganger("expression-labels", p)

  expect_error(
    print(ggpairs(tips, 1:2, columnLabels = expression(alpha, beta))),
    "xAxisLabels"
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
    2,
    3,
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
    2,
    3,
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
    2,
    3,
    c("A", "B", "C"),
    c("D", "E"),
    byrow = TRUE
  )
  # reaches code where there are more cells than plots
  ggally_expect_doppelganger("not-enough-plots", a)

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


test_that("blank", {
  pm <- ggpairs(tips, 1:2)
  pm[1, 2] <- "blank"
  ggally_expect_doppelganger("blank-1_2", pm)

  pm[2, 1] <- NULL
  ggally_expect_doppelganger("blank-1_2-2_1", pm)

  expect_equal(length(pm$plots), 4)

  expect_error(
    {
      pm[2, 2] <- "not blank"
    },
    "character values \\(besides 'blank'\\)"
  )
})

test_that("proportions", {
  pm <- ggpairs(iris, 1:2, mapping = ggplot2::aes(color = Species))
  pm[2, 2] <- pm[2, 2] + ggplot2::coord_flip()

  pm2 <- ggmatrix(
    data = iris,
    pm$plots,
    ncol = 2,
    nrow = 2,
    xProportions = c(2, 1),
    yProportions = c(1, 2),
    title = "big plot, small marginals"
  )
  ggally_expect_doppelganger("proportions", pm2)

  # turn on progress for a quick plot
  # TODO - turn test back on when it uses message properly
  # testthat::expect_message(print(pm2, progress = TRUE))
})


test_that("ggmatrix_gtable progress", {
  pm <- ggpairs(iris, 1:2)
  expect_silent({
    pg <- ggmatrix_gtable(pm)
  })
  lifecycle::expect_deprecated({
    ggmatrix_gtable(pm, progress = TRUE)
  })
  lifecycle::expect_deprecated({
    ggmatrix_gtable(pm, progress_format = "asdfasdf :plot_i")
  })
})

#
# printShowStrips <- c(TRUE, FALSE)
# if (i <= length(printShowStrips)) {
#   printShowStrip <- printShowStrips[i]
# } else {
#   printShowStrip <- NULL
# }
#

test_that("ggmatrix proportions", {
  expect_error(
    {
      ggmatrix_proportions("not auto", tips, 1:ncol(tips))
    },
    "need to be non-NA"
  )
  expect_error(
    {
      ggmatrix_proportions(NA, tips, 1:ncol(tips))
    },
    "need to be non-NA"
  )
  expect_error(
    {
      ggmatrix_proportions(c(1, NA, 1, 1, 1, 1, 1), tips, 1:ncol(tips))
    },
    "need to be non-NA"
  )

  expect_equal(
    ggmatrix_proportions("auto", tips, 1:ncol(tips)),
    c(2.5, 2.5, 2, 2, 4, 2, 2.5)
  )

  expect_equal(
    ggmatrix_proportions(1, tips, 1:ncol(tips)),
    c(1, 1, 1, 1, 1, 1, 1)
  )
  expect_equal(
    ggmatrix_proportions(NULL, tips, 1:ncol(tips)),
    NULL
  )
})
