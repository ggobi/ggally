
context("gg-plots")

data(tips, package = "reshape")
data(nasa)
nas <- subset(nasa, x <= 2 & y == 1)

expect_print <- function(x) {
  testthat::expect_silent(print(x))
}

test_that("denstrip", {
  expect_message(
    suppressWarnings(print(ggally_denstrip(tips, mapping = aes_string("sex", "tip")))),
    "`stat_bin()` using `bins = 30`", fixed = TRUE
  )
  expect_message(
    suppressWarnings(print(ggally_denstrip(tips, mapping = aes_string("tip", "sex")))),
    "`stat_bin()` using `bins = 30`", fixed = TRUE
  )
})


test_that("density", {

  p <- ggally_density(
    tips,
    mapping = ggplot2::aes_string(x = "total_bill", y = "tip", fill = "..level..")
  ) + ggplot2::scale_fill_gradient(breaks = c(0.05, 0.1, 0.15, 0.2))
  expect_equal(p$labels$fill, "level")

})

test_that("cor", {

  expect_warning(
    ggally_cor(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip"), use = "NOTFOUND"),
    "correlation 'use' not found"
  )


  ti <- tips
  class(ti) <- c("NOTFOUND", "data.frame")
  p <- ggally_cor(ti, ggplot2::aes(x = total_bill, y = tip, color = day), use = "complete.obs")
  expect_equal(mapping_string(get("mapping", envir = p$layers[[2]])$colour), "labelp")

  p <- ggally_cor(
    ti,
    ggplot2::aes(x = total_bill, y = tip, color = I("blue")),
    use = "complete.obs"
  )
  expect_equal(mapping_string(get("mapping", envir = p$layers[[1]])$colour), "I(\"blue\")")

  expect_err <- function(..., msg = NULL) {
    expect_error(
      ggally_cor(
        ti, ggplot2::aes(x = total_bill, y = tip),
        ...
      ),
      msg
    )
  }
  expect_err(corAlignPercent = 0.9, "'corAlignPercent' is deprecated")
  expect_err(corMethod = "pearson", "'corMethod' is deprecated")
  expect_err(corUse = "complete.obs", "'corUse' is deprecated")

  expect_print(ggally_cor(ti, ggplot2::aes(x = total_bill, y = tip, color = I("green"))))

  ti3 <- ti2 <- ti
  ti2[2, "total_bill"] <- NA

  ti3[2, "total_bill"] <- NA
  ti3[3, "tip"] <- NA
  ti3[4, "total_bill"] <- NA
  ti3[4, "tip"] <- NA

  expect_warn <- function(data, msg) {
    expect_warning(
      ggally_cor(data, ggplot2::aes(x = total_bill, y = tip)),
      msg
    )
  }
  expect_warn(ti2, "Removing 1 row that")
  expect_warn(ti3, "Removed 3 rows containing")

  expect_error(
    ggally_cor(
      ti,
      ggplot2::aes(x = total_bill, y = tip, color = size)
    ),
    "ggally_cor: mapping color column"
  )
  expect_silent(
    ggally_cor(
      ti,
      ggplot2::aes(x = total_bill, y = tip, color = as.factor(size))
    )
  )
})

test_that("diagAxis", {
  p <- ggally_diagAxis(iris, ggplot2::aes(x = Petal.Width))
  pDat1 <- get("data", envir = p$layers[[2]])
  attr(pDat1, "out.attrs") <- NULL
  testDt1 <- data.frame(
    xPos = c(0.076, 0.076, 0.076, 0.076, 0.076, 0.076, 0.500, 1.000, 1.500, 2.000, 2.500),
    yPos = c(0.500, 1.000, 1.500, 2.000, 2.500, 0.076, 0.076, 0.076, 0.076, 0.076, 0.076),
    lab = as.character(c(0.5, 1, 1.5, 2, 2.5, 0, 0.5, 1, 1.5, 2, 2.5)),
    hjust = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5),
    vjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    stringsAsFactors = FALSE
  )
  rownames(testDt1) <- 2:12
  expect_equal(pDat1, testDt1)

  p <- ggally_diagAxis(iris, ggplot2::aes(x = Species))
  pDat2 <- get("data", envir = p$layers[[2]])
  attr(pDat2, "out.attrs") <- NULL
  testDt2 <- data.frame(
    x = c(0.125, 0.500, 0.875),
    y = c(0.875, 0.500, 0.125),
    lab = c("setosa", "versicolor", "virginica")
  )
  expect_equal(pDat2, testDt2)


  expect_error({
    ggally_diagAxis(iris, mapping = ggplot2::aes(y = Sepal.Length))
  }, "mapping\\$x is null.") # nolint
})

test_that("dates", {

  class(nas) <- c("NOTFOUND", "data.frame")
  p <- ggally_cor(nas, ggplot2::aes(x = date, y = ozone))
  expect_equal(get("aes_params", envir = p$layers[[1]])$label, "Corr:\n0.278")
  p <- ggally_cor(nas, ggplot2::aes(y = date, x = ozone))
  expect_equal(get("aes_params", envir = p$layers[[1]])$label, "Corr:\n0.278")

  p <- ggally_barDiag(nas, ggplot2::aes(x = date))
  expect_equal(mapping_string(p$mapping$x), "date")
  expect_equal(p$labels$y, "count")

})

test_that("rescale", {
  p <- ggally_densityDiag(tips, mapping = ggplot2::aes(x = day), rescale = FALSE)
  expect_true(p$labels$y == "density")
  expect_print(p)

  p <- ggally_densityDiag(tips, mapping = ggplot2::aes(x = day), rescale = TRUE)
  expect_true(! identical(p$labels$y, "density"))
  expect_print(p)


  p <- ggally_barDiag(tips, mapping = ggplot2::aes(x = tip), binwidth = 0.25, rescale = FALSE)
  expect_true(p$labels$y == "count")
  expect_print(p)

  p <- ggally_barDiag(tips, mapping = ggplot2::aes(x = tip), binwidth = 0.25, rescale = TRUE)
  expect_true(! identical(p$labels$y, "count"))
  expect_print(p)



})



test_that("shrink", {
  p <- ggally_smooth_loess(iris, mapping = ggplot2::aes(Sepal.Width, Petal.Length))
  expect_true(!is.null(p$coordinates$limits$y))
  expect_print(p)

  p <- ggally_smooth_loess(iris, mapping = ggplot2::aes(Sepal.Width, Petal.Length), shrink = FALSE)
  expect_true(is.null(p$coordinates$limits$y))
  expect_print(p)
})

test_that("smooth_se", {
  p <- ggally_smooth_loess(iris, mapping = ggplot2::aes(Sepal.Width, Petal.Length), se = TRUE)
  expect_equal(p$layers[[2]]$stat_params$se, TRUE)
  expect_print(p)

  p <- ggally_smooth_loess(iris, mapping = ggplot2::aes(Sepal.Width, Petal.Length), se = FALSE)
  expect_equal(p$layers[[2]]$stat_params$se, FALSE)
  expect_print(p)
})
