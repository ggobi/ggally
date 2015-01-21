

context("ggparcoord")

data(diamonds, package="ggplot2")
diamonds.samp <- diamonds[sample(1:dim(diamonds)[1],100),]

test_that("stops", {

  # basic parallel coordinate plot, using default settings
  # ggparcoord(data = diamonds.samp,columns = c(1,5:10))
  # this time, color by diamond cut
  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = NULL, order = "anyClass"), "can't use the 'order' methods ")
  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = NULL, order = "allClass"), "can't use the 'order' methods ")

  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = c(1,2)), "invalid value for 'groupColumn'")
  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = 1i), "invalid value for 'groupColumn'")

  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = 2, scale = "notValid"), "invalid value for 'scale'")

  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = 2, centerObsID = nrow(diamonds.samp) + 10), "invalid value for 'centerObsID'")

  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = 2, missing = "notValid"), "invalid value for 'missing'")

  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = 2, order = "notValid"), "invalid value for 'order'")
  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = 2, order = 1i), "invalid value for 'order'")

  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = 2, showPoints = 1), "invalid value for 'showPoints'")

  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = 2, alphaLines = "notAColumn"), "'alphaLines' column is missing in data")
  tmpDt <- diamonds.samp
  tmpDt$price[1] <- NA
  range(tmpDt$price)
  expect_error(ggparcoord(data = tmpDt,columns = c(1,5:10), groupColumn = 2, alphaLines = "price"), "missing data in 'alphaLines' column")
  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = 2, alphaLines = "price"), "invalid value for 'alphaLines' column; max range ")
  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = 2, alphaLines = -0.1), "invalid value for 'alphaLines'; must be a scalar value")
  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = 2, alphaLines = 1.1), "invalid value for 'alphaLines'; must be a scalar value")

  expect_error(ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = 2, boxplot = 1), "invalid value for 'boxplot'")

  expect_error(ggparcoord(diamonds.samp, columns = c(1,5:10), groupColumn = 2, splineFactor = NULL), "invalid value for 'splineFactor'")


})

test_that("alphaLines", {
  iris2 <- iris
  iris2$alphaLevel <- c("setosa" = 0.2, "versicolor" = 0.3, "virginica" = 0)[iris2$Species]
  p <- ggparcoord(
    data = iris2, columns = 1:4, groupColumn = 5,
    order = "anyClass", showPoints = TRUE,
    title = "Parallel Coordinate Plot for the Iris Data",
    alphaLines = "alphaLevel"
  )

  expect_equivalent(as.character(get("mapping", envir = p$layers[[1]])$alpha), "alphaLevel")

})

test_that("splineFactor", {
  ## Use splines on values, rather than lines (all produce the same result)
  columns <- c(1, 5:10)
  p1 <- ggparcoord(diamonds.samp, columns, groupColumn = 2, splineFactor = TRUE)
  p2 <- ggparcoord(diamonds.samp, columns, groupColumn = 2, splineFactor = 3)

  splineFactor <- length(columns) * 3
  p3 <- ggparcoord(diamonds.samp, columns, groupColumn = 2, splineFactor = I(splineFactor))

  pList <- list(p1, p2, p3)
  for (p in pList) {
    expect_equivalent(as.character(get("mapping", envir = p$layers[[1]])$x), "spline.x")
    expect_equivalent(as.character(get("mapping", envir = p$layers[[1]])$y), "spline.y")

    tmp <- unique(as.numeric(get("data", envir = p$layers[[1]])$ggally_splineFactor))
    expect_true((tmp == 3) || (tmp == 21))
  }

})

test_that("basic", {

  ds2 <- diamonds.samp
  ds2$color <- as.character(ds2$color)

  # column 3 has a character
  # column 4 has a factor
  p <- ggparcoord(data = ds2,columns = c(1,3:10), groupColumn = 2)
  expect_true("color" %in% levels(p$data$variable))
  expect_true("clarity" %in% levels(p$data$variable))
  expect_true(is.numeric(p$data$value))
})

test_that("scale", {
  for (scale in c("std", "robust", "uniminmax", "globalminmax", "center", "centerObs")) {
    p <- ggparcoord(data = diamonds.samp, columns = c(1,5:10), groupColumn = 2, scale = scale)
  }
  expect_true(TRUE)
})

test_that("missing", {
  ds2 <- diamonds.samp
  ds2[3, 1] <- NA

  for (missing in c("exclude", "mean", "median", "min10", "random")) {
    p <- ggparcoord(data = ds2, columns = c(1,5:10), groupColumn = 2, missing = missing)
  }
  expect_true(TRUE)
})

test_that("basic", {

  # basic parallel coordinate plot, using default settings
  # ggparcoord(data = diamonds.samp,columns = c(1,5:10))
  # this time, color by diamond cut
  gpd <- ggparcoord(data = diamonds.samp,columns = c(1,5:10), groupColumn = 2)
  # gpd
  # underlay univariate boxplots, add title, use uniminmax scaling
  gpd <- ggparcoord(data = diamonds.samp,columns = c(1,5:10),groupColumn = 2,
    scale = "uniminmax",boxplot = TRUE,title = "Parallel Coord. Plot of Diamonds Data")

  expect_true(TRUE)
})



