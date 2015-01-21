

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



