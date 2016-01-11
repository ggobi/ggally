
context("gg-plots")

data(tips, package = "reshape")
data(nasa)
nas <- subset(nasa, x <= 2 & y == 1)

test_that("density", {

  p <- ggally_density(tips, mapping = ggplot2::aes(x = total_bill, y = tip))
  expect_equal(p$type, "continuous")
  expect_equal(p$subType, "density")


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
  expect_equal(as.character(get("mapping", envir = p$layers[[2]])$colour), "labelp")

  p <- ggally_cor(
    ti,
    ggplot2::aes(x = total_bill, y = tip, color = I("blue")),
    use = "complete.obs"
  )
  expect_equal(deparse(get("aes_params", envir = p$layers[[1]])$colour), "I(\"blue\")")

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

})

test_that("dates", {

  class(nas) <- c("NOTFOUND", "data.frame")
  p <- ggally_cor(nas, ggplot2::aes(x = "date", y = "ozone"))
  expect_equal(get("aes_params", envir = p$layers[[1]])$label, "Corr:\n0.278")
  p <- ggally_cor(nas, ggplot2::aes(y = "date", x = "ozone"))
  expect_equal(get("aes_params", envir = p$layers[[1]])$label, "Corr:\n0.278")

  p <- ggally_barDiag(nas, ggplot2::aes(x = date))
  expect_equal(as.character(p$mapping$x), "date")
  expect_equal(p$labels$y, "count")

})
