
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
  ) + ggplot2::scale_fill_gradient(breaks = c(0.05, 0.1,0.15,0.2))
  expect_equal(p$labels$fill, "level")

})

test_that("cor", {

  expect_warning(
    ggally_cor(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip"), corUse = "NOTFOUND"),
    "correlation 'use' not found"
  )
})

test_that("diagAxis", {
  ggally_diagAxis(iris, ggplot2::aes(x=Petal.Width))
  ggally_diagAxis(iris, ggplot2::aes(x=Species))
})

test_that("dates", {

  p <- ggally_cor(nas, ggplot2::aes(x = "date", y = "ozone"))
  expect_equal(get("geom_params", envir = p$layers[[1]])$label, "Corr:\n0.278")
  p <- ggally_cor(nas, ggplot2::aes(y = "date", x = "ozone"))
  expect_equal(get("geom_params", envir = p$layers[[1]])$label, "Corr:\n0.278")

  p <- ggally_barDiag(nas, ggplot2::aes(x = date))
  expect_equal(as.character(p$mapping$x), "date")
  expect_equal(p$labels$y, "count")

})
