
context("ggcorr")

# nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")

data(flea)



test_that("examples", {
  # Default output.
  p <- ggcorr(flea[, -1])
  expect_equal(length(p$layers), 2)

  # Labelled output, with coefficient transparency.
  p <- ggcorr(flea[, -1],
         label = TRUE,
         label_alpha = TRUE,
         name = "")
  expect_equal(length(p$layers), 3)

  # Custom options.
  p <- ggcorr(
    flea[, -1],
    geom = "circle",
    max_size = 6,
    size = 3,
    hjust = 0.75,
    angle = -45,
    palette = "PuOr" # colorblind safe, photocopy-able
  )
  expect_equal(length(p$layers), 3)


  p <- ggcorr(flea[, -1],
         label = TRUE,
         name = "")
  expect_equal(length(p$layers), 3)
})


test_that("data.matrix", {
  # Default output.
  p <- ggcorr(data.matrix(flea[, -1]))
  expect_equal(length(p$layers), 2)
})
