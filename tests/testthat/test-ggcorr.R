
context("ggcorr")

# nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")

data(flea)

test_that("limits", {
  vdiffr::expect_doppelganger("flea", ggcorr(flea[, -1]))
  vdiffr::expect_doppelganger("flea-limits", ggcorr(flea[, -1], limits = TRUE))
  vdiffr::expect_doppelganger("flea-no-limits", ggcorr(flea[, -1], limits = FALSE))
  vdiffr::expect_doppelganger("flea-null-limits", ggcorr(flea[, -1], limits = NULL))
  vdiffr::expect_doppelganger("flea-big-limits", ggcorr(flea[, -1], limits = c(-5, 5)))
  vdiffr::expect_doppelganger("flea-small-limits", ggcorr(flea[, -1], limits = c(-0.5, 0.5)))
})

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
    nbreaks = 6,
    angle = -45,
    palette = "PuOr" # colorblind safe, photocopy-able
  )
  expect_equal(length(p$layers), 3)

  p <- ggcorr(flea[, -1],
         label = TRUE,
         name = "")
  expect_equal(length(p$layers), 3)

  # test other combinations of geoms + color scales
  ggcorr(flea[, -1], nbreaks = 4, palette = "PuOr")
  ggcorr(flea[, -1], nbreaks = 4, geom = "circle")
  ggcorr(flea[, -1], geom = "text")
  ggcorr(flea[, -1], geom = "text", limits = FALSE)
  ggcorr(flea[, -1], nbreaks = 4, geom = "text")
  ggcorr(flea[, -1], nbreaks = 4, palette = "PuOr", geom = "text")

  ggcorr(flea[, -1], label = TRUE, label_alpha = 0.5)

})

test_that("non-numeric data", {
  expect_warning(ggcorr(flea), "not numeric")
})

test_that("null midpoint", {
  expect_message(ggcorr(flea[, -1], midpoint = NULL), "Color gradient")
})

test_that("further options", {
  vdiffr::expect_doppelganger("geom-circle", ggcorr(flea[, -1], geom = "circle"))
  vdiffr::expect_doppelganger("geom-circle-no-limits", ggcorr(flea[, -1], geom = "circle", limits = FALSE))
  vdiffr::expect_doppelganger("geom-tile", ggcorr(flea[, -1], geom = "tile", nbreaks = 3))
  vdiffr::expect_doppelganger("geom-tile-no-limits", ggcorr(flea[, -1], geom = "tile", limits = FALSE))
  expect_error(ggcorr(flea[, -1], layout.exp = "a"), "incorrect layout.exp")
  vdiffr::expect_doppelganger("layout.exp", ggcorr(flea[, -1], layout.exp = 1))
})

test_that("data.matrix", {
  p <- ggcorr(data.matrix(flea[, -1]))
  expect_equal(length(p$layers), 2)
})


test_that("cor_matrix", {
  p <- ggcorr(data = NULL, cor_matrix = cor(flea[, -1], use = "pairwise"))
  expect_equal(length(p$layers), 2)
})

test_that("other geoms", {
  expect_error(ggcorr(flea[, -1], geom = "hexbin"), "incorrect geom")
  vdiffr::expect_doppelganger("geom-blank",
    ggcorr(flea[, -1], geom = "blank")
  )
})

test_that("backwards compatibility", {
  vdiffr::expect_doppelganger("method-everything",
    ggcorr(flea[, -1], method = "everything")
  )
})
