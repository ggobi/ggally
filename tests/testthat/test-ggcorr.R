data(flea)

test_that("limits", {
  ggally_expect_doppelganger("flea", ggcorr(flea[, -1]))
  ggally_expect_doppelganger("flea-limits", ggcorr(flea[, -1], limits = TRUE))
  ggally_expect_doppelganger(
    "flea-no-limits",
    ggcorr(flea[, -1], limits = FALSE)
  )
  ggally_expect_doppelganger(
    "flea-null-limits",
    ggcorr(flea[, -1], limits = NULL)
  )
  ggally_expect_doppelganger(
    "flea-big-limits",
    ggcorr(flea[, -1], limits = c(-5, 5))
  )
  ggally_expect_doppelganger(
    "flea-small-limits",
    ggcorr(flea[, -1], limits = c(-0.5, 0.5))
  )
})

test_that("examples", {
  # Default output.
  p <- ggcorr(flea[, -1])
  expect_equal(length(p$layers), 2)

  # Labelled output, with coefficient transparency.
  p <- ggcorr(flea[, -1], label = TRUE, label_alpha = TRUE, name = "")
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

  p <- ggcorr(flea[, -1], label = TRUE, name = "")
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
  ggally_expect_doppelganger(
    "geom-circle",
    ggcorr(flea[, -1], geom = "circle")
  )
  ggally_expect_doppelganger(
    "geom-circle-no-limits",
    ggcorr(flea[, -1], geom = "circle", limits = FALSE)
  )
  ggally_expect_doppelganger(
    "geom-tile",
    ggcorr(flea[, -1], geom = "tile", nbreaks = 3)
  )
  ggally_expect_doppelganger(
    "geom-tile-no-limits",
    ggcorr(flea[, -1], geom = "tile", limits = FALSE)
  )
  expect_snapshot(ggcorr(flea[, -1], layout.exp = "a"), error = TRUE)
  ggally_expect_doppelganger("layout.exp", ggcorr(flea[, -1], layout.exp = 1))
})

test_that("data.matrix", {
  p <- ggcorr(data.matrix(flea[, -1]))
  expect_equal(length(p$layers), 2)
})


test_that("cor_matrix", {
  p <- ggcorr(data = NULL, cor_matrix = cor(flea[, -1], use = "pairwise"))
  expect_equal(length(p$layers), 2)
})

test_that("nonsyntactic variable names keep their labels and correlations", {
  data <- flea[2:5]
  labels <- c("e'", "E/e'", "two words", ".ggally_ggcorr_row_names")
  names(data) <- labels
  cor_matrix <- cor(data)
  cells <- which(lower.tri(cor_matrix), arr.ind = TRUE)
  cells <- cells[order(cells[, "row"], cells[, "col"]), ]

  for (plot in list(ggcorr(data), ggcorr(NULL, cor_matrix = cor_matrix))) {
    expect_identical(ggplot2::layer_data(plot, 2)$label, labels)
    expect_identical(plot$data$x, labels[cells[, "row"]])
    expect_identical(as.character(plot$data$y), labels[cells[, "col"]])
    expect_equal(plot$data$coefficient, cor_matrix[cells])
  }
})

test_that("missing variable labels keep their correlation tiles", {
  cor_matrix <- cor(flea[2:4])
  labels <- c("a", NA_character_, "c")
  dimnames(cor_matrix) <- list(labels, labels)

  plot <- ggcorr(NULL, cor_matrix = cor_matrix)
  expect_identical(ggplot2::layer_data(plot, 2)$label, c("a", "NA.", "c"))
  expect_equal(nrow(plot$data), 3L)
})

test_that("other geoms", {
  expect_snapshot(ggcorr(flea[, -1], geom = "hexbin"), error = TRUE)
  ggally_expect_doppelganger(
    "geom-blank",
    ggcorr(flea[, -1], geom = "blank")
  )
})

test_that("backwards compatibility", {
  ggally_expect_doppelganger(
    "method-everything",
    ggcorr(flea[, -1], method = "everything")
  )
})

test_that("label with round gives same size corr values and all corr squares", {
  cors <- matrix(
    c(1, 0, .001, 0, 1, .2, .001, .2, 1),
    nrow = 3,
    byrow = TRUE
  )
  row.names(cors) <- colnames(cors) <- c("X1", "X2", "X3")

  p <- ggcorr(data = NULL, cor_matrix = cors, label = TRUE, label_round = 2)
  ggally_expect_doppelganger("label-round-2", p)
})
