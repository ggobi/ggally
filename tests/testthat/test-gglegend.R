library(ggplot2)

test_that("examples", {
  histPlot <-
    ggplot(diamonds, aes(price, fill = cut)) +
    geom_histogram(binwidth = 500)

  (right <- histPlot)
  (bottom <- histPlot + theme(legend.position = "bottom"))
  (top <- histPlot + theme(legend.position = "top"))
  (left <- histPlot + theme(legend.position = "left"))

  expect_legend <- function(name, p) {
    plotLegend <- grab_legend(p)
    expect_true(inherits(plotLegend, "gtable"))
    expect_true(inherits(plotLegend, "gTree"))
    expect_true(inherits(plotLegend, "grob"))
    vdiffr::expect_doppelganger(paste0("pos-", name), plotLegend)
  }

  expect_legend("right", right)
  expect_legend("bottom", bottom)
  expect_legend("top", top)
  expect_legend("left", left)
})


test_that("legend", {
  # display regular plot
  vdiffr::expect_doppelganger(
    "legend",
    ggally_points(
      iris,
      ggplot2::aes(Sepal.Length, Sepal.Width, color = Species)
    )
  )

  # Make a function that will only print the legend
  points_legend <- gglegend(ggally_points)
  l <- points_legend(
    iris,
    ggplot2::aes(Sepal.Length, Sepal.Width, color = Species)
  )
  vdiffr::expect_doppelganger("points", l)

  # produce the sample legend plot, but supply a string that 'wrap' understands
  same_points_legend <- gglegend("points")
  expect_identical(
    attr(attr(points_legend, "fn"), "original_fn"),
    attr(attr(same_points_legend, "fn"), "original_fn")
  )

  # Complicated examples
  custom_legend <- wrap(gglegend("points"), size = 6)
  p <- custom_legend(
    iris,
    ggplot2::aes(Sepal.Length, Sepal.Width, color = Species)
  )
  vdiffr::expect_doppelganger("custom", p)
  expect_true(inherits(p, "gtable"))
  expect_true(inherits(p, "gTree"))
  expect_true(inherits(p, "grob"))

  # Use within ggpairs
  expect_silent({
    pm <- ggpairs(
      iris,
      1:2,
      mapping = ggplot2::aes(color = Species),
      upper = list(continuous = gglegend("points"))
    )
  })
  vdiffr::expect_doppelganger("legend-pm", pm)

  # Use within ggpairs
  expect_silent({
    pm <- ggpairs(
      iris,
      1:2,
      mapping = ggplot2::aes(color = Species)
    )
    pm[1, 2] <- points_legend(
      iris,
      ggplot2::aes(Sepal.Width, Sepal.Length, color = Species)
    )
  })
  vdiffr::expect_doppelganger("internal-legend", pm)
})

test_that("plotNew", {
  points_legend <- gglegend(ggally_points)
  vdiffr::expect_doppelganger(
    "plotNew-default",
    points_legend(
      iris,
      ggplot2::aes(Sepal.Length, Sepal.Width, color = Species)
    )
  )
  expect_silent(
    print(
      points_legend(
        iris,
        ggplot2::aes(Sepal.Length, Sepal.Width, color = Species)
      ),
      plotNew = TRUE
    )
  )
})
