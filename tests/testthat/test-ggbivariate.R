context("ggbivariate")

test_that("example", {
  data(tips)

  p <- ggbivariate(tips, "smoker", c("day", "time", "sex", "tip"))
  vdiffr::expect_doppelganger("tips", p)

  # Personalize plot title and legend title
  p <- ggbivariate(
    tips, "smoker", c("day", "time", "sex", "tip"),
    title = "Custom title"
  ) +
    labs(fill = "Smoker ?")
  vdiffr::expect_doppelganger("tips-title", p)

  # Customize fill colour scale
  p <- ggbivariate(tips, "smoker", c("day", "time", "sex", "tip")) +
    scale_fill_brewer(type = "qual")
  vdiffr::expect_doppelganger("tips-fill-qual", p)


  # Customize labels
  p <- ggbivariate(
    tips, "smoker", c("day", "time", "sex", "tip"),
    rowbar_args = list(
      colour = "white",
      size = 4,
      fontface = "bold",
      label_format = scales::label_percent(accurary = 1)
    )
  )
  vdiffr::expect_doppelganger("tips-rowbar", p)

  # Choose the sub-plot from which get legend
  p <- ggbivariate(tips, "smoker")
  vdiffr::expect_doppelganger("tips-legend-default", p)

  ggbivariate(tips, "smoker", legend = 3)
  vdiffr::expect_doppelganger("tips-legend-3", p)

  # Use mapping to indicate weights
  d <- as.data.frame(Titanic)
  p <- ggbivariate(d, "Survived", mapping = aes(weight = Freq))
  vdiffr::expect_doppelganger("titanic-weight-freq", p)

  # outcome can be numerical
  p <- ggbivariate(tips, outcome = "tip", title = "tip")
  vdiffr::expect_doppelganger("tips-numeric", p)
})
