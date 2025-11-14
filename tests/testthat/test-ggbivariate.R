test_that("example", {
  data(tips)

  p <- ggbivariate(tips, "smoker", c("day", "time", "sex", "tip"))
  ggally_expect_doppelganger("tips", p)

  # Personalize plot title and legend title
  p <- ggbivariate(
    tips,
    "smoker",
    c("day", "time", "sex", "tip"),
    title = "Custom title"
  ) +
    labs(fill = "Smoker ?")
  ggally_expect_doppelganger("tips-title", p)

  # Customize fill colour scale
  p <- ggbivariate(tips, "smoker", c("day", "time", "sex", "tip")) +
    scale_fill_brewer(type = "qual")
  ggally_expect_doppelganger("tips-fill-qual", p)

  # Customize labels
  p <- ggbivariate(
    tips,
    "smoker",
    c("day", "time", "sex", "tip"),
    rowbar_args = list(
      colour = "white",
      size = 4,
      fontface = "bold",
      label_format = scales::label_percent(accurary = 1)
    )
  )
  ggally_expect_doppelganger("tips-rowbar", p)

  # Choose the sub-plot from which get legend
  p <- ggbivariate(tips, "smoker")
  ggally_expect_doppelganger("tips-legend-default", p)

  ggbivariate(tips, "smoker", legend = 3)
  ggally_expect_doppelganger("tips-legend-3", p)

  # Use mapping to indicate weights
  d <- as.data.frame(Titanic)
  p <- ggbivariate(d, "Survived", mapping = aes(weight = Freq))
  ggally_expect_doppelganger("titanic-weight-freq", p)

  # outcome can be numerical
  p <- ggbivariate(tips, outcome = "tip", title = "tip")
  ggally_expect_doppelganger("tips-numeric", p)
})
