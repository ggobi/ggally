context("ggbivariate")

test_that("example", {
  expect_print <- function(x) {
    expect_silent(print(x))
  }
  skip_if_not_installed("reshape")
  data(tips, package = "reshape")

  expect_print(ggbivariate(tips, "smoker", c("day", "time", "sex", "tip")))

  # Personalize plot title and legend title
  expect_print(ggbivariate(
    tips, "smoker", c("day", "time", "sex", "tip"),
    title = "Custom title"
  ) +
    labs(fill = "Smoker ?"))

  # Customize fill colour scale
  expect_print(ggbivariate(tips, "smoker", c("day", "time", "sex", "tip")) +
    scale_fill_brewer(type = "qual"))

  # Customize labels
  expect_print(ggbivariate(
    tips, "smoker", c("day", "time", "sex", "tip"),
    rowbar_args = list(
      colour = "white",
      size = 4,
      fontface = "bold",
      label_format = scales::label_percent(accurary = 1)
    )
  ))

  # Choose the sub-plot from which get legend
  expect_print(ggbivariate(tips, "smoker"))
  expect_print(ggbivariate(tips, "smoker", legend = 3))

  # Use mapping to indicate weights
  d <- as.data.frame(Titanic)
  expect_print(ggbivariate(d, "Survived", mapping = aes(weight = Freq)))

  # outcome can be numerical
  expect_print(ggbivariate(tips, outcome = "tip", title = "tip"))
})
