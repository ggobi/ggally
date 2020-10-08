context("ggtable")

suppressMessages(require(broom))

test_that("example", {
  expect_print <- function(x) {
    expect_silent(print(x))
  }
  reg <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
  expect_print(ggcoef(reg))

  skip_if_not_installed("reshape")
  data(tips, package = "reshape")
  expect_print(ggtable(tips, "smoker", c("day", "time", "sex")))

  # displaying row proportions
  expect_print(ggtable(tips, "smoker", c("day", "time", "sex"), cells = "row.prop"))

  # filling cells with residuals
  expect_print(ggtable(tips, "smoker", c("day", "time", "sex"), fill = "std.resid", legend = 1))
  expect_print(ggtable(tips, "smoker", c("day", "time", "sex"), fill = "resid", legend = 1))

  # if continuous variables are provided, just displaying some summary statistics
  expect_print(ggtable(tips, c("smoker", "total_bill"), c("day", "time", "sex", "tip")))

  # specifying weights
  d <- as.data.frame(Titanic)
  expect_print(ggtable(
    d,
    "Survived",
    c("Class", "Sex", "Age"),
    mapping = aes(weight = Freq),
    cells = "row.prop",
    fill = "std.resid"
  ))
})
