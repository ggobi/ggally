
context("ggcoef")

suppressMessages(require(broom))

test_that("example", {
  expect_print <- function(x) {
    expect_silent(print(x))
  }
  reg <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
  expect_print(ggcoef(reg))

  skip_if_not_installed("MASS")
  d <- as.data.frame(Titanic)
  reg2 <- glm(Survived ~ Sex + Age + Class, family = binomial, data = d, weights = d$Freq)
  expect_print(ggcoef(reg2, exponentiate = TRUE))
  expect_print(ggcoef(
    reg2,
    exponentiate = TRUE,
    exclude_intercept = TRUE,
    errorbar_height = .2,
    color = "blue"
  ))
})
