context("stat_prop")

test_that("example", {
  expect_print <- function(x) {
    expect_silent(print(x))
  }
  d <- as.data.frame(Titanic)

  p <- ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq, by = Class) +
    geom_bar(position = "fill") +
    geom_text(stat = "prop", position = position_fill(.5))
  expect_print(p)
  expect_print(p + facet_grid(~Sex))

  expect_print(ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq) +
    geom_bar(position = "dodge") +
    geom_text(
      aes(by = Survived),
      stat = "prop",
      position = position_dodge(0.9), vjust = "bottom"
    ))

  expect_print(ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq, by = 1) +
    geom_bar() +
    geom_text(
      aes(label = scales::percent(after_stat(prop), accuracy = 1)),
      stat = "prop",
      position = position_stack(.5)
    ))

  skip_if_not_installed("reshape")
  data(tips, package = "reshape")

  expect_print(ggally_colbar(tips, mapping = aes(x = smoker, y = sex)))
  expect_print(ggally_rowbar(tips, mapping = aes(x = smoker, y = sex)))

  # change labels' size
  expect_print(ggally_colbar(tips, mapping = aes(x = smoker, y = sex), size = 8))

  # change labels' colour and use bold
  expect_print(ggally_colbar(tips, mapping = aes(x = smoker, y = sex),
                   colour = "white", fontface = "bold"))

  # display number of observations instead of proportions
  expect_print(ggally_colbar(tips, mapping = aes(x = smoker, y = sex, label = after_stat(count))))

  # custom bar width
  expect_print(ggally_colbar(tips, mapping = aes(x = smoker, y = sex), geom_bar_args = list(width = .5)))

  # change format of labels
  expect_print(ggally_colbar(tips, mapping = aes(x = smoker, y = sex),
                   label_format = scales::label_percent(accuracy = .01, decimal.mark = ",")))

  expect_print(ggduo(
    data = as.data.frame(Titanic),
    mapping = aes(weight = Freq),
    columnsX = "Survived",
    columnsY = c("Sex", "Class", "Age"),
    types = list(discrete = "rowbar"),
    legend = 1
  ))
})

test_that("stat_prop() works with an y aesthetic", {
  expect_print <- function(x) {
    expect_silent(print(x))
  }

  d <- as.data.frame(Titanic)
  p <- ggplot(d) +
    aes(y = Class, fill = Survived, weight = Freq, by = Class) +
    geom_bar(position = "fill") +
    geom_text(stat = "prop", position = position_fill(.5))
  expect_print(p)
})
