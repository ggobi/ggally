context("stat_prop")

test_that("example", {
  d <- as.data.frame(Titanic)

  p <- ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq, by = Class) +
    geom_bar(position = "fill") +
    geom_text(stat = "prop", position = position_fill(.5))
  vdiffr::expect_doppelganger("titanic", p)
  vdiffr::expect_doppelganger("titanic-facet", p + facet_grid(~Sex))

  vdiffr::expect_doppelganger("titanic-dodge", ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq) +
    geom_bar(position = "dodge") +
    geom_text(
      aes(by = Survived),
      stat = "prop",
      position = position_dodge(0.9), vjust = "bottom"
    ))

  vdiffr::expect_doppelganger("titanic-stack", ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq, by = 1) +
    geom_bar() +
    geom_text(
      aes(label = scales::percent(after_stat(prop), accuracy = 1)),
      stat = "prop",
      position = position_stack(.5)
    ))

  skip_if_not_installed("reshape")
  data(tips, package = "reshape")

  vdiffr::expect_doppelganger("tips", ggally_rowbar(tips, mapping = aes(x = smoker, y = sex)))

  # change labels' size
  vdiffr::expect_doppelganger("tips-size8", ggally_colbar(tips, mapping = aes(x = smoker, y = sex), size = 8))

  # change labels' colour and use bold
  vdiffr::expect_doppelganger("tips-color-white", ggally_colbar(tips, mapping = aes(x = smoker, y = sex),
                   colour = "white", fontface = "bold"))

  # display number of observations instead of proportions
  vdiffr::expect_doppelganger("tips-label", ggally_colbar(tips, mapping = aes(x = smoker, y = sex, label = after_stat(count))))

  # custom bar width
  vdiffr::expect_doppelganger("tips-bar-width", ggally_colbar(tips, mapping = aes(x = smoker, y = sex), geom_bar_args = list(width = .5)))

  # change format of labels
  vdiffr::expect_doppelganger("tips-label-custom", ggally_colbar(tips, mapping = aes(x = smoker, y = sex),
                   label_format = scales::label_percent(accuracy = .01, decimal.mark = ",")))

  vdiffr::expect_doppelganger("ggduo-titanic", ggduo(
    data = as.data.frame(Titanic),
    mapping = aes(weight = Freq),
    columnsX = "Survived",
    columnsY = c("Sex", "Class", "Age"),
    types = list(discrete = "rowbar"),
    legend = 1
  ))
})

test_that("stat_prop() works with an y aesthetic", {
  d <- as.data.frame(Titanic)
  p <- ggplot(d) +
    aes(y = Class, fill = Survived, weight = Freq, by = Class) +
    geom_bar(position = "fill") +
    geom_text(stat = "prop", position = position_fill(.5))
  vdiffr::expect_doppelganger("titanic-stat-prop", p)
})
