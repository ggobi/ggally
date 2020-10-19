context("stat_cross")

test_that("example", {
  expect_print <- function(x) {
    expect_silent(print(x))
  }
  d <- as.data.frame(Titanic)

  # plot number of observations
  expect_print(ggplot(d) +
       aes(x = Class, y = Survived, weight = Freq, size = after_stat(observed)) +
       stat_cross() +
       scale_size_area(max_size = 20))

  # custom shape and fill colour based on chi-squared residuals
  expect_print(ggplot(d) +
       aes(
         x = Class, y = Survived, weight = Freq,
         size = after_stat(observed), fill = after_stat(std.resid)
       ) +
       stat_cross(shape = 22) +
       scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE) +
       scale_size_area(max_size = 20))

  # plotting the number of observations as a table
  expect_print(ggplot(d) +
       aes(
         x = Class, y = Survived, weight = Freq, label = after_stat(observed)
       ) +
       geom_text(stat = "cross"))

  # Row proportions with standardized residuals
  expect_print(ggplot(d) +
       aes(
         x = Class, y = Survived, weight = Freq,
         label = scales::percent(after_stat(row.prop)),
         size = NULL, fill = after_stat(std.resid)
       ) +
       stat_cross(shape = 22, size = 30) +
       geom_text(stat = "cross") +
       scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE) +
       facet_grid(Sex ~ .) +
       labs(fill = "Standardized residuals") +
       theme_minimal())

  # ggally_cross
  skip_if_not_installed("reshape")
  data(tips, package = "reshape")

  expect_print(ggally_cross(tips, mapping = aes(x = smoker, y = sex)))
  expect_print(ggally_cross(tips, mapping = aes(x = day, y = time)))

  # Custom fill
  expect_print(ggally_cross(tips, mapping = aes(x = smoker, y = sex), fill = "red"))

  # Custom shape
  expect_print(ggally_cross(tips, mapping = aes(x = smoker, y = sex), shape = 21))

  # Fill squares according to standardized residuals
  d <- as.data.frame(Titanic)
  expect_print(ggally_cross(
    d,
    mapping = aes(x = Class, y = Survived, weight = Freq, fill = after_stat(std.resid))
  ) +
    scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE))

  # Add labels
  expect_print(ggally_cross(
    tips,
    mapping = aes(
      x = smoker, y = sex, colour = smoker,
      label = scales::percent(after_stat(prop))
    )
  ))

  # Customize labels' appearance and same size for all squares
  expect_print(ggally_cross(
    tips,
    mapping = aes(
      x = smoker, y = sex,
      size = NULL, # do not map size to a variable
      label = scales::percent(after_stat(prop))
    ),
    size = 40, # fix value for points size
    fill = "darkblue",
    geom_text_args = list(colour = "white", fontface = "bold", size = 6)
  ))


  expect_print(ggally_table(tips, mapping = aes(x = smoker, y = sex)))
  expect_print(ggally_table(tips, mapping = aes(x = day, y = time)))
  expect_print(ggally_table(tips, mapping = aes(x = smoker, y = sex, colour = smoker)))

  # colour is kept only if equal to x or y
  expect_print(ggally_table(tips, mapping = aes(x = smoker, y = sex, colour = day)))

  # diagonal version
  expect_print(ggally_tableDiag(tips, mapping = aes(x = smoker)))

  # custom label size and color
  expect_print(ggally_table(tips, mapping = aes(x = smoker, y = sex), size = 16, color = "red"))

  # display column proportions
  expect_print(ggally_table(
    tips,
    mapping = aes(x = day, y = sex, label = scales::percent(after_stat(col.prop)))
  ))

  # draw table cells
  expect_print(ggally_table(
    tips,
    mapping = aes(x = smoker, y = sex),
    geom_tile_args = list(colour = "black", fill = "white")
  ))

  # Use standardized residuals to fill table cells
  expect_print(ggally_table(
    as.data.frame(Titanic),
    mapping = aes(
      x = Class, y = Survived, weight = Freq,
      fill = after_stat(std.resid),
      label = scales::percent(after_stat(col.prop), accuracy = .1)
    ),
    geom_tile_args = list(colour = "black")
  ) +
    scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE))


})
