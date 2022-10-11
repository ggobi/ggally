context("stat_cross")

test_that("example", {
  d <- as.data.frame(Titanic)

  # plot number of observations
  p <-
    ggplot(d) +
    aes(x = Class, y = Survived, weight = Freq, size = after_stat(observed)) +
    stat_cross() +
    scale_size_area(max_size = 20)
  vdiffr::expect_doppelganger("ex", p)

  # custom shape and fill colour based on chi-squared residuals
  p <-
    ggplot(d) +
    aes(
      x = Class, y = Survived, weight = Freq,
      size = after_stat(observed), fill = after_stat(std.resid)
    ) +
    stat_cross(shape = 22) +
    scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE) +
    scale_size_area(max_size = 20)
  vdiffr::expect_doppelganger("shape-22", p)

  # plotting the number of observations as a table
  p <-
    ggplot(d) +
    aes(
      x = Class, y = Survived, weight = Freq, label = after_stat(observed)
    ) +
    geom_text(stat = "cross")
  vdiffr::expect_doppelganger("label", p)

  # Row proportions with standardized residuals
  p <-
    ggplot(d) +
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
    theme_minimal()
  vdiffr::expect_doppelganger("row-prop", p)

  # ggally_cross
  skip_if_not_installed("reshape")
  data(tips, package = "reshape")

  # Custom fill
  vdiffr::expect_doppelganger("tips-fill-red", ggally_cross(tips, mapping = aes(x = smoker, y = sex), fill = "red"))

  # Custom shape
  vdiffr::expect_doppelganger("tips-shape", ggally_cross(tips, mapping = aes(x = smoker, y = sex), shape = 21))

  # Fill squares according to standardized residuals
  d <- as.data.frame(Titanic)
  vdiffr::expect_doppelganger(
    "titanic-fill-steps2",
    ggally_cross(
      d,
      mapping = aes(x = Class, y = Survived, weight = Freq, fill = after_stat(std.resid))
    ) +
      scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE)
  )

  # Add labels
  vdiffr::expect_doppelganger(
    "tips-label",
    ggally_cross(
      tips,
      mapping = aes(
        x = smoker, y = sex, colour = smoker,
        label = scales::percent(after_stat(prop))
      )
    )
  )

  # Customize labels' appearance and same size for all squares
  vdiffr::expect_doppelganger(
    "tips-label-custom",
    ggally_cross(
      tips,
      mapping = aes(
        x = smoker, y = sex,
        size = NULL, # do not map size to a variable
        label = scales::percent(after_stat(prop))
      ),
      size = 40, # fix value for points size
      fill = "darkblue",
      geom_text_args = list(colour = "white", fontface = "bold", size = 6)
    )
  )


  vdiffr::expect_doppelganger("tips-sex", ggally_cross(tips, mapping = aes(x = smoker, y = sex)))
  vdiffr::expect_doppelganger("tips-time", ggally_cross(tips, mapping = aes(x = day, y = time)))
  vdiffr::expect_doppelganger("tips-time-color", ggally_table(tips, mapping = aes(x = smoker, y = sex, colour = smoker)))

  # colour is kept only if equal to x or y
  vdiffr::expect_doppelganger("tips-color-equal", ggally_table(tips, mapping = aes(x = smoker, y = sex, colour = day)))

  # diagonal version
  vdiffr::expect_doppelganger("tips-diagonal", ggally_tableDiag(tips, mapping = aes(x = smoker)))

  # custom label size and color
  vdiffr::expect_doppelganger("tips-red-size16", ggally_table(tips, mapping = aes(x = smoker, y = sex), size = 16, color = "red"))

  # display column proportions
  vdiffr::expect_doppelganger("table-label", ggally_table(
    tips,
    mapping = aes(x = day, y = sex, label = scales::percent(after_stat(col.prop)))
  ))

  # draw table cells
  vdiffr::expect_doppelganger("table-color-fill", ggally_table(
    tips,
    mapping = aes(x = smoker, y = sex),
    geom_tile_args = list(colour = "black", fill = "white")
  ))

  # Use standardized residuals to fill table cells
  vdiffr::expect_doppelganger("table-fill-steps2", ggally_table(
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
