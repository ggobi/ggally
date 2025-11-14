test_that("example", {
  # ggally_cross
  data(tips)

  # Custom fill
  ggally_expect_doppelganger(
    "tips-fill-red",
    ggally_cross(tips, mapping = aes(x = smoker, y = sex), fill = "red")
  )

  # Custom shape
  ggally_expect_doppelganger(
    "tips-shape",
    ggally_cross(tips, mapping = aes(x = smoker, y = sex), shape = 21)
  )

  # Fill squares according to standardized residuals
  d <- as.data.frame(Titanic)
  ggally_expect_doppelganger(
    "titanic-fill-steps2",
    ggally_cross(
      d,
      mapping = aes(
        x = Class,
        y = Survived,
        weight = Freq,
        fill = after_stat(std.resid)
      )
    ) +
      scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE)
  )

  # Add labels
  ggally_expect_doppelganger(
    "tips-label",
    ggally_cross(
      tips,
      mapping = aes(
        x = smoker,
        y = sex,
        colour = smoker,
        label = scales::percent(after_stat(prop))
      )
    )
  )

  # Customize labels' appearance and same size for all squares
  ggally_expect_doppelganger(
    "tips-label-custom",
    ggally_cross(
      tips,
      mapping = aes(
        x = smoker,
        y = sex,
        size = NULL, # do not map size to a variable
        label = scales::percent(after_stat(prop))
      ),
      size = 40, # fix value for points size
      fill = "darkblue",
      geom_text_args = list(colour = "white", fontface = "bold", size = 6)
    )
  )

  ggally_expect_doppelganger(
    "tips-sex",
    ggally_cross(tips, mapping = aes(x = smoker, y = sex))
  )
  ggally_expect_doppelganger(
    "tips-time",
    ggally_cross(tips, mapping = aes(x = day, y = time))
  )
  ggally_expect_doppelganger(
    "tips-time-color",
    ggally_table(tips, mapping = aes(x = smoker, y = sex, colour = smoker))
  )

  # colour is kept only if equal to x or y
  ggally_expect_doppelganger(
    "tips-color-equal",
    ggally_table(tips, mapping = aes(x = smoker, y = sex, colour = day))
  )

  # diagonal version
  ggally_expect_doppelganger(
    "tips-diagonal",
    ggally_tableDiag(tips, mapping = aes(x = smoker))
  )

  # custom label size and color
  ggally_expect_doppelganger(
    "tips-red-size16",
    ggally_table(
      tips,
      mapping = aes(x = smoker, y = sex),
      size = 16,
      color = "red"
    )
  )

  # display column proportions
  ggally_expect_doppelganger(
    "table-label",
    ggally_table(
      tips,
      mapping = aes(
        x = day,
        y = sex,
        label = scales::percent(after_stat(col.prop))
      )
    )
  )

  # draw table cells
  ggally_expect_doppelganger(
    "table-color-fill",
    ggally_table(
      tips,
      mapping = aes(x = smoker, y = sex),
      geom_tile_args = list(colour = "black", fill = "white")
    )
  )

  # Use standardized residuals to fill table cells
  ggally_expect_doppelganger(
    "table-fill-steps2",
    ggally_table(
      as.data.frame(Titanic),
      mapping = aes(
        x = Class,
        y = Survived,
        weight = Freq,
        fill = after_stat(std.resid),
        label = scales::percent(after_stat(col.prop), accuracy = .1)
      ),
      geom_tile_args = list(colour = "black")
    ) +
      scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE)
  )
})
