# v1_ggmatrix_theme

    Code
      pm <- ggpairs(tips, 1:2)
      pm1 <- pm + v1_ggmatrix_theme()
    Condition
      Warning:
      `v1_ggmatrix_theme()` was deprecated in GGally 2.3.0.
      i This function will be removed in future releases.
    Code
      expect_true(is.null(pm$gg))
      expect_true(!is.null(pm1$gg))

