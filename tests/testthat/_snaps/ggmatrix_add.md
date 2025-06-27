# v1_ggmatrix_theme

    Code
      pm <- ggpairs(tips, 1:2)
      pm1 <- pm + v1_ggmatrix_theme()
      expect_true(is.null(pm$gg))
      expect_true(!is.null(pm1$gg))

