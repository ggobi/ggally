# add

    Code
      pm + 3
    Condition
      Error in `+.ggmatrix`:
      ! `ggmatrix()` does not know how to add objects that do not have class <theme>, <labels> or <ggproto>.
      i Received object with class: <numeric>

# v1_ggmatrix_theme

    Code
      pm <- ggpairs(tips, 1:2)
      pm1 <- pm + v1_ggmatrix_theme()
      expect_true(is.null(pm$gg))
      expect_true(!is.null(pm1$gg))

