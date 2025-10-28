# stops

    Code
      ggmatrix(plots = matrix(), nrow = 2, ncol = 3)
    Condition
      Error in `ggmatrix()`:
      ! `plots` must be a `list()`

---

    Code
      ggmatrix(plots = list(), nrow = "2", ncol = 3)
    Condition
      Error in `check_nrow_ncol()`:
      ! `nrow` must be a numeric value

---

    Code
      ggmatrix(plots = list(), nrow = 2, ncol = "3")
    Condition
      Error in `check_nrow_ncol()`:
      ! `ncol` must be a numeric value

---

    Code
      ggmatrix(plots = list(), nrow = c(2, 3), ncol = 3)
    Condition
      Error in `check_nrow_ncol()`:
      ! `nrow` must be a single numeric value

---

    Code
      ggmatrix(plots = list(), nrow = 2, ncol = c(2, 3))
    Condition
      Error in `check_nrow_ncol()`:
      ! `ncol` must be a single numeric value

# expression labels

    Code
      print(ggpairs(tips, 1:2, columnLabels = expression(alpha, beta)))
    Condition
      Error in `get_labels()`:
      ! `xAxisLabels` can only be a character vector or `NULL`.
      i Character values can be parsed using the `labeller` parameter.

# blank

    Code
      pm[2, 2] <- "not blank"
    Condition
      Error in `putPlot()`:
      ! character values (besides `'blank'`) are not allowed to be stored as plot values.

# ggmatrix proportions

    Code
      ggmatrix_proportions("not auto", tips, 1:ncol(tips))
    Condition
      Error in `ggmatrix_proportions()`:
      ! `proportions` need to be non-NA numeric values or `'auto'`. proportions: "not auto"

---

    Code
      ggmatrix_proportions(NA, tips, 1:ncol(tips))
    Condition
      Error in `ggmatrix_proportions()`:
      ! `proportions` need to be non-NA numeric values or `'auto'`. proportions: NA

---

    Code
      ggmatrix_proportions(c(1, NA, 1, 1, 1, 1, 1), tips, 1:ncol(tips))
    Condition
      Error in `ggmatrix_proportions()`:
      ! `proportions` need to be non-NA numeric values or `'auto'`. proportions: c(1, NA, 1, 1, 1, 1, 1)

