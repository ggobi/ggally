# rows work

    Code
      ggmatrix_location(pm, rows = TRUE)
    Condition
      Error in `ggmatrix_location()`:
      ! `rows` must be numeric

---

    Code
      ggmatrix_location(pm, rows = "1")
    Condition
      Error in `ggmatrix_location()`:
      ! `rows` must be numeric

# cols work

    Code
      ggmatrix_location(pm, cols = TRUE)
    Condition
      Error in `ggmatrix_location()`:
      ! `cols` must be numeric

---

    Code
      ggmatrix_location(pm, cols = "1")
    Condition
      Error in `ggmatrix_location()`:
      ! `cols` must be numeric

# location character

    Code
      ggmatrix_location(pm, location = "unknown")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "all", "upper", "lower", "diag", "none"

# location matrix

    Code
      ggmatrix_location(pm, location = mat[, 1:6])
    Condition
      Error in `ggmatrix_location()`:
      ! `location` provided does not have the same size of columns

---

    Code
      ggmatrix_location(pm, location = mat[1:6, ])
    Condition
      Error in `ggmatrix_location()`:
      ! `location` provided does not have the same size of rows

---

    Code
      ggmatrix_location(pm, location = cbind(mat, 1))
    Condition
      Error in `ggmatrix_location()`:
      ! `location` provided does not have the same size of columns

---

    Code
      ggmatrix_location(pm, location = rbind(mat, 1))
    Condition
      Error in `ggmatrix_location()`:
      ! `location` provided does not have the same size of rows

---

    Code
      ggmatrix_location(pm, location = expand.grid(row = 1:7, col = 2:8))
    Condition
      Error in `ggmatrix_location()`:
      ! `col` must be non-NA / positive numeric values `<= pm$ncol`
      * `pm$ncol`: 7
      * `col`: 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, ..., 8, and 8

---

    Code
      ggmatrix_location(pm, location = expand.grid(row = 2:8, col = 1:7))
    Condition
      Error in `ggmatrix_location()`:
      ! `row` must be non-NA / positive numeric values `<= pm$nrow`
      * `pm$nrow`: 7
      * row: 2, 3, 4, 5, 6, 7, 8, 2, 3, 4, 5, 6, 7, 8, 2, 3, 4, 5, ..., 7, and 8

---

    Code
      ggmatrix_location(pm, location = expand.grid(row = 1:7, col = 0:6))
    Condition
      Error in `ggmatrix_location()`:
      ! `col` must be non-NA / positive numeric values `<= pm$ncol`
      * `pm$ncol`: 7
      * `col`: 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, ..., 6, and 6

---

    Code
      ggmatrix_location(pm, location = expand.grid(row = 0:6, col = 1:7))
    Condition
      Error in `ggmatrix_location()`:
      ! `row` must be non-NA / positive numeric values `<= pm$nrow`
      * `pm$nrow`: 7
      * row: 0, 1, 2, 3, 4, 5, 6, 0, 1, 2, 3, 4, 5, 6, 0, 1, 2, 3, ..., 5, and 6

---

    Code
      ggmatrix_location(pm, location = expand.grid(row = 1:7, col = c(1:6, NA)))
    Condition
      Error in `ggmatrix_location()`:
      ! `col` must be non-NA / positive numeric values `<= pm$ncol`
      * `pm$ncol`: 7
      * `col`: 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, ..., NA, and NA

---

    Code
      ggmatrix_location(pm, location = expand.grid(row = c(1:6, NA), col = 1:7))
    Condition
      Error in `ggmatrix_location()`:
      ! `row` must be non-NA / positive numeric values `<= pm$nrow`
      * `pm$nrow`: 7
      * row: 1, 2, 3, 4, 5, 6, NA, 1, 2, 3, 4, 5, 6, NA, 1, 2, 3, 4, ..., 6, and NA

