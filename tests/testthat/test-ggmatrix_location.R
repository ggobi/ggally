expect_loc_grid <- function(loc, to_loc) {
  testthat::expect_equal(
    colnames(loc),
    colnames(to_loc)
  )
  testthat::expect_equal(
    nrow(loc),
    nrow(to_loc)
  )
  loc <- loc[order(loc$row, loc$col), ]
  to_loc <- to_loc[order(to_loc$row, to_loc$col), ]
  testthat::expect_equal(
    loc$row,
    to_loc$row
  )
  testthat::expect_equal(
    loc$col,
    to_loc$col
  )
}
expect_rows_cols <- function(loc, rows, cols) {
  to_loc <- expand.grid(row = rows, col = cols)
  expect_loc_grid(loc, to_loc)
}

test_that("rows work", {
  pm <- ggpairs(tips)

  expect_rows_cols(
    ggmatrix_location(pm, rows = c(3, 5)),
    rows = c(3, 5),
    cols = 1:7
  )
  expect_rows_cols(
    ggmatrix_location(pm, rows = 1),
    rows = 1,
    cols = 1:7
  )

  expect_snapshot(ggmatrix_location(pm, rows = TRUE), error = TRUE)
  expect_snapshot(ggmatrix_location(pm, rows = "1"), error = TRUE)
})


test_that("cols work", {
  pm <- ggpairs(tips)

  expect_rows_cols(
    ggmatrix_location(pm, cols = c(3, 5)),
    rows = 1:7,
    cols = c(3, 5)
  )
  expect_rows_cols(
    ggmatrix_location(pm, cols = 1),
    rows = 1:7,
    cols = 1
  )

  expect_snapshot(ggmatrix_location(pm, cols = TRUE), error = TRUE)
  expect_snapshot(ggmatrix_location(pm, cols = "1"), error = TRUE)
})


test_that("location logical", {
  pm <- ggpairs(tips)

  expect_loc_grid(
    ggmatrix_location(pm, location = TRUE),
    expand.grid(row = 1:7, col = 1:7)
  )
  expect_warning(
    ggmatrix_location(pm, location = FALSE)
  )
})

test_that("location character", {
  pm <- ggpairs(tips)
  to_loc <- expand.grid(row = 1:7, col = 1:7)

  expect_loc_grid(
    ggmatrix_location(pm, location = "all"),
    to_loc
  )
  expect_loc_grid(
    ggmatrix_location(pm, location = "none"),
    subset(to_loc, FALSE)
  )
  expect_loc_grid(
    ggmatrix_location(pm, location = "upper"),
    subset(to_loc, col > row)
  )
  expect_loc_grid(
    ggmatrix_location(pm, location = "lower"),
    subset(to_loc, col < row)
  )
  expect_loc_grid(
    ggmatrix_location(pm, location = "diag"),
    subset(to_loc, col == row)
  )

  expect_snapshot(ggmatrix_location(pm, location = "unknown"), error = TRUE)
})


test_that("location matrix", {
  pm <- ggpairs(tips)
  to_loc <- subset(
    expand.grid(row = 1:7, col = 1:7),
    row %in% c(3, 5) | col %in% c(3, 5)
  )

  mat <- matrix(FALSE, nrow = 7, ncol = 7, byrow = TRUE)
  mat[, c(3, 5)] <- TRUE
  mat[c(3, 5), ] <- TRUE

  expect_loc_grid(
    ggmatrix_location(pm, location = mat),
    to_loc
  )
  expect_loc_grid(
    ggmatrix_location(pm, location = as.data.frame(mat)),
    to_loc
  )

  mat2 <- mat
  mat2[TRUE] <- FALSE
  expect_loc_grid(
    ggmatrix_location(pm, location = mat2),
    subset(to_loc, FALSE)
  )

  expect_snapshot(ggmatrix_location(pm, location = mat[, 1:6]), error = TRUE)
  expect_snapshot(ggmatrix_location(pm, location = mat[1:6, ]), error = TRUE)
  expect_snapshot(ggmatrix_location(pm, location = cbind(mat, 1)), error = TRUE)
  expect_snapshot(ggmatrix_location(pm, location = rbind(mat, 1)), error = TRUE)
})


test_that("location matrix", {
  pm <- ggpairs(tips)
  to_loc <- expand.grid(row = 1:7, col = 1:7)

  expect_loc_grid(
    ggmatrix_location(pm),
    expand.grid(row = 1:7, col = 1:7)
  )

  expect_snapshot(
    ggmatrix_location(pm, location = expand.grid(row = 1:7, col = 2:8)),
    error = TRUE
  )
  expect_snapshot(
    ggmatrix_location(pm, location = expand.grid(row = 2:8, col = 1:7)),
    error = TRUE
  )

  expect_snapshot(
    ggmatrix_location(pm, location = expand.grid(row = 1:7, col = 0:6)),
    error = TRUE
  )
  expect_snapshot(
    ggmatrix_location(pm, location = expand.grid(row = 0:6, col = 1:7)),
    error = TRUE
  )

  expect_snapshot(
    ggmatrix_location(pm, location = expand.grid(row = 1:7, col = c(1:6, NA))),
    error = TRUE
  )
  expect_snapshot(
    ggmatrix_location(pm, location = expand.grid(row = c(1:6, NA), col = 1:7)),
    error = TRUE
  )
})


test_that("location recursion", {
  pm <- ggpairs(tips)
  to_loc <- expand.grid(row = 1:7, col = 1:7)

  expect_loc_grid(
    ggmatrix_location(pm),
    expand.grid(row = 1:7, col = 1:7)
  )

  expect_loc_grid(
    ggmatrix_location(pm, location = ggmatrix_location(pm)),
    expand.grid(row = 1:7, col = 1:7)
  )
})
