# [`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md) plot locations

**\[experimental\]**

## Usage

``` r
ggmatrix_location(pm, location = NULL, rows = NULL, cols = NULL)
```

## Arguments

- pm:

  [`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md)
  plot object

- location:

  `"all"`, `TRUE`

  :   All row and col combinations

  `"none"`

  :   No row and column combinations

  `"upper"`

  :   Locations where the column value is higher than the row value

  `"lower"`

  :   Locations where the row value is higher than the column value

  `"diag"`

  :   Locations where the column value is equal to the row value

  `matrix` or `data.frame`

  :   `matrix` values will be converted into `data.frame`s.

      - A `data.frame` with the exact column names `c("row", "col")`

      - A `data.frame` with the number of rows and columns matching the
        plot matrix object provided. Each cell will be tested for a
        "truthy" value to determine if the location should be kept.

- rows:

  numeric vector of the rows to be used. Will be used with `cols` if
  `location` is `NULL`

- cols:

  numeric vector of the cols to be used. Will be used with `rows` if
  `location` is `NULL`

## Value

Data frame with columns `c("row", "col")` containing locations for the
plot matrix

## Details

Convert many types of location values to a consistent `data.frame` of
`row` and `col` values.

## Examples

``` r
pm <- ggpairs(tips, 1:3)

# All locations
ggmatrix_location(pm, location = "all")
#>   row col
#> 1   1   1
#> 2   2   1
#> 3   3   1
#> 4   1   2
#> 5   2   2
#> 6   3   2
#> 7   1   3
#> 8   2   3
#> 9   3   3
ggmatrix_location(pm, location = TRUE)
#>   row col
#> 1   1   1
#> 2   2   1
#> 3   3   1
#> 4   1   2
#> 5   2   2
#> 6   3   2
#> 7   1   3
#> 8   2   3
#> 9   3   3

# No locations
ggmatrix_location(pm, location = "none")
#> [1] row col
#> <0 rows> (or 0-length row.names)

# "upper" triangle locations
ggmatrix_location(pm, location = "upper")
#>   row col
#> 4   1   2
#> 7   1   3
#> 8   2   3

# "lower" triangle locations
ggmatrix_location(pm, location = "lower")
#>   row col
#> 2   2   1
#> 3   3   1
#> 6   3   2

# "diag" locations
ggmatrix_location(pm, location = "diag")
#>   row col
#> 1   1   1
#> 5   2   2
#> 9   3   3

# specific rows
ggmatrix_location(pm, rows = 2)
#>   row col
#> 1   2   1
#> 2   2   2
#> 3   2   3

# specific columns
ggmatrix_location(pm, cols = 2)
#>   row col
#> 1   1   2
#> 2   2   2
#> 3   3   2

# row and column combinations
ggmatrix_location(pm, rows = c(1, 2), cols = c(1, 3))
#>   row col
#> 1   1   1
#> 2   2   1
#> 3   1   3
#> 4   2   3

# matrix locations
mat <- matrix(TRUE, ncol = 3, nrow = 3)
mat[1, 1] <- FALSE
locs <- ggmatrix_location(pm, location = mat)
## does not contain the 1, 1 cell
locs
#>   row col
#> 1   1   2
#> 2   1   3
#> 3   2   1
#> 4   2   2
#> 5   2   3
#> 6   3   1
#> 7   3   2
#> 8   3   3

# Use the output of a prior ggmatrix_location
ggmatrix_location(pm, location = locs)
#>   row col
#> 1   1   2
#> 2   1   3
#> 3   2   1
#> 4   2   2
#> 5   2   3
#> 6   3   1
#> 7   3   2
#> 8   3   3
```
