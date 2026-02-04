# Modify a [`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md) object by adding an ggplot2 object to all plots

This operator allows you to add ggplot2 objects to a
[`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md)
object.

## Usage

``` r
add_to_ggmatrix(e1, e2, location = NULL, rows = NULL, cols = NULL)
```

## Arguments

- e1:

  An object of class
  [`ggnostic`](https://ggobi.github.io/ggally/dev/reference/ggnostic.md)
  or `ggplot`

- e2:

  A component to add to `e1`

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

## Details

If the first object is an object of class
[`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md),
you can add the following types of objects, and it will return a
modified ggplot2 object.

- `theme`: update plot theme

- `scale`: replace current scale

- `coord`: override current coordinate system

The `+` operator completely replaces elements with elements from e2.

`add_to_ggmatrix` gives you more control to modify only some subplots.
This function may be replaced and/or removed in the future.
**\[experimental\]**

## See also

[`ggmatrix_location`](https://ggobi.github.io/ggally/dev/reference/ggmatrix_location.md)

## Examples

``` r
# small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive
data(tips)

pm <- ggpairs(tips[, 2:4], ggplot2::aes(color = sex))
## change to black and white theme
pm + ggplot2::theme_bw()
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

## change to linedraw theme
p_(pm + ggplot2::theme_linedraw())
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

## change to custom theme
p_(pm + ggplot2::theme(panel.background = ggplot2::element_rect(fill = "lightblue")))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

## add a list of information
extra <- list(ggplot2::theme_bw(), ggplot2::labs(caption = "My caption!"))
p_(pm + extra)
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.


## modify scale
p_(pm + scale_fill_brewer(type = "qual"))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

## only first row
p_(add_to_ggmatrix(pm, scale_fill_brewer(type = "qual"), rows = 1:2))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

## only second col
p_(add_to_ggmatrix(pm, scale_fill_brewer(type = "qual"), cols = 2:3))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

## only to upper triangle of plot matrix
p_(add_to_ggmatrix(
  pm,
  scale_fill_brewer(type = "qual"),
  location = "upper"
))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```
