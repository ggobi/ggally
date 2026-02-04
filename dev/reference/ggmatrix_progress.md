# [`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md) default progress bar

[`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md)
default progress bar

## Usage

``` r
ggmatrix_progress(
  format = " plot: [:plot_i, :plot_j] [:bar]:percent est::eta ",
  clear = TRUE,
  show_after = 0,
  ...
)
```

## Arguments

- format, clear, show_after, ...:

  parameters supplied directly to
  `progress::`[`progress_bar`](http://r-lib.github.io/progress/reference/progress_bar.md)`$new()`

## Value

function that accepts a plot matrix as the first argument and `...` for
future expansion. Internally, the plot matrix is used to determine the
total number of plots for the progress bar.

## Examples

``` r
p_ <- GGally::print_if_interactive

pm <- ggpairs(iris, 1:2, progress = ggmatrix_progress())
p_(pm)


# does not clear after finishing
pm <- ggpairs(iris, 1:2, progress = ggmatrix_progress(clear = FALSE))
p_(pm)
```
