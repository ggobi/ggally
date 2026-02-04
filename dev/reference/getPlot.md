# Subset a [`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md) object

Retrieves the ggplot object at the desired location.

## Usage

``` r
getPlot(pm, i, j)

# S3 method for class 'ggmatrix'
pm[i, j, ...]
```

## Arguments

- pm:

  [`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md)
  object to select from

- i:

  row from the top

- j:

  column from the left

- ...:

  ignored

## See also

[`putPlot`](https://ggobi.github.io/ggally/dev/reference/putPlot.md)

## Author

Barret Schloerke

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

data(tips)
plotMatrix2 <- ggpairs(tips[, 3:2], upper = list(combo = "denstrip"))
p_(plotMatrix2[1, 2])
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```
