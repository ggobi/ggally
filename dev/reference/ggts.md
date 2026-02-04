# Multiple time series

GGally implementation of ts.plot. Wraps around the ggduo function and
removes the column strips

## Usage

``` r
ggts(..., columnLabelsX = NULL, xlab = "time")
```

## Arguments

- ...:

  supplied directly to
  [`ggduo`](https://ggobi.github.io/ggally/dev/reference/ggduo.md)

- columnLabelsX:

  remove top strips for the X axis by default

- xlab:

  defaults to "time"

## Value

[`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md)
object

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

p_(ggts(pigs, "time", c("gilts", "profit", "s_per_herdsz", "production", "herdsz")))
```
