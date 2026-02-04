# [`ggnostic`](https://ggobi.github.io/ggally/dev/reference/ggnostic.md) standardized residuals

If non-null `pVal` and `sigma` values are given, confidence interval
lines will be added to the plot at the specified `pVal` locations of a
N(0, 1) distribution.

## Usage

``` r
ggally_nostic_std_resid(data, mapping, ..., sigma = 1)
```

## Arguments

- data, mapping, ...:

  parameters supplied to
  [`ggally_nostic_resid`](https://ggobi.github.io/ggally/dev/reference/ggally_nostic_resid.md)

- sigma:

  sigma value for the `pVal` percentiles. Set to 1 for standardized
  residuals

## Value

ggplot2 plot object

## See also

[`stats::rstandard()`](https://rdrr.io/r/stats/influence.measures.html)

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

if (require(broom)) {
  dt <- broomify(stats::lm(mpg ~ wt + qsec + am, data = mtcars))
  p_(ggally_nostic_std_resid(dt, ggplot2::aes(wt, .std.resid)))
}
#> `geom_smooth()` using method = 'loess'
```
