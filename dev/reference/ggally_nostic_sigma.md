# [`ggnostic`](https://ggobi.github.io/ggally/dev/reference/ggnostic.md) leave one out model sigma

A function to display
[`stats::influence()`](https://rdrr.io/r/stats/lm.influence.html)'s
sigma value.

## Usage

``` r
ggally_nostic_sigma(
  data,
  mapping,
  ...,
  lineColor = brew_colors("grey"),
  linePosition = attr(data, "broom_glance")$sigma
)
```

## Arguments

- data, mapping, ..., lineColor:

  parameters supplied to
  [`ggally_nostic_line`](https://ggobi.github.io/ggally/dev/reference/ggally_nostic_line.md)

- linePosition:

  line that is drawn in the background of the plot. Defaults to the
  overall model's sigma value.

## Value

ggplot2 plot object

## Details

As stated in
[`stats::influence()`](https://rdrr.io/r/stats/lm.influence.html)
documentation:

sigma: a vector whose i-th element contains the estimate of the residual
standard deviation obtained when the i-th case is dropped from the
regression. (The approximations needed for GLMs can result in this being
'NaN'.)

A line is added to display the overall model's sigma value. This gives a
baseline for comparison

## See also

[`stats::influence()`](https://rdrr.io/r/stats/lm.influence.html)

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

if (require(broom)) {
  dt <- broomify(stats::lm(mpg ~ wt + qsec + am, data = mtcars))
  p_(ggally_nostic_sigma(dt, ggplot2::aes(wt, .sigma)))
}
```
