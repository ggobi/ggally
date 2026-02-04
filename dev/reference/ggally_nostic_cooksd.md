# [`ggnostic`](https://ggobi.github.io/ggally/dev/reference/ggnostic.md) Cook's distance

A function to display
[`stats::cooks.distance()`](https://rdrr.io/r/stats/influence.measures.html).

## Usage

``` r
ggally_nostic_cooksd(
  data,
  mapping,
  ...,
  linePosition = pf(0.5, length(attr(data, "var_x")), nrow(data) - length(attr(data,
    "var_x"))),
  lineColor = brew_colors("grey"),
  lineType = 2
)
```

## Arguments

- data, mapping, ..., lineColor, lineType:

  parameters supplied to
  [`ggally_nostic_line`](https://ggobi.github.io/ggally/dev/reference/ggally_nostic_line.md)

- linePosition:

  4 / n is the general cutoff point for Cook's Distance

## Value

ggplot2 plot object

## Details

A line is added at \\F\_{p,n-p}(0.5)\\ to display the general cutoff
point for Cook's Distance.

Reference: Michael H. Kutner, Christopher J. Nachtsheim, John Neter, and
William Li. Applied linear statistical models. The McGraw-Hill / Irwin
series operations and decision sciences. McGraw-Hill Irwin, 2005, p. 403

## See also

[`stats::cooks.distance()`](https://rdrr.io/r/stats/influence.measures.html)

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

if (require(broom)) {
  dt <- broomify(stats::lm(mpg ~ wt + qsec + am, data = mtcars))
  p_(ggally_nostic_cooksd(dt, ggplot2::aes(wt, .cooksd)))
}
```
