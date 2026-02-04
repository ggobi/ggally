# Bivariate density plot

Make a 2D density plot from a given data.

## Usage

``` r
ggally_density(data, mapping, ...)
```

## Arguments

- data:

  data set using

- mapping:

  aesthetics being used

- ...:

  parameters sent to either stat_density2d or geom_density2d

## Details

The aesthetic "fill" determines whether or not `stat_density2d` (filled)
or `geom_density2d` (lines) is used.

## Author

Barret Schloerke

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

data(tips)
if (require(MASS)) {
  p_(ggally_density(tips, mapping = ggplot2::aes(x = total_bill, y = tip)))
  p_(ggally_density(
    tips,
    mapping = ggplot2::aes(total_bill, tip, fill = after_stat(level))
  ))
  p_(ggally_density(
    tips,
    mapping = ggplot2::aes(total_bill, tip, fill = after_stat(level))
  ) + ggplot2::scale_fill_gradient(breaks = c(0.05, 0.1, 0.15, 0.2)))
}
#> Loading required package: MASS


```
