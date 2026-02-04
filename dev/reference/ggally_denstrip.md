# Tile plot with facets

Displays a Tile Plot as densely as possible.

## Usage

``` r
ggally_denstrip(data, mapping, ...)
```

## Arguments

- data:

  data set using

- mapping:

  aesthetics being used

- ...:

  other arguments being sent to stat_bin

## Author

Barret Schloerke

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

data(tips)
p_(ggally_denstrip(tips, mapping = ggplot2::aes(x = total_bill, y = sex)))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

p_(ggally_denstrip(
  tips,
  mapping = ggplot2::aes(sex, tip), binwidth = 0.2
) + ggplot2::scale_fill_gradient(low = "grey80", high = "black"))
```
