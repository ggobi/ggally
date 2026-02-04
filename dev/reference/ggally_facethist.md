# Faceted histogram

Display subsetted histograms of the data in different panels.

## Usage

``` r
ggally_facethist(data, mapping, ...)
```

## Arguments

- data:

  data set using

- mapping:

  aesthetics being used

- ...:

  parameters sent to stat_bin()

## Author

Barret Schloerke

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

data(tips)
p_(ggally_facethist(tips, mapping = ggplot2::aes(x = tip, y = sex)))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

p_(ggally_facethist(tips, mapping = ggplot2::aes(x = tip, y = sex), binwidth = 0.1))
```
