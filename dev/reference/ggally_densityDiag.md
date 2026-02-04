# Univariate density plot

Displays a density plot for the diagonal of a
[`ggpairs`](https://ggobi.github.io/ggally/dev/reference/ggpairs.md)
plot matrix.

## Usage

``` r
ggally_densityDiag(data, mapping, ..., rescale = FALSE)
```

## Arguments

- data:

  data set using

- mapping:

  aesthetics being used.

- ...:

  other arguments sent to stat_density

- rescale:

  boolean to decide whether or not to rescale the count output

## Author

Barret Schloerke

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

data(tips)
p_(ggally_densityDiag(tips, mapping = ggplot2::aes(x = total_bill)))

p_(ggally_densityDiag(tips, mapping = ggplot2::aes(x = total_bill, color = day)))
```
