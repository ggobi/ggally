# Bar plot

Displays a bar plot for the diagonal of a
[`ggpairs`](https://ggobi.github.io/ggally/dev/reference/ggpairs.md)
plot matrix.

## Usage

``` r
ggally_barDiag(data, mapping, ..., rescale = FALSE)
```

## Arguments

- data:

  data set using

- mapping:

  aesthetics being used

- ...:

  other arguments are sent to geom_bar

- rescale:

  boolean to decide whether or not to rescale the count output. Only
  applies to numeric data

## Author

Barret Schloerke

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

data(tips)
p_(ggally_barDiag(tips, mapping = ggplot2::aes(x = day)))

p_(ggally_barDiag(tips, mapping = ggplot2::aes(x = tip), binwidth = 0.25))
```
