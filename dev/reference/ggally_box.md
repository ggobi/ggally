# Box plot

Make a box plot with a given data set. `ggally_box_no_facet` will be a
single panel plot, while `ggally_box` will be a faceted plot

## Usage

``` r
ggally_box(data, mapping, ...)

ggally_box_no_facet(data, mapping, ...)
```

## Arguments

- data:

  data set using

- mapping:

  aesthetics being used

- ...:

  other arguments being supplied to geom_boxplot

## Author

Barret Schloerke

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

data(tips)
p_(ggally_box(tips, mapping = ggplot2::aes(x = total_bill, y = sex)))

p_(ggally_box(
  tips,
  mapping        = ggplot2::aes(sex, total_bill, color = sex),
  outlier.colour = "red",
  outlier.shape  = 13,
  outlier.size   = 8
))
```
