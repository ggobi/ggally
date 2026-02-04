# Mosaic plot

Plots the mosaic plot by using fluctuation.

## Usage

``` r
ggally_ratio(
  data,
  mapping = ggplot2::aes(!!!stats::setNames(lapply(colnames(data)[1:2], as.name), c("x",
    "y"))),
  ...,
  floor = 0,
  ceiling = NULL
)
```

## Arguments

- data:

  data set using

- mapping:

  aesthetics being used. Only x and y will used and both are required

- ...:

  passed to
  [`geom_tile`](https://ggplot2.tidyverse.org/reference/geom_tile.html)`(...)`

- floor:

  don't display cells smaller than this value

- ceiling:

  max value to scale frequencies. If any frequency is larger than the
  ceiling, the fill color is displayed darker than other rectangles

## Author

Barret Schloerke

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

data(tips)
p_(ggally_ratio(tips, ggplot2::aes(sex, day)))

p_(ggally_ratio(tips, ggplot2::aes(sex, day)) + ggplot2::coord_equal())

# only plot tiles greater or equal to 20 and scale to a max of 50
p_(ggally_ratio(
  tips, ggplot2::aes(sex, day),
  floor = 20, ceiling = 50
) + ggplot2::theme(aspect.ratio = 4 / 2))
```
