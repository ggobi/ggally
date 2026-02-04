# Box and dot plot

Place box plots or dot plots on the graph

## Usage

``` r
ggally_dot_and_box(data, mapping, ..., boxPlot = TRUE)
```

## Arguments

- data:

  data set using

- mapping:

  aesthetics being used

- ...:

  parameters passed to either geom_jitter or geom_boxplot

- boxPlot:

  boolean to decide to plot either box plots (TRUE) or dot plots (FALSE)

## Author

Barret Schloerke

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

data(tips)
p_(ggally_dot_and_box(
  tips,
  mapping = ggplot2::aes(x = total_bill, y = sex, color = sex),
  boxPlot = TRUE
))

p_(ggally_dot_and_box(
  tips,
  mapping = ggplot2::aes(x = total_bill, y = sex, color = sex),
  boxPlot = FALSE
))
```
