# [`ggnostic`](https://ggobi.github.io/ggally/dev/reference/ggnostic.md) background line with geom

If a non-null `linePosition` value is given, a line will be drawn before
the given `continuous_geom` or `combo_geom` is added to the plot.

## Usage

``` r
ggally_nostic_line(
  data,
  mapping,
  ...,
  linePosition = NULL,
  lineColor = "red",
  lineSize = 0.5,
  lineAlpha = 1,
  lineType = 1,
  continuous_geom = ggplot2::geom_point,
  combo_geom = ggplot2::geom_boxplot,
  mapColorToFill = TRUE
)
```

## Arguments

- data, mapping:

  supplied directly to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)

- ...:

  parameters supplied to `continuous_geom` or `combo_geom`

- linePosition, lineColor, lineSize, lineAlpha, lineType:

  parameters supplied to
  [`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)

- continuous_geom:

  ggplot2 geom that is executed after the line is (possibly) added and
  if the x data is continuous

- combo_geom:

  ggplot2 geom that is executed after the line is (possibly) added and
  if the x data is discrete

- mapColorToFill:

  boolean to determine if combo plots should cut the color mapping to
  the fill mapping

## Value

ggplot2 plot object

## Details

Functions with a color in their name have different default color
behavior.
