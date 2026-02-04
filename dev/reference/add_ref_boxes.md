# Add reference boxes around each cell of the glyphmap.

Add reference boxes around each cell of the glyphmap.

## Usage

``` r
add_ref_boxes(
  data,
  var_fill = NULL,
  color = "white",
  size = 0.5,
  fill = NA,
  ...
)
```

## Arguments

- data:

  A glyphmap structure.

- var_fill:

  Variable name to use to set the fill color

- color:

  Set the color to draw in, default is "white"

- size:

  Set the line size, default is 0.5

- fill:

  fill value used if `var_fill` is `NULL`

- ...:

  other arguments passed onto
  [`ggplot2::geom_rect()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)
