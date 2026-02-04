# Glyph plot class

Glyph plot class

## Usage

``` r
glyphplot(data, width, height, polar, x_major, y_major)

is.glyphplot(x)

# S3 method for class 'glyphplot'
x[...]

# S3 method for class 'glyphplot'
print(x, ...)
```

## Arguments

- data:

  A data frame containing variables named in `x_major`, `x_minor`,
  `y_major` and `y_minor`.

- height, width:

  The height and width of each glyph. Defaults to 95% of the
  [`resolution`](https://ggplot2.tidyverse.org/reference/resolution.html)
  of the data. Specify the width absolutely by supplying a numeric
  vector of length 1, or relative to the

- polar:

  A logical of length 1, specifying whether the glyphs should be drawn
  in polar coordinates. Defaults to `FALSE`.

- x_major, y_major:

  The name of the variable (as a string) for the major x and y axes.
  Together, the

- x:

  glyphplot to be printed

- ...:

  ignored

## Author

Di Cook, Heike Hofmann, Hadley Wickham
