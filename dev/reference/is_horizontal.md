# Check if plot is horizontal

Check if plot is horizontal

## Usage

``` r
is_horizontal(data, mapping, val = "y")

is_character_column(data, mapping, val = "y")
```

## Arguments

- data:

  data used in ggplot2 plot

- mapping:

  ggplot2 [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html)
  mapping

- val:

  key to retrieve from `mapping`

## Value

Boolean determining if the data is a character-like data

## Examples

``` r
is_horizontal(iris, ggplot2::aes(Sepal.Length, Species)) # TRUE
#> [1] TRUE
is_horizontal(iris, ggplot2::aes(Sepal.Length, Species), "x") # FALSE
#> [1] FALSE
is_horizontal(iris, ggplot2::aes(Sepal.Length, Sepal.Width)) # FALSE
#> [1] FALSE
```
