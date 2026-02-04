# Swap x and y mapping

Swap x and y mapping

## Usage

``` r
mapping_swap_x_y(mapping)
```

## Arguments

- mapping:

  output of
  `ggplot2::`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(...)`

## Value

Aes mapping with the x and y values switched

## Examples

``` r
mapping <- ggplot2::aes(Petal.Length, Sepal.Width)
mapping
#> Aesthetic mapping: 
#> * `x` -> `Petal.Length`
#> * `y` -> `Sepal.Width`
mapping_swap_x_y(mapping)
#> Aesthetic mapping: 
#> * `x` -> `Sepal.Width`
#> * `y` -> `Petal.Length`
```
