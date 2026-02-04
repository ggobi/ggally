# Aes name

Aes name

## Usage

``` r
mapping_string(aes_col)
```

## Arguments

- aes_col:

  Single value from
  `ggplot2::`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(...)`

## Value

character string

## Examples

``` r
mapping <- ggplot2::aes(Petal.Length)
mapping_string(mapping$x)
#> [1] "Petal.Length"
```
