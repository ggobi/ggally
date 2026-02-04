# Check if an object is a ggmatrix

Check if an object is a ggmatrix

## Usage

``` r
is_ggmatrix(x)
```

## Arguments

- x:

  An object to check

## Value

Logical value indicating if the object is a `ggmatrix`

## Examples

``` r
is_ggmatrix(ggpairs(mtcars))
#> [1] TRUE
is_ggmatrix(ggplot2::ggplot())
#> [1] FALSE
```
