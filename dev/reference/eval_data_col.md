# Evaluate data column

Evaluate data column

## Usage

``` r
eval_data_col(data, aes_col)
```

## Arguments

- data:

  data set to evaluate the data with

- aes_col:

  Single value from an
  `ggplot2::`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(...)`
  object

## Value

Aes mapping with the x and y values switched

## Examples

``` r
mapping <- ggplot2::aes(Petal.Length)
eval_data_col(iris, mapping$x)
#>   [1] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 1.5 1.6 1.4 1.1 1.2 1.5
#>  [17] 1.3 1.4 1.7 1.5 1.7 1.5 1.0 1.7 1.9 1.6 1.6 1.5 1.4 1.6 1.6 1.5
#>  [33] 1.5 1.4 1.5 1.2 1.3 1.4 1.3 1.5 1.3 1.3 1.3 1.6 1.9 1.4 1.6 1.4
#>  [49] 1.5 1.4 4.7 4.5 4.9 4.0 4.6 4.5 4.7 3.3 4.6 3.9 3.5 4.2 4.0 4.7
#>  [65] 3.6 4.4 4.5 4.1 4.5 3.9 4.8 4.0 4.9 4.7 4.3 4.4 4.8 5.0 4.5 3.5
#>  [81] 3.8 3.7 3.9 5.1 4.5 4.5 4.7 4.4 4.1 4.0 4.4 4.6 4.0 3.3 4.2 4.2
#>  [97] 4.2 4.3 3.0 4.1 6.0 5.1 5.9 5.6 5.8 6.6 4.5 6.3 5.8 6.1 5.1 5.3
#> [113] 5.5 5.0 5.1 5.3 5.5 6.7 6.9 5.0 5.7 4.9 6.7 4.9 5.7 6.0 4.8 4.9
#> [129] 5.6 5.8 6.1 6.4 5.6 5.1 5.6 6.1 5.6 5.5 4.8 5.4 5.6 5.1 5.1 5.9
#> [145] 5.7 5.2 5.0 5.2 5.4 5.1
```
