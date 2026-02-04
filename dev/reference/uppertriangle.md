# Rearrange dataset as the preparation of [`ggscatmat`](https://ggobi.github.io/ggally/dev/reference/ggscatmat.md) function

Function for making the dataset used to plot the uppertriangle plots.

## Usage

``` r
uppertriangle(
  data,
  columns = 1:ncol(data),
  color = NULL,
  corMethod = "pearson"
)
```

## Arguments

- data:

  a data matrix. Should contain numerical (continuous) data.

- columns:

  an option to choose the column to be used in the raw dataset. Defaults
  to `1:ncol(data)`

- color:

  an option to choose a factor variable to be grouped with. Defaults to
  `(NULL)`

- corMethod:

  method argument supplied to [`cor`](https://rdrr.io/r/stats/cor.html)

## Author

Mengjia Ni, Di Cook

## Examples

``` r
data(flea)
head(uppertriangle(flea, columns = 2:4))
#> # A tibble: 3 × 5
#> # Groups:   xlab [2]
#>   xlab  ylab  r     xvalue yvalue
#>   <fct> <fct> <chr>  <dbl>  <dbl>
#> 1 tars2 tars1 0.03   126.    182 
#> 2 head  tars1 -0.1    50.5   182 
#> 3 head  tars2 0.67    50.5   126.
head(uppertriangle(flea))
#> # A tibble: 6 × 5
#> # Groups:   xlab [3]
#>   xlab  ylab  r     xvalue yvalue
#>   <fct> <fct> <chr>  <dbl>  <dbl>
#> 1 tars2 tars1 0.03   126.   182  
#> 2 head  tars1 -0.1    50.5  182  
#> 3 head  tars2 0.67    50.5  126. 
#> 4 aede1 tars1 -0.33  136.   182  
#> 5 aede1 tars2 0.56   136.   126. 
#> 6 aede1 head  0.59   136.    50.5
head(uppertriangle(flea, color = "species"))
#> # A tibble: 6 × 6
#> # Groups:   xlab, ylab [2]
#>   xlab  ylab  colorcolumn r     xvalue yvalue
#>   <fct> <fct> <fct>       <chr>  <dbl>  <dbl>
#> 1 tars2 tars1 Concinna    0.77   126.     152
#> 2 tars2 tars1 Heikert.    0.64   126.     182
#> 3 tars2 tars1 Heptapot.   0.56   126.     212
#> 4 head  tars1 Concinna    0.68    50.5    152
#> 5 head  tars1 Heikert.    0.65    50.5    182
#> 6 head  tars1 Heptapot.   0.77    50.5    212
```
