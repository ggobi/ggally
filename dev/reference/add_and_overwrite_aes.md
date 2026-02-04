# Add new aes

Add new aesthetics to a previous aes.

## Usage

``` r
add_and_overwrite_aes(current, new)
```

## Value

aes\_ output

## Author

Barret Schloerke

## Examples

``` r
data(diamonds, package = "ggplot2")
diamonds.samp <- diamonds[sample(1:dim(diamonds)[1], 1000), ]
pm <- ggpairs(diamonds.samp,
  columns = 5:7,
  mapping = ggplot2::aes(color = color),
  upper = list(continuous = "cor", mapping = ggplot2::aes(color = clarity)),
  lower = list(continuous = "cor", mapping = ggplot2::aes(color = cut)),
  title = "Diamonds Sample"
)
str(pm)
#> 
#> Custom str.ggmatrix output: 
#> To view original object use 'str(pm, raw = TRUE)'
#> 
#> List of 20
#>  $ data               :'data.frame': 1000 obs. of  10 variables:
#>   ..$ carat  : num [1:1000] 1 0.31 1.03 0.25 1.01 1.12 1.11 0.92 0.55 1.06 ...
#>   ..$ cut    : Ord.factor w/ 5 levels "Fair"<"Good"<..: 5 5 4 2 3 5 4 4 3 5 ...
#>   ..$ color  : Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 1 2 5 3 4 4 5 1 3 ...
#>   ..$ clarity: Ord.factor w/ 8 levels "I1"<"SI2"<"SI1"<..: 2 4 5 7 3 6 4 3 3 2 ...
#>   ..$ depth  : num [1:1000] 61.9 61.2 59.6 60.6 63.1 61.9 61 62 63.1 61.8 ...
#>   ..$ table  : num [1:1000] 56 55 59 64 59 57 59 60 56 56 ...
#>   ..$ price  : int [1:1000] 4760 734 7817 401 5010 9214 5346 3555 1593 5354 ...
#>   ..$ x      : num [1:1000] 6.43 4.37 6.59 4.05 6.3 6.62 6.7 6.27 5.19 6.56 ...
#>   ..$ y      : num [1:1000] 6.4 4.39 6.57 4.07 6.37 6.66 6.67 6.21 5.22 6.51 ...
#>   ..$ z      : num [1:1000] 3.97 2.68 3.92 2.46 4 4.11 4.08 3.87 3.28 4.04 ...
#>  $ plots              :List of 9
#>   ..$ : chr "PM; aes: c(x = ~depth, colour = ~color); fn: {wrap: 'ggally_densityDiag'}; gg: FALSE"
#>   ..$ : chr "PM; aes: c(x = ~table, y = ~depth, colour = ~clarity); fn: {wrap: 'ggally_cor'}; gg: FALSE"
#>   ..$ : chr "PM; aes: c(x = ~price, y = ~depth, colour = ~clarity); fn: {wrap: 'ggally_cor'}; gg: FALSE"
#>   ..$ : chr "PM; aes: c(x = ~depth, y = ~table, colour = ~cut); fn: {wrap: 'ggally_cor'}; gg: FALSE"
#>   ..$ : chr "PM; aes: c(x = ~table, colour = ~color); fn: {wrap: 'ggally_densityDiag'}; gg: FALSE"
#>   ..$ : chr "PM; aes: c(x = ~price, y = ~table, colour = ~clarity); fn: {wrap: 'ggally_cor'}; gg: FALSE"
#>   ..$ : chr "PM; aes: c(x = ~depth, y = ~price, colour = ~cut); fn: {wrap: 'ggally_cor'}; gg: FALSE"
#>   ..$ : chr "PM; aes: c(x = ~table, y = ~price, colour = ~cut); fn: {wrap: 'ggally_cor'}; gg: FALSE"
#>   ..$ : chr "PM; aes: c(x = ~price, colour = ~color); fn: {wrap: 'ggally_densityDiag'}; gg: FALSE"
#>  $ title              : chr "Diamonds Sample"
#>  $ xlab               : NULL
#>  $ ylab               : NULL
#>  $ showStrips         : NULL
#>  $ xAxisLabels        : chr [1:3] "depth" "table" "price"
#>  $ yAxisLabels        : chr [1:3] "depth" "table" "price"
#>  $ showXAxisPlotLabels: logi TRUE
#>  $ showYAxisPlotLabels: logi TRUE
#>  $ labeller           : chr "label_value"
#>  $ switch             : NULL
#>  $ xProportions       : NULL
#>  $ yProportions       : NULL
#>  $ progress           : logi FALSE
#>  $ legend             : NULL
#>  $ gg                 : NULL
#>  $ nrow               : int 3
#>  $ ncol               : int 3
#>  $ byrow              : logi TRUE
```
