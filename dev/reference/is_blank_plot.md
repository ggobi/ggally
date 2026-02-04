# Is Blank Plot? Find out if the plot equals a blank plot

Is Blank Plot? Find out if the plot equals a blank plot

## Usage

``` r
is_blank_plot(p)
```

## Examples

``` r
GGally:::is_blank_plot(ggally_blank())
#> [1] TRUE
GGally:::is_blank_plot(ggally_points(mtcars, ggplot2::aes(disp, hp)))
#> [1] FALSE
```
