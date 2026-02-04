# Density or tiles plot with facets

Make tile plot or density plot as compact as possible.

## Usage

``` r
ggally_facetdensitystrip(data, mapping, ..., den_strip = FALSE)
```

## Arguments

- data:

  data set using

- mapping:

  aesthetics being used

- ...:

  other arguments being sent to either geom_histogram or stat_density

- den_strip:

  boolean to decide whether or not to plot a density strip(TRUE) or a
  facet density(FALSE) plot.

## Author

Barret Schloerke

## Examples

``` r
example(ggally_facetdensity)
#> 
#> gglly_> # Small function to display plots only if it's interactive
#> gglly_> p_ <- GGally::print_if_interactive
#> 
#> gglly_> data(tips)
#> 
#> gglly_> p_(ggally_facetdensity(tips, mapping = ggplot2::aes(x = total_bill, y = sex)))

#> 
#> gglly_> p_(ggally_facetdensity(
#> gglly_+   tips,
#> gglly_+   mapping = ggplot2::aes(sex, total_bill, color = sex)
#> gglly_+ ))

example(ggally_denstrip)
#> 
#> gglly_> # Small function to display plots only if it's interactive
#> gglly_> p_ <- GGally::print_if_interactive
#> 
#> gglly_> data(tips)
#> 
#> gglly_> p_(ggally_denstrip(tips, mapping = ggplot2::aes(x = total_bill, y = sex)))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

#> 
#> gglly_> p_(ggally_denstrip(
#> gglly_+   tips,
#> gglly_+   mapping = ggplot2::aes(sex, tip), binwidth = 0.2
#> gglly_+ ) + ggplot2::scale_fill_gradient(low = "grey80", high = "black"))
```
