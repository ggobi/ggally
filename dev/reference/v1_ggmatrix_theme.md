# Modify a [`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md) object by adding an ggplot2 object to all

**\[deprecated\]**

This function allows cleaner axis labels for your plots, but is
deprecated. You can achieve the same effect by specifying strip's
background and placement properties (see Examples).

## Usage

``` r
v1_ggmatrix_theme()
```

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

# Cleaner axis labels with v1_ggmatrix_theme
p_(ggpairs(iris, 1:2) + v1_ggmatrix_theme())
#> Warning: `v1_ggmatrix_theme()` was deprecated in GGally 2.3.0.
#> ℹ This function will be removed in future releases.


# Move the column names to the left and bottom
p_(ggpairs(iris, 1:2, switch = "both") + v1_ggmatrix_theme())


# Manually specifying axis labels properties
p_(
  ggpairs(iris, 1:2) +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.placement = "outside"
  )
)


# This way you have even more control over how the final plot looks.
# For example, if you want to set the background color to yellow:
p_(
  ggpairs(iris, 1:2) +
  theme(
    strip.background = element_rect(fill = "yellow"),
    strip.placement = "outside"
  )
)
```
