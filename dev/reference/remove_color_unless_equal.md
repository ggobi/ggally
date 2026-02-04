# Remove colour mapping unless found in select mapping keys

Remove colour mapping unless found in select mapping keys

## Usage

``` r
remove_color_unless_equal(mapping, to = c("x", "y"))
```

## Arguments

- mapping:

  output of
  `ggplot2::`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(...)`

- to:

  set of mapping keys to check

## Value

Aes mapping with colour mapping kept only if found in selected mapping
keys.

## Examples

``` r
mapping <- aes(x = sex, y = age, colour = sex)

mapping <- aes(x = sex, y = age, colour = region)
remove_color_unless_equal(mapping)
#> Aesthetic mapping: 
#> * `x` -> `sex`
#> * `y` -> `age`
```
