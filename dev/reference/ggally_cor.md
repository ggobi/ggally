# Correlation value plot

Estimate correlation from the given data. If a color variable is
supplied, the correlation will also be calculated per group.

## Usage

``` r
ggally_cor(
  data,
  mapping,
  ...,
  stars = TRUE,
  method = "pearson",
  display_grid = FALSE,
  digits = 3,
  title_args = list(...),
  group_args = list(...),
  justify_labels = "right",
  align_percent = 0.5,
  title = "Corr",
  na.rm = NA,
  use = deprecated(),
  alignPercent = deprecated(),
  displayGrid = deprecated()
)
```

## Arguments

- data:

  data set using

- mapping:

  aesthetics being used

- ...:

  other arguments being supplied to
  [`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
  for the title and groups

- stars:

  logical value which determines if the significance stars should be
  displayed. Given the
  [`cor.test`](https://rdrr.io/r/stats/cor.test.html) p-values, display

  `"***"`

  :   if the p-value is `< 0.001`

  `"**"`

  :   if the p-value is `< 0.01`

  `"*"`

  :   if the p-value is `< 0.05`

  `"."`

  :   if the p-value is `< 0.10`

  `""`

  :   otherwise

- method:

  `method` supplied to cor function

- display_grid:

  if `TRUE`, display aligned panel grid lines. If `FALSE` (default),
  display a thin panel border.

- digits:

  number of digits to be displayed after the decimal point. See
  [`formatC`](https://rdrr.io/r/base/formatc.html) for how numbers are
  calculated.

- title_args:

  arguments being supplied to the title's
  [`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)

- group_args:

  arguments being supplied to the split-by-color group's
  [`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)

- justify_labels:

  `justify` argument supplied when
  [`format`](https://rdrr.io/r/base/format.html)ting the labels

- align_percent:

  relative align position of the text. When `justify_labels = 0.5`, this
  should not be needed to be set.

- title:

  title text to be displayed

- na.rm:

  logical value which determines if `NA` values are removed. If `TRUE`,
  no warning message will be displayed.

- use:

  **\[deprecated\]**. This variable is not used internally. Please
  remove it from your code.

- alignPercent, displayGrid:

  **\[deprecated\]**. Please use their snake-case counterparts.

## See also

[`ggally_statistic`](https://ggobi.github.io/ggally/dev/reference/ggally_statistic.md),
[`ggally_cor_v1_5`](https://ggobi.github.io/ggally/dev/reference/ggally_cor_v1_5.md)

## Author

Barret Schloerke

## Examples

``` r
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

data(tips)
p_(ggally_cor(tips, mapping = ggplot2::aes(total_bill, tip)))

# display with grid
p_(ggally_cor(
  tips,
  mapping = ggplot2::aes(total_bill, tip),
  display_grid = TRUE
))

# change text attributes
p_(ggally_cor(
  tips,
  mapping = ggplot2::aes(x = total_bill, y = tip),
  size = 15,
  colour = I("red"),
  title = "Correlation"
))

# split by a variable
p_(ggally_cor(
  tips,
  mapping = ggplot2::aes(total_bill, tip, color = sex),
  size = 5
))
```
