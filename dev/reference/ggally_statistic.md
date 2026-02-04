# Generalized text display

Generalized text display

## Usage

``` r
ggally_statistic(
  data,
  mapping,
  text_fn,
  title,
  na.rm = NA,
  display_grid = FALSE,
  justify_labels = "right",
  justify_text = "left",
  sep = ": ",
  family = "mono",
  title_args = list(),
  group_args = list(),
  align_percent = 0.5,
  title_hjust = 0.5,
  group_hjust = 0.5
)
```

## Arguments

- data:

  data set using

- mapping:

  aesthetics being used

- text_fn:

  function that takes in `x` and `y` and returns a text string

- title:

  title text to be displayed

- na.rm:

  logical value which determines if `NA` values are removed. If `TRUE`,
  no warning message will be displayed.

- display_grid:

  if `TRUE`, display aligned panel grid lines. If `FALSE` (default),
  display a thin panel border.

- justify_labels:

  `justify` argument supplied when
  [`format`](https://rdrr.io/r/base/format.html)ting the labels

- justify_text:

  `justify` argument supplied when
  [`format`](https://rdrr.io/r/base/format.html)ting the returned
  `text_fn(x, y)` values

- sep:

  separation value to be placed between the labels and text

- family:

  font family used when displaying all text. This value will be set in
  `title_args` or `group_args` if no `family` value exists. By using
  `"mono"`, groups will align with each other.

- title_args:

  arguments being supplied to the title's
  [`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)

- group_args:

  arguments being supplied to the split-by-color group's
  [`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)

- align_percent:

  relative align position of the text. When `title_hjust = 0.5` and
  `group_hjust = 0.5`, this should not be needed to be set.

- title_hjust, group_hjust:

  `hjust` sent to
  [`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
  for the title and group values respectively. Any `hjust` value
  supplied in `title_args` or `group_args` will take precedence.

## See also

[`ggally_cor`](https://ggobi.github.io/ggally/dev/reference/ggally_cor.md)
