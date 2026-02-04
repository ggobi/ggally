# Changelog

## GGally (development version)

- Fixed duplicate aesthetics warning in
  [`ggsurv()`](https://ggobi.github.io/ggally/dev/reference/ggsurv.md)
  with upcoming ggplot2 version. Removed duplicate `lty` aesthetic
  mapping from confidence interval bounds.
  ([\#572](https://github.com/ggobi/ggally/issues/572))

- Fixed
  [`mapping_string()`](https://ggobi.github.io/ggally/dev/reference/mapping_string.md)
  to properly handle long aesthetic expressions by collapsing multi-line
  [`deparse()`](https://rdrr.io/r/base/deparse.html) output.
  ([\#573](https://github.com/ggobi/ggally/issues/573))

- Updated `fix_data()` to handle map objects correctly with ggplot2 \>=
  4.0.0 using
  [`ggplot2::map_data()`](https://ggplot2.tidyverse.org/reference/map_data.html)
  instead of
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html).
  ([\#573](https://github.com/ggobi/ggally/issues/573))

- Wrapped documentation examples requiring optional packages
  ([MASS](http://www.stats.ox.ac.uk/pub/MASS4/),
  [broom](https://broom.tidymodels.org/),
  [network](https://statnet.org/), [sna](https://statnet.org),
  [ggforce](https://ggforce.data-imaginist.com)) in `if (require(...))`
  blocks to prevent errors when packages are not installed.
  ([\#573](https://github.com/ggobi/ggally/issues/573))

- Added [MASS](http://www.stats.ox.ac.uk/pub/MASS4/) to Suggests in
  DESCRIPTION to support density plot examples.
  ([\#573](https://github.com/ggobi/ggally/issues/573))

- Updated `.Rbuildignore` to exclude [vdiffr](https://vdiffr.r-lib.org/)
  snapshot SVG files more specifically.
  ([\#573](https://github.com/ggobi/ggally/issues/573))

- Added GitHub Actions check script to conditionally remove snapshot
  paths from `.Rbuildignore` for macOS testing.
  ([\#573](https://github.com/ggobi/ggally/issues/573))

## GGally 2.4.0

CRAN release: 2025-08-23

- Replace internal usage with the base pipe (`|>`). (Thank you
  [@m-muecke](https://github.com/m-muecke)!
  [\#554](https://github.com/ggobi/ggally/issues/554))

- Enhance all error and warning outputs by using
  [cli](https://cli.r-lib.org). (Thank you
  [@m-muecke](https://github.com/m-muecke)!
  [\#557](https://github.com/ggobi/ggally/issues/557))

- Add `nba_ppg_2008` dataset describing NBA Player Statistics for
  2008-2009 Season.
  ([\#562](https://github.com/ggobi/ggally/issues/562))

- Update `ggnetworkmap` to use
  [airports](https://github.com/OpenIntroStat/airports) package for
  airport data visualization.
  ([\#562](https://github.com/ggobi/ggally/issues/562))

## GGally 2.3.0

CRAN release: 2025-07-18

- With [ggplot2](https://ggplot2.tidyverse.org) v4.0.0, objects are now
  `+`’ed together using [S7](https://rconsortium.github.io/S7/). This
  means the startup message for
  `Registered S3 method overwritten by 'GGally'` has been removed.
  (Thank you [@teunbrand](https://github.com/teunbrand) for the
  enhancement in ggplot2!
  [\#545](https://github.com/ggobi/ggally/issues/545))

- Fixed bug where correlations of 0 in a `ggcor()` output were silently
  dropped. Now all correlations are always displayed. (Thank you
  [@winterstat](https://github.com/winterstat)!
  [\#536](https://github.com/ggobi/ggally/issues/536))

- Fixed correlations values `ggcor()` so that they are formatted to the
  same number of decimal places via `label_round`. Now `0.2` and `0.001`
  with `label_round = 2` will be displayed as `"0.20"` and `"0.00"`
  respectively. (Thank you [@winterstat](https://github.com/winterstat)!
  [\#536](https://github.com/ggobi/ggally/issues/536))

- Added parameter `ggally_cor(na.rm=)` which is passed directly to
  [`ggally_statistic()`](https://ggobi.github.io/ggally/dev/reference/ggally_statistic.md).
  (Thank you [@vinouselouane](https://github.com/vinouselouane)!
  [\#516](https://github.com/ggobi/ggally/issues/516))

- Deprecated parameter `ggally_cor(use=)`. The value was never leveraged
  within the code. Please use `ggally_cor(na.rm=)` instead. (Thank you
  [@vinouselouane](https://github.com/vinouselouane)!
  [\#516](https://github.com/ggobi/ggally/issues/516))

- Prepare GGally for [ggplot2](https://ggplot2.tidyverse.org) v4 (Thank
  you [@teunbrand](https://github.com/teunbrand)!
  [\#528](https://github.com/ggobi/ggally/issues/528))

- Replace internal [plyr](http://had.co.nz/plyr) usage with
  [dplyr](https://dplyr.tidyverse.org). (Thank you
  [@MichaelChirico](https://github.com/MichaelChirico)!
  [\#520](https://github.com/ggobi/ggally/issues/520),
  [\#521](https://github.com/ggobi/ggally/issues/521),
  [\#522](https://github.com/ggobi/ggally/issues/522),
  [\#523](https://github.com/ggobi/ggally/issues/523),
  [\#524](https://github.com/ggobi/ggally/issues/524),
  [\#525](https://github.com/ggobi/ggally/issues/525),
  [\#527](https://github.com/ggobi/ggally/issues/527),
  [\#530](https://github.com/ggobi/ggally/issues/530))

- General package dependency cleanup. (Thank you
  [@olivroy](https://github.com/olivroy)!
  [\#509](https://github.com/ggobi/ggally/issues/509))

- Fix `anyClass` ordering in
  [`ggparcoord()`](https://ggobi.github.io/ggally/dev/reference/ggparcoord.md)
  when data has missing values (Thank you
  [@92amartins](https://github.com/92amartins)!
  [\#500](https://github.com/ggobi/ggally/issues/500))

- Use [lifecycle](https://lifecycle.r-lib.org/) for deprecation warnings
  (Thank you [@92amartins](https://github.com/92amartins)!
  [\#494](https://github.com/ggobi/ggally/issues/494),
  [\#496](https://github.com/ggobi/ggally/issues/496))

- Leverage `.data$` mask to remove all global variable declarations.
  (Thank you [@MichaelChirico](https://github.com/MichaelChirico)!
  [\#533](https://github.com/ggobi/ggally/issues/533))

- Warn and return `"NA"` when less than 3 values are given to a
  combination in
  [`ggally_cor()`](https://ggobi.github.io/ggally/dev/reference/ggally_cor.md).
  (Thank you [@bk1n](https://github.com/bk1n)!
  [\#510](https://github.com/ggobi/ggally/issues/510))

- Added helper method
  [`is_ggmatrix()`](https://ggobi.github.io/ggally/dev/reference/is_ggmatrix.md)
  to check if an object is a `ggmatrix` object.
  ([\#548](https://github.com/ggobi/ggally/issues/548))

- Remove `gg` class from `ggmatrix` objects. This is no longer needed
  due to enhanced `+` S7 methods.
  ([\#549](https://github.com/ggobi/ggally/issues/549))

- Bumped minimum required version of `R` to 4.3 due to S7 handling the
  `+` operations. ([\#549](https://github.com/ggobi/ggally/issues/549))

## GGally 2.2.1

CRAN release: 2024-02-13

- Fix compatibility with [ggplot2](https://ggplot2.tidyverse.org) 3.5.0
  (Thank you [@teunbrand](https://github.com/teunbrand)!
  [\#481](https://github.com/ggobi/ggally/issues/481))

## GGally 2.2.0

CRAN release: 2023-11-22

#### Bug fixes

- Removed dependency on reshape2
  ([\#475](https://github.com/ggobi/ggally/issues/475))
- Reverse ordering of y-axis in
  [`ggally_count()`](https://ggobi.github.io/ggally/dev/reference/ggally_count.md)
  ([\#420](https://github.com/ggobi/ggally/issues/420))
- Facets ordering in
  [`ggcoef_compare()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.html)
  ([\#426](https://github.com/ggobi/ggally/issues/426))
- Fix in
  [`ggcoef_compare()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.html)
  when using tidy selectors for `no_reference_row`
  ([\#430](https://github.com/ggobi/ggally/issues/430))
- Fix in
  [`ggcoef_compare()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.html)
  regarding `no_reference_row` option
  ([\#430](https://github.com/ggobi/ggally/issues/430))
- Fix in
  [`ggcoef_compare()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.html)
  with an `include` argument
  ([\#447](https://github.com/ggobi/ggally/issues/447))
- New default tidier for
  [`ggcoef_model()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.html),
  now using
  [`broom.helpers::tidy_with_broom_or_parameters()`](https://larmarange.github.io/broom.helpers/reference/tidy_with_broom_or_parameters.html)
  ([\#432](https://github.com/ggobi/ggally/issues/432))
- Re-export methods from and redirect vignettes to the
  [ggstats](https://larmarange.github.io/ggstats/) package
  ([\#452](https://github.com/ggobi/ggally/issues/452),
  [\#457](https://github.com/ggobi/ggally/issues/457))
- Replaced `..scaled..` with `after_stat(scaled)` in ggscatmat
  ([\#467](https://github.com/ggobi/ggally/issues/467))

## GGally 2.1.2

CRAN release: 2021-06-21

#### Bug fixes

- Replace `ggplot2` usage of `*_guide = FALSE` with `*_guide = "none"`
  ([@larmarange](https://github.com/larmarange),
  [\#418](https://github.com/ggobi/ggally/issues/418))
- Require `network >= 1.17.1`
  ([\#418](https://github.com/ggobi/ggally/issues/418))

## GGally 2.1.1

CRAN release: 2021-03-08

#### Bug fixes

- Ignore `colour` aesthetic if all values are `NA`.
  ([@larmarange](https://github.com/larmarange),
  [\#404](https://github.com/ggobi/ggally/issues/404))
- Avoid all duplicates within
  [`stat_cross()`](https://larmarange.github.io/ggstats/reference/stat_cross.html).
  ([@larmarange](https://github.com/larmarange),
  [\#402](https://github.com/ggobi/ggally/issues/402))
- Avoid an error when tidiers do not return p-values.
  ([@larmarange](https://github.com/larmarange),
  [\#400](https://github.com/ggobi/ggally/issues/400))
- Suggest `emmeans` to allow
  [`ggcoef()`](https://ggobi.github.io/ggally/dev/reference/ggcoef.md)
  example to execute.
  ([\#407](https://github.com/ggobi/ggally/issues/407))

## GGally 2.1.0

CRAN release: 2021-01-06

#### Breaking changes

- Following version 7.0.0 of `broom`, computed residuals in
  [`stat_cross()`](https://larmarange.github.io/ggstats/reference/stat_cross.html)
  are now named `"resid"` and `"std.resid"`. `cells` and `fill`
  arguments of
  [`ggally_crosstable()`](https://ggobi.github.io/ggally/dev/reference/ggally_crosstable.md)
  and
  [`ggtable()`](https://ggobi.github.io/ggally/dev/reference/ggtable.md)
  have been updated accordingly
  ([@larmarange](https://github.com/larmarange),
  [\#391](https://github.com/ggobi/ggally/issues/391))

#### Other changes

- [`ggcoef()`](https://ggobi.github.io/ggally/dev/reference/ggcoef.md)
  redesign based on `broom.helpers` with four new functions:
  [`ggcoef_model()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.html),
  [`ggcoef_compare()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.html),
  [`ggcoef_multinom()`](https://larmarange.github.io/ggstats/reference/ggcoef_multicomponents.html)
  and
  [`ggcoef_plot()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.html)
  (more informations in the dedicated vignette,
  [@larmarange](https://github.com/larmarange),
  [\#392](https://github.com/ggobi/ggally/issues/392))
- New geometries:
  [`geom_stripped_rows()`](https://larmarange.github.io/ggstats/reference/geom_stripped_rows.html)
  and
  [`geom_stripped_cols()`](https://larmarange.github.io/ggstats/reference/geom_stripped_rows.html)
  ([\#392](https://github.com/ggobi/ggally/issues/392),
  [@larmarange](https://github.com/larmarange))
- New option `reverse_fill_labels` for
  [`ggally_colbar()`](https://ggobi.github.io/ggally/dev/reference/ggally_colbar.md)
  and
  [`ggally_rowbar()`](https://ggobi.github.io/ggally/dev/reference/ggally_colbar.md)
  ([@larmarange](https://github.com/larmarange),
  [\#374](https://github.com/ggobi/ggally/issues/374))
- [`stat_prop()`](https://larmarange.github.io/ggstats/reference/stat_prop.html)
  now accepts a **x** or a **y** aesthetic
  ([\#395](https://github.com/ggobi/ggally/issues/395),
  [@larmarange](https://github.com/larmarange))
- Temporarily not listening to `ggally_statistic(family)` to avoid
  monospaced font issues. See
  [\#373](https://github.com/ggobi/ggally/issues/373) for more details.
  ([\#387](https://github.com/ggobi/ggally/issues/387))

## GGally 2.0.0

CRAN release: 2020-06-06

#### New Vignettes

- [`vig_ggally("ggally_plots")`](https://ggobi.github.io/ggally/articles/ggally_plots.html) -
  ggally\_\*(): List of available high-level plots
- [`vig_ggally("ggally_stats")`](https://ggobi.github.io/ggally/articles/ggally_stats.html) -
  stat\_\*(): Additional statistics for ggplot2
- [`vig_ggally("ggbivariate")`](https://ggobi.github.io/ggally/articles/ggbivariate.html) -
  ggbivariate(): Plot an outcome with several potential explanatory
  variables
- [`vig_ggally("ggtable")`](https://ggobi.github.io/ggally/articles/ggtable.html) -
  ggtable(): Cross-tabulated tables
- To view all vignettes for `GGally`, call
  [`GGally::vig_ggally()`](https://ggobi.github.io/ggally/dev/reference/vig_ggally.md)

#### New functions

[`ggbivariate()`](https://ggobi.github.io/ggally/dev/reference/ggbivariate.md)
([@larmarange](https://github.com/larmarange),
[\#324](https://github.com/ggobi/ggally/issues/324)) \* Display an
outcome using several potential explanatory variables \*
[`vig_ggally("ggbivariate")`](https://ggobi.github.io/ggally/articles/ggbivariate.html)

[`ggtable()`](https://ggobi.github.io/ggally/dev/reference/ggtable.md)
([@larmarange](https://github.com/larmarange),
[\#351](https://github.com/ggobi/ggally/issues/351)) \* Cross-tabulated
tables of discrete variables \*
[`vig_ggally("ggtable")`](https://ggobi.github.io/ggally/articles/ggtable.html)

[`add_to_ggmatrix()`](https://ggobi.github.io/ggally/dev/reference/add_to_ggmatrix.md)
([\#362](https://github.com/ggobi/ggally/issues/362)) \* Add ggplot2
objects to `ggmatrix` objects at selected locations \* Locations can be
rows, columns, matrices, or other shorthand values.

[`ggally_autopoint()`](https://ggobi.github.io/ggally/dev/reference/ggally_autopoint.md),
[`ggally_autopointDiag()`](https://ggobi.github.io/ggally/dev/reference/ggally_autopoint.md)
([@larmarange](https://github.com/larmarange),
[\#325](https://github.com/ggobi/ggally/issues/325)) \* Make
scatterplots compatible with both continuous and categorical variables
using
[`ggforce::geom_autopoint()`](https://ggforce.data-imaginist.com/reference/geom_autopoint.html).

[`ggally_colbar()`](https://ggobi.github.io/ggally/dev/reference/ggally_colbar.md),
[`ggally_rowbar()`](https://ggobi.github.io/ggally/dev/reference/ggally_colbar.md)
([@larmarange](https://github.com/larmarange),
[\#324](https://github.com/ggobi/ggally/issues/324)) \* Plot column or
row percentage using bar plots.

[`ggally_count()`](https://ggobi.github.io/ggally/dev/reference/ggally_count.md),
[`ggally_countDiag()`](https://ggobi.github.io/ggally/dev/reference/ggally_count.md)
([@larmarange](https://github.com/larmarange),
[\#321](https://github.com/ggobi/ggally/issues/321)) \* Plot the number
of observations by using rectangles with proportional areas.

[`ggally_cross()`](https://ggobi.github.io/ggally/dev/reference/ggally_cross.md)
([@larmarange](https://github.com/larmarange),
[\#326](https://github.com/ggobi/ggally/issues/326)) \* Plot the number
of observations by using square points with proportional areas.

[`ggally_crosstable()`](https://ggobi.github.io/ggally/dev/reference/ggally_crosstable.md)
([@larmarange](https://github.com/larmarange),
[\#351](https://github.com/ggobi/ggally/issues/351)) \* Display a
cross-tabulated table.

[`ggally_statistic()`](https://ggobi.github.io/ggally/dev/reference/ggally_statistic.md)
([\#327](https://github.com/ggobi/ggally/issues/327)) \* A generalized
version of
[`ggally_cor()`](https://ggobi.github.io/ggally/dev/reference/ggally_cor.md)
\* Use this method to create functions similar to
[`ggally_cor()`](https://ggobi.github.io/ggally/dev/reference/ggally_cor.md)
that return any text value given and `x` and `y` vector of data

[`ggally_summarise_by()`](https://ggobi.github.io/ggally/dev/reference/ggally_summarise_by.md)
([@larmarange](https://github.com/larmarange),
[\#325](https://github.com/ggobi/ggally/issues/325)) \* Display summary
statistics of a continuous variable for each value of a discrete
variable.

[`ggally_table()`](https://ggobi.github.io/ggally/dev/reference/ggally_table.md)
([@larmarange](https://github.com/larmarange),
[\#326](https://github.com/ggobi/ggally/issues/326)) \* Plot the number
of observations as a table.

[`ggally_trends()`](https://ggobi.github.io/ggally/dev/reference/ggally_trends.md)
([@larmarange](https://github.com/larmarange),
[\#333](https://github.com/ggobi/ggally/issues/333)) \* Plot trends
using line plots.

[`signif_stars()`](https://larmarange.github.io/ggstats/reference/signif_stars.html)
([@larmarange](https://github.com/larmarange),
[\#327](https://github.com/ggobi/ggally/issues/327)) \* Return the
appropriate number of significance stars as a character vector for the
provided numeric input values.

#### New `ggplot2` plot statistics:

[`stat_cross()`](https://larmarange.github.io/ggstats/reference/stat_cross.html)
([@larmarange](https://github.com/larmarange),
[\#326](https://github.com/ggobi/ggally/issues/326)) \* Computes
statistics of a 2-dimensional matrix using
[`broom::augment.htest`](https://broom.tidymodels.org/reference/augment.htest.html).

[`stat_prop()`](https://larmarange.github.io/ggstats/reference/stat_prop.html)
([@larmarange](https://github.com/larmarange),
[\#324](https://github.com/ggobi/ggally/issues/324)) \* Compute
proportions according to custom denominator.

[`stat_weighted_mean()`](https://larmarange.github.io/ggstats/reference/stat_weighted_mean.html)
([@larmarange](https://github.com/larmarange),
[\#333](https://github.com/ggobi/ggally/issues/333)) \* Compute the mean
of y aesthetic for each unique value of x, taking into account weight
aesthetic if provided.

#### Major updates

[`ggally_cor()`](https://ggobi.github.io/ggally/dev/reference/ggally_cor.md)
([\#327](https://github.com/ggobi/ggally/issues/327)) \* New
implementation using
[`ggally_statistic()`](https://ggobi.github.io/ggally/dev/reference/ggally_statistic.md)
\* Will now hide the grid by default and add a border
(`displayGrid = FALSE`) \* Added the ability to display significance
stars (`stars = TRUE`) \* Alignment has been fixed so both short and
long names should be displayed within view. `alignPercent` now
corresponds to the center of the text. \* Added the ability to separate
the arguments sent to the title and the groups (`title_args` and
`group_args`) \* Digits now represents the total number of digits after
the decimal place. \* To use the old version, change your
[`ggally_cor()`](https://ggobi.github.io/ggally/dev/reference/ggally_cor.md)
function calls to
[`ggally_cor_v1_5()`](https://ggobi.github.io/ggally/dev/reference/ggally_cor_v1_5.md).
\* Previously deprecated parameters have been removed

Website \* Updated to use `pkgdown`
([\#335](https://github.com/ggobi/ggally/issues/335))

#### Features and bug fixes:

[`ggpairs()`](https://ggobi.github.io/ggally/dev/reference/ggpairs.md)
([\#331](https://github.com/ggobi/ggally/issues/331)) \* New
`proportion` argument to control relative size of sub-plots \* option
`proportion = "auto"` for automatic guess based on the number of levels
for discrete variables

[`ggduo()`](https://ggobi.github.io/ggally/dev/reference/ggduo.md)
([\#331](https://github.com/ggobi/ggally/issues/331)) \* New
`xProportion` and `yProportion` arguments to control relative size of
sub-plots \* Set option `xProportion = "auto"` and
`yProportion = "auto"` for automatic guess based on the number of levels
for discrete variables

[`ggscatmat()`](https://ggobi.github.io/ggally/dev/reference/ggscatmat.md)
\*
[`lowertriangle()`](https://ggobi.github.io/ggally/dev/reference/lowertriangle.md)
now preallocates it’s memory usage for a 2-5x speed improvement.
([@vlepori](https://github.com/vlepori),
[\#328](https://github.com/ggobi/ggally/issues/328)) \* Fixed
`facet`’ing error where the factor order was not preserved. This error
caused the facets to be alphabetically sorted, cause plots to appear in
unexpected locations.
([\#355](https://github.com/ggobi/ggally/issues/355))

## GGally 1.5.0

CRAN release: 2020-03-25

- Updated to work with ggplot2 v3.3.0
  ([\#308](https://github.com/ggobi/ggally/issues/308))

`ggnet` and `ggnet2` \* Fixed some logic bugs from newer R versions

`ggally_box` and `ggally_dot` \* Label now appears axis and is displayed
in a plot matrix. ([\#253](https://github.com/ggobi/ggally/issues/253))

`ggsurv` \* Provide sensible legend values when multiple factors are
present. ([\#310](https://github.com/ggobi/ggally/issues/310))

`ggally_cor` \* Added `displayGrid` argument to turn of the background
grid. ([\#312](https://github.com/ggobi/ggally/issues/312))

### GGally 1.3.3

`ggpairs` and `ggduo`

- Become ggplot2 v2.2.2 compliant
  ([\#266](https://github.com/ggobi/ggally/issues/266))
- When retrieving functions with wrap, `ggally_*` functions do not
  require the GGally namespace
  ([\#269](https://github.com/ggobi/ggally/issues/269))
- Exported `eval_data_col`, `mapping_string`, and `mapping_swap_x_y`
  (5d157f6)
- Exported `is_horizontal` and `is_character_column`
  ([\#270](https://github.com/ggobi/ggally/issues/270))
- Logical values are now treated as discrete
  ([\#272](https://github.com/ggobi/ggally/issues/272))

`ggmatrix`

- `progress` parameter added to ggmatrix (and appropriate parent
  functions). Allows for `TRUE`, `FALSE`, `NULL`, and
  `function(pm){...}`
  ([\#271](https://github.com/ggobi/ggally/issues/271))

`ggnostic`

- Cooks distance cutoff is now at F\_{p, n - p}(0.5)
  ([\#274](https://github.com/ggobi/ggally/issues/274))

`ggnet2`

- Replaced loading packages with loading
  namespaces([\#262](https://github.com/ggobi/ggally/issues/262))

`ggally_smooth`

- Added `shrink` and `se` parameters to `ggally_smooth`
  ([\#247](https://github.com/ggobi/ggally/issues/247))

`ggcoef`

- Added `sort` parameter to sort by beta values
  ([\#273](https://github.com/ggobi/ggally/issues/273))

`ggparcoord`

- Fixed bug where x axis breaks and labels did not appear when
  `splineFactor = TRUE`
  ([\#279](https://github.com/ggobi/ggally/issues/279))

### GGally 1.3.2

`ggpairs` and `ggduo`

- Removed warning where pure numeric names gave a warning
  ([\#238](https://github.com/ggobi/ggally/issues/238),
  [@lepennec](https://github.com/lepennec))
- Fixed ordering issue with horizontal boxplots
  ([\#239](https://github.com/ggobi/ggally/issues/239))

`ggparcoord`

- Fixed missing `x` aes requirement when shadebox is provided
  ([\#237](https://github.com/ggobi/ggally/issues/237),
  [@treysp](https://github.com/treysp))

Package

- Made igraph a non required dependency for tests
  ([\#240](https://github.com/ggobi/ggally/issues/240))

### GGally 1.3.1

Added new dataset `psychademic`

- See
  [`?psychademic`](https://ggobi.github.io/ggally/dev/reference/psychademic.md)
  for more details
- (And updated the broken UCLA links)

Added original ggmatrix theme

- added function to set theme to have clear strip background and
  rearrange the strip positions
- added parameter `switch` to ggmatrix (and friends) to allow for strip
  repositioning. See `?ggplot::facet_grid` for more documentation on
  `switch` ([\#223](https://github.com/ggobi/ggally/issues/223),
  [\#224](https://github.com/ggobi/ggally/issues/224))

`ggsurv` error reporting

- removed a one error check that is covered in other places
  ([\#222](https://github.com/ggobi/ggally/issues/222))

`+.gg`

- allow to add a list of items to a ggmatrix
  ([\#228](https://github.com/ggobi/ggally/issues/228))

`ggmatrix.print`

- fix strip issues with ggplot2 name update

### GGally 1.3.0

`ggmatrix.print` - massive update!

- Now prints with a ggplot2 facet’ed structure
- Column titles are now placed in the strip of a plot matrix
- If there are 16 plots or more, a progress bar is displayed
  automatically (if interactive). Please look at the documentation for
  `ggmatrix_gtable` more details.

`ggmatrix` legend

- A legend may be added with the `legend` parameter in `ggduo`,
  `ggpairs`, and `ggmatrix`
- May specify a (length two) numeric plot coordinate
- May specify a (length one) numeric plot position
- May specify a legend object retrieved from `grab_legend`

`ggnostic` - New function!

- Produces a `ggmatrix` of diagnostic plots from a model object
- Uses broom to retrieve model information
- Each column of the plot matrix is a predictor variable. The rows can
  display the response variables, fitted points, residuals, standardized
  residuals, leave one out model sigma values, diagonals of the hat
  matrix, and cook’s distance for each point.

`ggfacet` - New function!

- Produces single ggplot2 object
- interface is very similar to `ggduo` and `ggpairs`

`fn_switch` - New function!

- Provide many functions in a list but only call one function at run
  time according to a mapping value
- Useful for `ggnostic` for different behavior depending on the y
  variable
- Allows for a ‘default’ value for the default switch case

`ggmatrix` - allow custom labellers for facet labels

- Added labeller parameter which is supplied to
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
- Allows for labels with plotmath expressions

`ggmatrix` and
[`ggplot2::last_plot()`](https://ggplot2.tidyverse.org/reference/get_last_plot.html)

- If a `ggmatrix` object is printed,
  [`ggplot2::last_plot()`](https://ggplot2.tidyverse.org/reference/get_last_plot.html)
  will return the plot matrix

`ggmatrix` and ggplot2 labels

- [`ggplot2::labs`](https://ggplot2.tidyverse.org/reference/labs.html)
  `+`’ed to a ggmatrix object
- [`ggplot2::xlab`](https://ggplot2.tidyverse.org/reference/labs.html)
  and
  [`ggplot2::ylab`](https://ggplot2.tidyverse.org/reference/labs.html)
  may be `+`’ed to a ggmatrix object
- [`ggplot2::ggtitle`](https://ggplot2.tidyverse.org/reference/labs.html)
  `+`’ed to a ggmatrix object
- (anything that returns a class of “labels” may be added to a ggmatrix
  object)

`ggmatrix` and
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)

- `ggsave` now works with `ggmatrix` objects

`ggpairs` and `ggduo` check for cardinality
([\#197](https://github.com/ggobi/ggally/issues/197))

- Before creating a ggmatrix object, a check is made for
  character/factor columns
- If there are more than 15 (default) unique combinations, an error is
  thrown.
- Setting `cardinality_threshold` parameter to a higher value can fix
  the problem (knowing single cell plots may take more time to produce)
- Setting `cardinality_threshold` parameter to `NULL` can stop the check

`ggmatrix` plot proportions

- `ggmatrix` can set the plot proportions with the parameters
  `xProportions` and `yProportions`
- These will change the relative size of the plot panels produced.

`ggally_cor` colour aesthetic

- color must be a non-numeric value

`ggsurv`

- added boolean to allow for legend to not be sorted
- fixed bug where censored points with custom color didn’t match
  properly ([\#185](https://github.com/ggobi/ggally/issues/185))

Vignettes

- vignettes are now displayed using `packagedocs`. More info at
  <http://hafen.github.io/packagedocs/>

`ggally_box_no_facet` and `ggally_dot_no_facet`

- New methods added as defaults to pair with new ggmatrix print method

### GGally 1.2.0

install requirements

- relaxed install requirements on grid (5d06dfc, d57469a, 933bb14,
  73b314d)

ggduo - New!

- plot two grouped data in a plot matrix
  ([\#173](https://github.com/ggobi/ggally/issues/173))
- helpful for plotting two sets of columns, multivariate analysis, and
  canonical correlation analysis
- be sure to check out the examples!

ggally_smooth_loess - New!

- uses the loess method with drawing a line (1552f96)

ggally_smooth_lm - New!

- uses the lm method with drawing a line (1552f96)
- alias of ggally_smooth

ggmatrix.print

- fixed bug strips where causing spacing issue when printing axis labels
  (174630d)

ggnetworkmap

- fixed bug where checking for the package ‘intergraph’ couldn’t be
  reached

ggsurv

- changed default of plotting multiple censored data color to match the
  survival line

package testing

- added many more tests!

### GGally 1.1.0

ggcoef - New!

- plot model coefficients with broom and ggplot2 PR#162
- Plotting model coefficients
  (<http://www.r-statistics.com/2010/07/visualization-of-regression-coefficients-in-r/>)

gglegend - New!

- pull out the legend of a plot which can also be used in ggpairs
  PR#155, PR#169

ggally_densityDiag

- fixed bug where ‘…’ was not respected (d0fe633)

ggally_smooth

- added ‘method’ parameter (411213c)

ggally_ratio

- Does not call ggfluctuation2 anymore. PR#165

ggcorr

- fixed issue with unnamed correlation matrix used as input PR#146
- fixed issue undesired shifting when layout.exp was \> 0 PR#171

ggfluctuation2

- is being deprecated. Please use ggally_ratio instead PR#165

ggnetworkmap

- fixed issue with overlaying network on a world map PR#157

ggparcoord

- Fixed odd bug where a list was trying to be forced as a double PR#162

ggpairs

- Fixed improperly rotated axes with ggally_ratio PR#165

ggscatmat

- added ‘corMethod’ parameter for use in upper triangle PR#145

ggsurv

- size.est and size.ci parameters added PR#153
- ordering changed to reflect survival time PR#147
- added a vignette PR#154

wrap

- documentation updated PR#152
- changes default behavior only. If an argument is supplied, the
  argument will take precedence

github chat

- <https://gitter.im/ggobi/ggally> is the place to visit for general
  questions.

travis-ci

- cache packages for faster checking
- install covr and lintr from github for testing purposes

### GGally 1.0.1

ggparcoord

- fix handling of factor group variable PR#131

ggscatmat

- force all char columns to factors PR#134

print.ggmatrix

- add boolean for grid.newpage ggmatrix print method PR#126

### GGally 1.0.0

ggplot2

- GGally has been upgraded to run on the latest ggplot2 v1.1.0. PR#109

New functions

- ggmatrix. Make a generic matrix of ggplot2 plots
- ggnetworkmap. Plot a network with ggplot2 suitable for overlay on a
  ggmap::map ggplot, or other ggplot
- ggnet2. Function for plotting network objects using ggplot2, with
  additional control over graphical parameters that are not supported by
  the ggnet function

Vignettes

- glyph - new!
- ggmatrix - new!
- ggnetworkmap - new!
- ggpairs - new!
- ggscatmat - new!

ggmatrix

- allows for bracket notation when getting or setting plots. PR#61
- full control over axis labels and axis text. PR#107, PR#111

ggpairs

- is now wrapper to ggmatrix
- takes in ‘wrapped’ functions. This better handles the case of many
  different parameters being supplied to different plot types. PR#90
- dates are better handled in ggpairs. Still room for improvement for
  default behavior, but they do not cause errors. PR#58, PR#59
- displays a ‘NA’ plot when all or a combination of the data is NA.
  PR#119

ggcorr

- legend title expressions may be used. PR#55
- handles objects that may be coerced into a data.frame PR#70

gglyph

- changed geom_line to geom_path in gglyph. Fixes ordering issue. PR#51

ggparcoord

- remaining columns are passed through so aesthetics may be added later.
  PR#54
- fixed parcoord ordering issues with odd names. PR#106
- fixed scaling when unique length equals 1. PR#122

ggsurv

- color censored marks the same color as the line. PR#74
- allow for different censored color marks. PR#113

ggally_density

- add fake data points to extend the limits of the stat_density2d.
  PR#114

ggally_na

- new plot type!

Data

- removed cityServiceFirms
- added twitter_spambots
