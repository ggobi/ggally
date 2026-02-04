# Order axis variables

Order axis variables by separation between one class and the rest (most
separation to least).

## Usage

``` r
singleClassOrder(classVar, axisVars, specClass = NULL)
```

## Arguments

- classVar:

  class variable (vector from original dataset)

- axisVars:

  variables to be plotted as axes (data frame)

- specClass:

  character string matching to level of `classVar`; instead of looking
  for separation between any class and the rest, will only look for
  separation between this class and the rest

## Value

character vector of names of axisVars ordered such that the first
variable has the most separation between one of the classes and the
rest, and the last variable has the least (as measured by F-statistics
from an ANOVA)

## Author

Jason Crowley
