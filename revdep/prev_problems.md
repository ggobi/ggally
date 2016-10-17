# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.1 (2016-06-21) |
|system   |x86_64, darwin13.4.0         |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Indiana/Indianapolis |
|date     |2016-10-17                   |

## Packages

|package      |*  |version    |date       |source                         |
|:------------|:--|:----------|:----------|:------------------------------|
|broom        |   |0.4.1      |2016-06-24 |cran (@0.4.1)                  |
|geosphere    |   |1.5-5      |2016-06-15 |cran (@1.5-5)                  |
|GGally       |*  |1.2.0      |2016-10-17 |local (ggobi/ggally@NA)        |
|ggplot2      |   |2.1.0      |2016-03-01 |cran (@2.1.0)                  |
|gtable       |   |0.2.0      |2016-02-26 |cran (@0.2.0)                  |
|igraph       |   |1.0.1      |2015-06-26 |cran (@1.0.1)                  |
|intergraph   |   |2.0-2      |2015-06-30 |cran (@2.0-2)                  |
|knitr        |   |1.14       |2016-08-13 |cran (@1.14)                   |
|mapproj      |   |1.2-4      |2015-08-03 |cran (@1.2-4)                  |
|maps         |   |3.1.1      |2016-07-27 |cran (@3.1.1)                  |
|network      |   |1.13.0     |2015-09-19 |CRAN (R 3.3.1)                 |
|plyr         |   |1.8.4      |2016-06-08 |cran (@1.8.4)                  |
|RColorBrewer |   |1.1-2      |2014-12-07 |cran (@1.1-2)                  |
|reshape      |   |0.8.5      |2014-04-23 |cran (@0.8.5)                  |
|rmarkdown    |   |1.1        |2016-10-16 |cran (@1.1)                    |
|roxygen2     |   |5.0.1      |2015-11-11 |cran (@5.0.1)                  |
|scagnostics  |   |0.2-4      |2012-11-05 |cran (@0.2-4)                  |
|scales       |   |0.4.0.9003 |2016-10-17 |Github (hadley/scales@d58d83a) |
|sna          |   |2.4        |2016-08-08 |cran (@2.4)                    |
|survival     |   |2.39-5     |2016-06-26 |cran (@2.39-5)                 |
|testthat     |   |1.0.2      |2016-04-23 |cran (@1.0.2)                  |

# Check results

8 packages with problems

|package             |version | errors| warnings| notes|
|:-------------------|:-------|------:|--------:|-----:|
|eechidna            |0.1     |      1|        0|     0|
|heatmaply           |0.5.0   |      1|        0|     0|
|MissingDataGUI      |0.2-5   |      1|        0|     0|
|ParamHelpers        |1.9     |      1|        0|     0|
|plotly              |4.5.2   |      1|        0|     1|
|specmine            |1.0     |      1|        0|     0|
|toaster             |0.5.2   |      1|        0|     0|
|userfriendlyscience |0.4-1   |      1|        0|     0|

## eechidna (0.1)
Maintainer: Ben Marwick <benmarwick@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘eechidna-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: hexDat
> ### Title: Electorate hexagon data in a tidy form
> ### Aliases: hexDat
> ### Keywords: datasets
> 
> ### ** Examples
> 
> data(hexDat)
> library(plotly)
Error: package ‘ggplot2’ 2.1.0 was found, but > 2.1.0 is required by ‘plotly’
Execution halted
```

## heatmaply (0.5.0)
Maintainer: Tal Galili <tal.galili@gmail.com>  
Bug reports: https://github.com/talgalili/heatmaply/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘heatmaply’ can be installed ... ERROR
Installation failed.
See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks/heatmaply.Rcheck/00install.out’ for details.
```

## MissingDataGUI (0.2-5)
Maintainer: Xiaoyue Cheng <xycheng@unomaha.edu>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Packages required but not available: ‘gWidgetsRGtk2’ ‘cairoDevice’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## ParamHelpers (1.9)
Maintainer: Bernd Bischl <bernd_bischl@gmx.net>  
Bug reports: https://github.com/berndbischl/ParamHelpers/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/run-all.R’ failed.
Last 13 lines of output:
  3. Failure: convertParamSetToIrace work with vecparam (@test_convertParamSetToIrace.R#85) 
  b$b not equal to c("v", "w").
  target is NULL, current is character
  
  
  testthat results ================================================================
  OK: 1003 SKIPPED: 0 FAILED: 3
  1. Error: convertParamSetToIrace (@test_convertParamSetToIrace.R#32) 
  2. Failure: convertParamSetToIrace uses correct boundaries (@test_convertParamSetToIrace.R#69) 
  3. Failure: convertParamSetToIrace work with vecparam (@test_convertParamSetToIrace.R#85) 
  
  Error: testthat unit tests failed
  Execution halted
```

## plotly (4.5.2)
Maintainer: Carson Sievert <cpsievert1@gmail.com>  
Bug reports: https://github.com/ropensci/plotly/issues

1 error  | 0 warnings | 1 note 

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
      last_plot
  
  The following object is masked from 'package:stats':
  
      filter
  
  The following object is masked from 'package:graphics':
  
      layout
  
  > library("RSclient")
  Error in library("RSclient") : there is no package called 'RSclient'
  Execution halted

checking package dependencies ... NOTE
Package suggested but not available for checking: ‘RSclient’
```

## specmine (1.0)
Maintainer: Christopher Costa <chrisbcl@hotmail.com>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Packages required but not available: ‘xcms’ ‘MAIT’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## toaster (0.5.2)
Maintainer: Gregory Kanevsky <gregory.kanevsky@teradata.com>  
Bug reports: https://github.com/teradata-aster-field/toaster/issues

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘RODBC’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## userfriendlyscience (0.4-1)
Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘MBESS’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

