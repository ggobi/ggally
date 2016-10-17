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

23 packages

|package             |version | errors| warnings| notes|
|:-------------------|:-------|------:|--------:|-----:|
|BAS                 |1.4.2   |      0|        0|     0|
|DescribeDisplay     |0.2.5   |      0|        0|     0|
|eechidna            |0.1     |      1|        0|     0|
|freqparcoord        |1.0.1   |      0|        0|     0|
|ggmcmc              |1.1     |      0|        0|     0|
|heatmaply           |0.5.0   |      1|        0|     0|
|imageData           |0.1-26  |      0|        0|     0|
|LANDD               |1.1.0   |      0|        0|     0|
|MissingDataGUI      |0.2-5   |      1|        0|     0|
|nzelect             |0.2.0   |      0|        0|     0|
|ParamHelpers        |1.9     |      1|        0|     0|
|plotly              |4.5.2   |      1|        0|     1|
|PopGenReport        |2.2.2   |      0|        0|     1|
|qualvar             |0.1.0   |      0|        0|     0|
|robCompositions     |2.0.2   |      0|        0|     1|
|robustbase          |0.92-6  |      0|        0|     1|
|rwty                |1.0.1   |      0|        0|     0|
|SHELF               |1.2.1   |      0|        0|     0|
|specmine            |1.0     |      1|        0|     0|
|svdvis              |0.1     |      0|        0|     0|
|toaster             |0.5.2   |      1|        0|     0|
|userfriendlyscience |0.4-1   |      1|        0|     0|
|vdmR                |0.2.2   |      0|        0|     0|

## BAS (1.4.2)
Maintainer: Merlise Clyde <clyde@stat.duke.edu>

0 errors | 0 warnings | 0 notes

## DescribeDisplay (0.2.5)
Maintainer: Di Cook <dicook@monash.edu>  
Bug reports: https://github.com/ggobi/DescribeDisplay/issues

0 errors | 0 warnings | 0 notes

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

## freqparcoord (1.0.1)
Maintainer: Norm Matloff <normmatloff@gmail.com>

0 errors | 0 warnings | 0 notes

## ggmcmc (1.1)
Maintainer: Xavier Fernández i Marín <xavier.fim@gmail.com>  
Bug reports: https://github.com/xfim/ggmcmc/issues

0 errors | 0 warnings | 0 notes

## heatmaply (0.5.0)
Maintainer: Tal Galili <tal.galili@gmail.com>  
Bug reports: https://github.com/talgalili/heatmaply/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘heatmaply’ can be installed ... ERROR
Installation failed.
See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks/heatmaply.Rcheck/00install.out’ for details.
```

## imageData (0.1-26)
Maintainer: Chris Brien <Chris.Brien@unisa.edu.au>

0 errors | 0 warnings | 0 notes

## LANDD (1.1.0)
Maintainer: Shangzhao Qiu <qsz1328@gmail.com>

0 errors | 0 warnings | 0 notes

## MissingDataGUI (0.2-5)
Maintainer: Xiaoyue Cheng <xycheng@unomaha.edu>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Packages required but not available: ‘gWidgetsRGtk2’ ‘cairoDevice’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## nzelect (0.2.0)
Maintainer: Peter Ellis <peter.ellis2013nz@gmail.com>

0 errors | 0 warnings | 0 notes

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

## PopGenReport (2.2.2)
Maintainer: Bernd Gruber <bernd.gruber@canberra.edu.au>

0 errors | 0 warnings | 1 note 

```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘ecodist’
```

## qualvar (0.1.0)
Maintainer: Joel Gombin <joel.gombin@gmail.com>

0 errors | 0 warnings | 0 notes

## robCompositions (2.0.2)
Maintainer: Matthias Templ <templ@tuwien.ac.at>

0 errors | 0 warnings | 1 note 

```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘StatDA’
```

## robustbase (0.92-6)
Maintainer: Martin Maechler <maechler@stat.math.ethz.ch>

0 errors | 0 warnings | 1 note 

```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘robustX’
```

## rwty (1.0.1)
Maintainer: Dan Warren <dan.l.warren@gmail.com>

0 errors | 0 warnings | 0 notes

## SHELF (1.2.1)
Maintainer: Jeremy Oakley <j.oakley@sheffield.ac.uk>  
Bug reports: https://github.com/OakleyJ/SHELF/issues

0 errors | 0 warnings | 0 notes

## specmine (1.0)
Maintainer: Christopher Costa <chrisbcl@hotmail.com>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Packages required but not available: ‘xcms’ ‘MAIT’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## svdvis (0.1)
Maintainer: Neo Christopher Chung <nchchung@gmail.com>

0 errors | 0 warnings | 0 notes

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

## vdmR (0.2.2)
Maintainer: Tomokazu Fujino <fujino@fwu.ac.jp>

0 errors | 0 warnings | 0 notes

