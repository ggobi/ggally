# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.1 (2017-06-30) |
|system   |x86_64, darwin15.6.0         |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Indiana/Indianapolis |
|date     |2017-08-02                   |

## Packages

|package      |*  |version |date       |source         |
|:------------|:--|:-------|:----------|:--------------|
|broom        |   |0.4.2   |2017-02-13 |CRAN (R 3.4.0) |
|chemometrics |   |1.4.2   |2017-03-17 |CRAN (R 3.4.0) |
|geosphere    |   |1.5-5   |2016-06-15 |CRAN (R 3.4.0) |
|GGally       |*  |1.3.1   |2017-06-08 |CRAN (R 3.4.1) |
|ggplot2      |   |2.2.1   |2016-12-30 |CRAN (R 3.4.0) |
|gtable       |   |0.2.0   |2016-02-26 |CRAN (R 3.4.0) |
|igraph       |   |1.1.2   |2017-07-21 |cran (@1.1.2)  |
|intergraph   |   |2.0-2   |2016-12-05 |CRAN (R 3.4.0) |
|mapproj      |   |1.2-5   |2017-06-08 |CRAN (R 3.4.0) |
|maps         |   |3.2.0   |2017-06-08 |cran (@3.2.0)  |
|network      |   |1.13.0  |2015-09-19 |CRAN (R 3.4.0) |
|packagedocs  |   |0.4.0   |2016-11-04 |CRAN (R 3.4.0) |
|plyr         |   |1.8.4   |2016-06-08 |CRAN (R 3.4.0) |
|progress     |   |1.1.2   |2016-12-14 |CRAN (R 3.4.0) |
|RColorBrewer |   |1.1-2   |2014-12-07 |CRAN (R 3.4.0) |
|reshape      |   |0.8.6   |2016-10-21 |CRAN (R 3.4.0) |
|rmarkdown    |   |1.6     |2017-06-15 |CRAN (R 3.4.0) |
|roxygen2     |   |6.0.1   |2017-02-06 |CRAN (R 3.4.0) |
|scagnostics  |   |0.2-4   |2012-11-05 |CRAN (R 3.4.0) |
|scales       |   |0.4.1   |2016-11-09 |CRAN (R 3.4.0) |
|sna          |   |2.4     |2016-08-08 |CRAN (R 3.4.0) |
|testthat     |   |1.0.2   |2016-04-23 |CRAN (R 3.4.0) |

# Check results

6 packages with problems

|package        |version | errors| warnings| notes|
|:--------------|:-------|------:|--------:|-----:|
|eechidna       |1.1     |      0|        1|     1|
|MissingDataGUI |0.2-5   |      1|        0|     0|
|nzelect        |0.3.3   |      0|        1|     0|
|PopGenReport   |3.0.0   |      0|        1|     1|
|POUMM          |1.3.2   |      1|        0|     0|
|specmine       |1.0     |      1|        0|     0|

## eechidna (1.1)
Maintainer: Ben Marwick <benmarwick@gmail.com>

0 errors | 1 warning  | 1 note 

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

    intersect, setdiff, setequal, union


Attaching package: 'purrr'

The following objects are masked from 'package:dplyr':
... 8 lines ...

Attaching package: 'scales'

The following object is masked from 'package:purrr':

    discard

Quitting from lines 155-172 (exploring-election-data.Rmd) 
Error: processing vignette 'exploring-election-data.Rmd' failed with diagnostics:
Value of SET_STRING_ELT() must be a 'CHARSXP' not a 'logical'
Execution halted

checking installed package size ... NOTE
  installed size is  6.3Mb
  sub-directories of 1Mb or more:
    data   4.9Mb
    doc    1.2Mb
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

## nzelect (0.3.3)
Maintainer: Peter Ellis <peter.ellis2013nz@gmail.com>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Quitting from lines 208-225 (README.Rmd) 
Error: processing vignette 'README.Rmd' failed with diagnostics:
Evaluation error: votes, parties, and electorates should all be vectors of the same length.
Execution halted

```

## PopGenReport (3.0.0)
Maintainer: Bernd Gruber <bernd.gruber@canberra.edu.au>

0 errors | 1 warning  | 1 note 

```
checking whether package ‘PopGenReport’ can be installed ... WARNING
Found the following significant warnings:
  Warning: namespace ‘DBI’ is not available and has been replaced
See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks/PopGenReport.Rcheck/00install.out’ for details.

checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘ecodist’
```

## POUMM (1.3.2)
Maintainer: Venelin Mitov <vmitov@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘POUMM’ can be installed ... ERROR
Installation failed.
See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks/POUMM.Rcheck/00install.out’ for details.
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

