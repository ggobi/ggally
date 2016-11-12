
## Comments

### 2016-11-12

All revdep authors were emailed on 2016-10-17. Have only been in contact with the authors of 'plotly' about changes and 'SHELF' on how to update their vignette.

Please let me know if there is anything I can do.  Thank you for your time.

Best,
Barret


#### Test environments and R CMD check results

* local OS X install (x86_64-apple-darwin13.4.0), R 3.3.0
  * There were no ERRORs, WARNINGs, or NOTEs.

* travis-ci
  * Platform: x86_64-pc-linux-gnu (64-bit)
  * Running under: Ubuntu precise (12.04.5 LTS)
  * R
    * R version 3.3.1 (2016-06-21)
    * R Under development (unstable) (2016-11-12 r71649)
  * There were no ERRORs, WARNINGs, or NOTEs.

* win-builder (devel and release)
  * release
    * There were no ERRORs, WARNINGs, or NOTEs
  * devel
    * There were no ERRORs, WARNINGs, or NOTEs


## Reverse dependencies
I have run R CMD check on downstream dependencies of GGally on my local machine.
* Summary - https://github.com/ggobi/ggally/blob/master/revdep/README.md


### RevDep Notes

* In contact with author on how to resolve WARNING
  * SHELF: checking re-building of vignette outputs ... WARNING


* Failed to install dependencies for: MissingDataGUI, Pi, specmine, toaster, userfriendlyscience

* Does not appear to be a GGally issue.
  * isomiRs: checking examples ... ERROR
  * ParamHelpers: checking tests ... ERROR

* Does not appear to be a GGally issue.  Seems like a ggplot2 issue.
  * ggbio: checking examples ... ERROR
  * MAST: checking examples ... ERROR
  * plotly: checking examples ... ERROR
  * robustbase: checking re-building of vignette outputs ... WARNING
  * vdmR: checking examples ... ERROR


### RevDep output

#### Packages

|package      |*  |version |date       |source                  |
|:------------|:--|:-------|:----------|:-----------------------|
|broom        |   |0.4.1   |2016-06-24 |cran (@0.4.1)           |
|chemometrics |   |1.4.1   |2016-08-03 |cran (@1.4.1)           |
|geosphere    |   |1.5-5   |2016-06-15 |cran (@1.5-5)           |
|GGally       |*  |1.3.0   |2016-11-11 |local (ggobi/ggally@NA) |
|ggplot2      |   |2.2.0   |2016-11-11 |CRAN (R 3.3.2)          |
|gtable       |   |0.2.0   |2016-02-26 |cran (@0.2.0)           |
|igraph       |   |1.0.1   |2015-06-26 |cran (@1.0.1)           |
|intergraph   |   |2.0-2   |2015-06-30 |cran (@2.0-2)           |
|mapproj      |   |1.2-4   |2015-08-03 |cran (@1.2-4)           |
|maps         |   |3.1.1   |2016-07-27 |cran (@3.1.1)           |
|network      |   |1.13.0  |2015-09-19 |CRAN (R 3.3.1)          |
|packagedocs  |   |0.4.0   |2016-11-04 |CRAN (R 3.3.2)          |
|plyr         |   |1.8.4   |2016-06-08 |cran (@1.8.4)           |
|progress     |   |1.0.2   |2015-06-05 |cran (@1.0.2)           |
|RColorBrewer |   |1.1-2   |2014-12-07 |cran (@1.1-2)           |
|reshape      |   |0.8.6   |2016-10-21 |cran (@0.8.6)           |
|rmarkdown    |   |1.1     |2016-10-16 |cran (@1.1)             |
|roxygen2     |   |5.0.1   |2015-11-11 |cran (@5.0.1)           |
|scagnostics  |   |0.2-4   |2012-11-05 |cran (@0.2-4)           |
|scales       |   |0.4.1   |2016-11-09 |cran (@0.4.1)           |
|sna          |   |2.4     |2016-08-08 |cran (@2.4)             |
|survival     |   |2.40-1  |2016-10-30 |CRAN (R 3.3.2)          |
|testthat     |   |1.0.2   |2016-04-23 |cran (@1.0.2)           |

#### Check results

13 packages with problems

|package             |version | errors| warnings| notes|
|:-------------------|:-------|------:|--------:|-----:|
|ggbio               |1.22.0  |      1|        1|     5|
|isomiRs             |1.2.0   |      1|        0|     4|
|MAST                |1.0.0   |      1|        1|     2|
|MissingDataGUI      |0.2-5   |      1|        0|     0|
|ParamHelpers        |1.9     |      1|        0|     0|
|Pi                  |1.0.0   |      1|        0|     0|
|plotly              |4.5.2   |      2|        0|     1|
|robustbase          |0.92-6  |      0|        1|     1|
|SHELF               |1.2.1   |      0|        1|     0|
|specmine            |1.0     |      1|        0|     0|
|toaster             |0.5.4   |      1|        0|     0|
|userfriendlyscience |0.5-1   |      1|        0|     0|
|vdmR                |0.2.2   |      2|        1|     0|
