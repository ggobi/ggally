# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.0 (2017-04-21) |
|system   |x86_64, darwin15.6.0         |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Indiana/Indianapolis |
|date     |2017-06-02                   |

## Packages

|package      |*  |version |date       |source                  |
|:------------|:--|:-------|:----------|:-----------------------|
|broom        |   |0.4.2   |2017-02-13 |CRAN (R 3.4.0)          |
|chemometrics |   |1.4.2   |2017-03-17 |CRAN (R 3.4.0)          |
|geosphere    |   |1.5-5   |2016-06-15 |CRAN (R 3.4.0)          |
|GGally       |*  |1.3.1   |2017-06-02 |local (ggobi/ggally@NA) |
|ggplot2      |   |2.2.1   |2016-12-30 |CRAN (R 3.4.0)          |
|gtable       |   |0.2.0   |2016-02-26 |CRAN (R 3.4.0)          |
|igraph       |   |1.0.1   |2015-06-26 |CRAN (R 3.4.0)          |
|intergraph   |   |2.0-2   |2016-12-05 |CRAN (R 3.4.0)          |
|mapproj      |   |1.2-4   |2015-08-03 |CRAN (R 3.4.0)          |
|maps         |   |3.1.1   |2016-07-27 |CRAN (R 3.4.0)          |
|network      |   |1.13.0  |2015-09-19 |CRAN (R 3.4.0)          |
|packagedocs  |   |0.4.0   |2016-11-04 |CRAN (R 3.4.0)          |
|plyr         |   |1.8.4   |2016-06-08 |CRAN (R 3.4.0)          |
|progress     |   |1.1.2   |2016-12-14 |CRAN (R 3.4.0)          |
|RColorBrewer |   |1.1-2   |2014-12-07 |CRAN (R 3.4.0)          |
|reshape      |   |0.8.6   |2016-10-21 |CRAN (R 3.4.0)          |
|rmarkdown    |   |1.5     |2017-04-26 |CRAN (R 3.4.0)          |
|roxygen2     |   |6.0.1   |2017-02-06 |CRAN (R 3.4.0)          |
|scagnostics  |   |0.2-4   |2012-11-05 |CRAN (R 3.4.0)          |
|scales       |   |0.4.1   |2016-11-09 |CRAN (R 3.4.0)          |
|sna          |   |2.4     |2016-08-08 |CRAN (R 3.4.0)          |
|testthat     |   |1.0.2   |2016-04-23 |CRAN (R 3.4.0)          |

# Check results

6 packages with problems

|package        |version | errors| warnings| notes|
|:--------------|:-------|------:|--------:|-----:|
|ggbio          |1.24.0  |      1|        0|     5|
|MissingDataGUI |0.2-5   |      1|        0|     0|
|POUMM          |1.2.2   |      0|        1|     0|
|specmine       |1.0     |      1|        0|     0|
|TVTB           |1.2.0   |      1|        1|     1|
|vortexR        |1.0.3   |      1|        0|     0|

## ggbio (1.24.0)
Maintainer: Michael Lawrence <lawrence.michael@gene.com>  
Bug reports: https://github.com/tengfei/ggbio/issues

1 error  | 0 warnings | 5 notes

```
checking examples ... ERROR
Running examples in ‘ggbio-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: autoplot
> ### Title: Generic autoplot function
> ### Aliases: autoplot autoplot,GRanges-method autoplot,GRangesList-method
> ###   autoplot,IRanges-method autoplot,Seqinfo-method
> ###   autoplot,BSgenome-method autoplot,GAlignments-method
... 416 lines ...
reduce alignemnts...
> 
> 
> ###################################################
> ### EnsDb
> ###################################################
> ## Fetching gene models from an EnsDb object.
> library(EnsDb.Hsapiens.v75)
Error in library(EnsDb.Hsapiens.v75) : 
  there is no package called 'EnsDb.Hsapiens.v75'
Execution halted

checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘BSgenome.Hsapiens.UCSC.hg19’ ‘EnsDb.Hsapiens.v75’

checking top-level files ... NOTE
Non-standard file/directory found at top level:
  ‘TODO.org’

checking dependencies in R code ... NOTE
Unexported objects imported by ':::' calls:
  'S4Vectors:::top_prenv' 'ggplot2:::add_ggplot' 'ggplot2:::cunion'
  'ggplot2:::rename_aes' 'ggplot2:::rescale01'
  'ggplot2:::set_last_plot'
  See the note in ?`:::` about the use of this operator.

checking R code for possible problems ... NOTE
.combineNames: no visible binding for global variable
  '.layout_circle.stats'
Ideogram: no visible global function definition for 'data'
Ideogram: no visible binding for global variable 'ideoCyto'
Ideogram: no visible binding for global variable 'cytobands'
ScalePlot: no visible binding for global variable 'y'
ScalePlot2: no visible binding for global variable 'breaks'
ScalePlot2: no visible binding for global variable 'yend'
ScalePlot2: no visible binding for global variable 'y.text'
... 48 lines ...
  .fragLength .layout_circle.stats .x breaks coefs cytobands data eds
  fe fl gieStain ideoCyto indexProbesProcessed midpoint mt name read se
  stepping sts value variable x xend y y.text y2 yend yend2
Consider adding
  importFrom("utils", "data")
to your NAMESPACE file.

Found the following calls to data() loading into the global environment:
File ‘ggbio/R/ideogram.R’:
  data(ideoCyto, package = "biovizBase")
See section ‘Good practice’ in ‘?data’.

checking Rd line widths ... NOTE
Rd file 'autoplot-method.Rd':
  \usage lines wider than 90 characters:
     autoplot(object, ..., type = c("heatmap", "link", "pcp", "boxplot", "scatterplot.matrix"), pheno.plot = FALSE,

Rd file 'geom_arrowrect-method.Rd':
  \examples lines wider than 100 characters:
     ggplot(gr) + geom_arrowrect(gr, stat = "stepping", aes(y = value, group = pair), group.selfish = FALSE)

Rd file 'plotSingleChrom.Rd':
  \usage lines wider than 90 characters:
     plotIdeogram(obj, subchr = NULL, zoom.region = NULL, which = NULL, xlab, ylab, main, xlabel =

These lines will be truncated in the PDF manual.
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

## POUMM (1.2.2)
Maintainer: Venelin Mitov <vmitov@gmail.com>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
  the condition has length > 1 and only the first element will be used
Warning in if (alpha == 0) { :
  the condition has length > 1 and only the first element will be used
Warning in window.mcmc(mc$mcmc, start = start, end = end, thin = thinMCMC) :
  end value not changed
Warning in if (alpha == 0) { :
  the condition has length > 1 and only the first element will be used
... 8 lines ...
  the condition has length > 1 and only the first element will be used
Warning in FUN(X[[i]], ...) : end value not changed
Warning in FUN(X[[i]], ...) : end value not changed
Warning in FUN(X[[i]], ...) : end value not changed
Warning in FUN(X[[i]], ...) : end value not changed
Warning in FUN(X[[i]], ...) : end value not changed
Warning in FUN(X[[i]], ...) : end value not changed
Quitting from lines 393-404 (UserGuide.Rmd) 
Error: processing vignette 'UserGuide.Rmd' failed with diagnostics:
package 'adaptMCMC' not found
Execution halted
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

## TVTB (1.2.0)
Maintainer: Kevin Rue-Albrecht <kevinrue67@gmail.com>  
Bug reports: https://github.com/kevinrue/TVTB/issues

1 error  | 1 warning  | 1 note 

```
checking tests ... ERROR
  Running ‘testthat.R’ [18s/19s]
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
    rs1426654 rs150379789 rs570906312 rs538198029 rs553496066 rs574775672 
         TRUE        TRUE       FALSE       FALSE        TRUE       FALSE 
  rs140666229 rs556950130 rs575303689 rs147513140 rs187525777 rs192454382 
         TRUE       FALSE        TRUE        TRUE       FALSE       FALSE 
  rs184818838 rs566886499 rs199924625 rs555872528 rs531820822 rs201353600 
        FALSE       FALSE       FALSE       FALSE       FALSE       FALSE 
  rs550201688 rs565338261 rs201239799 rs200461129 rs146726548 
        FALSE       FALSE       FALSE       FALSE        TRUE 
  testthat results ================================================================
  OK: 229 SKIPPED: 0 FAILED: 2
  1. Error: all signatures work to completion (@test_plotInfo-methods.R#25) 
  2. Error: invalid metric/phenotype combination is detected (@test_plotInfo-methods.R#50) 
  
  Error: testthat unit tests failed
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Overwriting INFO keys in data:
- REF
- HET
- ALT
- AAF
- MAF
Overwriting INFO keys in header:
- REF
- HET
- ALT
- AAF
- MAF
Quitting from lines 570-577 (Introduction.Rmd) 
Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
there is no package called 'EnsDb.Hsapiens.v75'
Execution halted


checking package dependencies ... NOTE
Package suggested but not available for checking: ‘EnsDb.Hsapiens.v75’
```

## vortexR (1.0.3)
Maintainer: Carlo Pacioni <C.Pacioni@Murdoch.edu.au>  
Bug reports: https://github.com/carlopacioni/vortexR/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  Improvements in best and average IC have bebingo en below the specified goals.
  Algorithm is declared to have converged.
  Completed.
     user  system elapsed 
    0.789   0.032   0.666 
  1. Failure: test m_scatter (@test-plot.R#65) -----------------------------------
  `scatter.plot` has length 19, not length 18.
  
  
  testthat results ================================================================
  OK: 28 SKIPPED: 0 FAILED: 1
  1. Failure: test m_scatter (@test-plot.R#65) 
  
  Error: testthat unit tests failed
  Execution halted
```

