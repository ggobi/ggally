# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.2 (2016-10-31) |
|system   |x86_64, darwin13.4.0         |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Indiana/Indianapolis |
|date     |2016-11-11                   |

## Packages

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

# Check results

29 packages

|package             |version | errors| warnings| notes|
|:-------------------|:-------|------:|--------:|-----:|
|BAS                 |1.4.2   |      0|        0|     0|
|clustrd             |1.0.9   |      0|        0|     0|
|DescribeDisplay     |0.2.5   |      0|        0|     0|
|eechidna            |0.1     |      0|        0|     0|
|freqparcoord        |1.0.1   |      0|        0|     0|
|ggbio               |1.22.0  |      1|        1|     5|
|ggmcmc              |1.1     |      0|        0|     0|
|heatmaply           |0.6.0   |      0|        0|     1|
|imageData           |0.1-26  |      0|        0|     0|
|isomiRs             |1.2.0   |      1|        0|     4|
|LANDD               |1.1.0   |      0|        0|     0|
|MAST                |1.0.0   |      1|        1|     2|
|MissingDataGUI      |0.2-5   |      1|        0|     0|
|nzelect             |0.2.0   |      0|        0|     0|
|ParamHelpers        |1.9     |      1|        0|     0|
|Pi                  |1.0.0   |      1|        0|     0|
|plotly              |4.5.2   |      2|        0|     1|
|PopGenReport        |2.2.2   |      0|        0|     1|
|qualvar             |0.1.0   |      0|        0|     0|
|robCompositions     |2.0.2   |      0|        0|     1|
|robustbase          |0.92-6  |      0|        1|     1|
|rwty                |1.0.1   |      0|        0|     0|
|SHELF               |1.2.1   |      0|        1|     0|
|specmine            |1.0     |      1|        0|     0|
|svdvis              |0.1     |      0|        0|     0|
|TCGAbiolinks        |2.2.5   |      0|        0|     4|
|toaster             |0.5.4   |      1|        0|     0|
|userfriendlyscience |0.5-1   |      1|        0|     0|
|vdmR                |0.2.2   |      2|        1|     0|

## BAS (1.4.2)
Maintainer: Merlise Clyde <clyde@stat.duke.edu>

0 errors | 0 warnings | 0 notes

## clustrd (1.0.9)
Maintainer: Angelos Markos <amarkos@gmail.com>

0 errors | 0 warnings | 0 notes

## DescribeDisplay (0.2.5)
Maintainer: Di Cook <dicook@monash.edu>  
Bug reports: https://github.com/ggobi/DescribeDisplay/issues

0 errors | 0 warnings | 0 notes

## eechidna (0.1)
Maintainer: Ben Marwick <benmarwick@gmail.com>

0 errors | 0 warnings | 0 notes

## freqparcoord (1.0.1)
Maintainer: Norm Matloff <normmatloff@gmail.com>

0 errors | 0 warnings | 0 notes

## ggbio (1.22.0)
Maintainer: Michael Lawrence <lawrence.michael@gene.com>  
Bug reports: https://github.com/tengfei/ggbio/issues

1 error  | 1 warning  | 5 notes

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
... 92 lines ...
  no non-missing arguments to min; returning Inf
Warning in max(g$layout[idx, ]$r) :
  no non-missing arguments to max; returning -Inf
Warning in min(g$layout[idx, ]$l) :
  no non-missing arguments to min; returning Inf
Warning in max(g$layout[idx, ]$r) :
  no non-missing arguments to max; returning -Inf
Error in mapply(child_vp, vp_name = vpname(x$layout), t = x$layout$t,  : 
  zero-length inputs cannot be mixed with those of non-zero length
Calls: <Anonymous> ... drawGTree -> makeContent -> makeContent.gtable -> mapply
Execution halted

checking for missing documentation entries ... WARNING
Undocumented S4 methods:
  generic '[' and siglist 'PlotList,numeric,missing,ANY'
  generic '[' and siglist 'Tracks,numeric,missing,ANY'
All user-level objects in a package (including S4 classes and methods)
should have documentation entries.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.

checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.

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
autoplot,matrix: warning in scale_x_continuous(breaks = x, label =
  x.lab, expand = c(0, 0)): partial argument match of 'label' to
  'labels'
autoplot,matrix: warning in scale_x_continuous(breaks = NULL, label =
  NULL, expand = c(0, 0)): partial argument match of 'label' to
  'labels'
print,Ideogram: warning in scale_y_continuous(breaks = 5, label =
  subchr): partial argument match of 'label' to 'labels'
.combineNames: no visible binding for global variable
... 53 lines ...
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

## ggmcmc (1.1)
Maintainer: Xavier Fernández i Marín <xavier.fim@gmail.com>  
Bug reports: https://github.com/xfim/ggmcmc/issues

0 errors | 0 warnings | 0 notes

## heatmaply (0.6.0)
Maintainer: Tal Galili <tal.galili@gmail.com>  
Bug reports: https://github.com/talgalili/heatmaply/issues

0 errors | 0 warnings | 1 note 

```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘d3heatmap’
```

## imageData (0.1-26)
Maintainer: Chris Brien <Chris.Brien@unisa.edu.au>

0 errors | 0 warnings | 0 notes

## isomiRs (1.2.0)
Maintainer: Lorena Pantano <lorena.pantano@gmail.com>

1 error  | 0 warnings | 4 notes

```
checking examples ... ERROR
Running examples in ‘isomiRs-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: isoNetwork
> ### Title: Clustering miRNAs-genes pairs in similar pattern expression
> ### Aliases: isoNetwork
> 
> ### ** Examples
> 
> 
> library(org.Mm.eg.db)
Error in library(org.Mm.eg.db) : 
  there is no package called ‘org.Mm.eg.db’
Execution halted

checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘org.Mm.eg.db’ ‘clusterProfiler’

checking for hidden files and directories ... NOTE
Found the following hidden files and directories:
  .travis.yml
These were most likely included in error. See section ‘Package
structure’ in the ‘Writing R Extensions’ manual.

checking top-level files ... NOTE
Non-standard file/directory found at top level:
  ‘TODO.md’

checking R code for possible problems ... NOTE
.apply_median: no visible global function definition for ‘rowMax’
.apply_median: no visible global function definition for ‘rowMin’
.clean_low_rate_changes: no visible binding for global variable ‘subs’
.clean_low_rate_changes: no visible binding for global variable ‘mir’
.clean_low_rate_changes: no visible binding for global variable ‘freq’
.clean_low_rate_changes: no visible binding for global variable
  ‘total_subs’
.clean_low_rate_changes: no visible binding for global variable
  ‘total_mir’
... 43 lines ...
isoPlotPosition: no visible binding for global variable ‘change’
isoPlotPosition: no visible global function definition for ‘n’
isoSelect.IsomirDataSeq : <anonymous>: no visible binding for global
  variable ‘mir’
isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
  variable ‘mir’
Undefined global functions or variables:
  Count DB X1 X2 add ambiguity average change condition current
  enrichGO error freq gene go group mir n ngene reference rowMax rowMin
  sel_genes subs t3 t5 term term_short total_mir total_subs type value
  y
```

## LANDD (1.1.0)
Maintainer: Shangzhao Qiu <qsz1328@gmail.com>

0 errors | 0 warnings | 0 notes

## MAST (1.0.0)
Maintainer: Andrew McDavid <Andrew_McDavid@urmc.rochester.edu>

1 error  | 1 warning  | 2 notes

```
checking examples ... ERROR
Running examples in ‘MAST-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: impute
> ### Title: impute missing continuous expression for plotting
> ### Aliases: impute
> 
> ### ** Examples
... 16 lines ...

stt_ll> plt <- ggplot(predicted)+aes(x=invlogit(muD),y=muC,xse=seD,yse=seC,col=sample)+
stt_ll+    facet_wrap(~primerid,scales="free_y")+theme_linedraw()+
stt_ll+    geom_point(size=0.5)+scale_x_continuous("Proportion expression")+
stt_ll+    scale_y_continuous("Estimated Mean")+
stt_ll+    stat_ell(aes(x=muD,y=muC),level=0.95, invert='x')

stt_ll> ## plot with inverse logit transformed x-axis
stt_ll> print(plt)
Error: stat_ell requires the following missing aesthetics: alpha
Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
'select()' returned many:many mapping between keys and columns
........................................................................................................................
Done!
Combining coefficients and standard errors
Calculating log-fold changes
Calculating likelihood ratio tests
Quitting from lines 252-269 (MAITAnalysis.Rmd) 
Error: processing vignette 'MAITAnalysis.Rmd' failed with diagnostics:
stat_ell requires the following missing aesthetics: alpha
Execution halted


checking installed package size ... NOTE
  installed size is  6.2Mb
  sub-directories of 1Mb or more:
    data   3.7Mb
    doc    1.9Mb

checking Rd line widths ... NOTE
Rd file 'ZlmFit-class.Rd':
  \examples lines wider than 100 characters:
     #Note that because we parse the expression, that the columns must be enclosed in backquotes to protect the \quote{+} and \quote{-} char ... [TRUNCATED]
     lrTest(zlmVbeta, Hypothesis('`PopulationCD154+VbetaUnresponsive` - `PopulationCD154-VbetaUnresponsive`'))
     waldTest(zlmVbeta, Hypothesis('`PopulationCD154+VbetaUnresponsive` - `PopulationCD154-VbetaUnresponsive`'))

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

## Pi (1.0.0)
Maintainer: Hai Fang <hfang@well.ox.ac.uk>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘glmnet’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## plotly (4.5.2)
Maintainer: Carson Sievert <cpsievert1@gmail.com>  
Bug reports: https://github.com/ropensci/plotly/issues

2 errors | 0 warnings | 1 note 

```
checking examples ... ERROR
Running examples in ‘plotly-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: style
> ### Title: Modify trace(s)
> ### Aliases: style
> 
> ### ** Examples
> 
> 
> p <- qplot(data = mtcars, wt, mpg, geom = c("point", "smooth"))
> # keep the hover info for points, but remove it for the line/ribbon
> style(p, hoverinfo = "none", traces = c(2, 3))
Error in gg2list(p, width = width, height = height, tooltip = tooltip,  : 
  attempt to apply non-function
Calls: style ... plotly_build.gg -> ggplotly -> ggplotly.ggplot -> gg2list
Execution halted

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

0 errors | 1 warning  | 1 note 

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Loading required package: robustbase
Loading required package: xtable
Loading required package: GGally

Error: processing vignette 'lmrob_simulation.Rnw' failed with diagnostics:
 chunk 6 (label = fig-example-design) 
Error : ggplot2 doesn't know how to deal with data of class matrix
Execution halted


checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘robustX’
```

## rwty (1.0.1)
Maintainer: Dan Warren <dan.l.warren@gmail.com>

0 errors | 0 warnings | 0 notes

## SHELF (1.2.1)
Maintainer: Jeremy Oakley <j.oakley@sheffield.ac.uk>  
Bug reports: https://github.com/OakleyJ/SHELF/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Quitting from lines 187-195 (Multivariate-normal-copula.Rmd) 
Error: processing vignette 'Multivariate-normal-copula.Rmd' failed with diagnostics:
'xAxisLabels' can only be a character vector or NULL
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

## svdvis (0.1)
Maintainer: Neo Christopher Chung <nchchung@gmail.com>

0 errors | 0 warnings | 0 notes

## TCGAbiolinks (2.2.5)
Maintainer: Antonio Colaprico <antonio.colaprico@ulb.ac.be>,
 Tiago Chedraoui Silva <tiagochst@usp.br>  
Bug reports: https://github.com/BioinformaticsFMRP/TCGAbiolinks/issues

0 errors | 0 warnings | 4 notes

```
checking installed package size ... NOTE
  installed size is 19.6Mb
  sub-directories of 1Mb or more:
    R      1.1Mb
    data   6.2Mb
    doc   12.1Mb

checking dependencies in R code ... NOTE
Missing or unexported object: ‘TCGAbiolinks::TCGAquery’

checking R code for possible problems ... NOTE
GDCquery_clinic: no visible binding for global variable ‘portions’
TCGAanalyze_Preprocessing: no visible global function definition for
  ‘assays’
TCGAvisualize_oncoprint: no visible binding for global variable ‘value’
checkTumorInput: no visible binding for global variable ‘project’
get.mutation.matrix: no visible binding for global variable ‘Tumor’
update.clinical.with.last.followup: no visible binding for global
  variable ‘bcr_patient_barcode’
Undefined global functions or variables:
  Tumor assays bcr_patient_barcode portions project value

checking Rd line widths ... NOTE
Rd file 'TCGAvisualize_starburst.Rd':
  \examples lines wider than 100 characters:
     SummarizedExperiment::rowRanges(met)$diffmean.g2.g1 <- -1*(SummarizedExperiment::rowRanges(met)$diffmean.g1.g2)

These lines will be truncated in the PDF manual.
```

## toaster (0.5.4)
Maintainer: Gregory Kanevsky <gregory.kanevsky@teradata.com>  
Bug reports: https://github.com/teradata-aster-field/toaster/issues

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘RODBC’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## userfriendlyscience (0.5-1)
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

2 errors | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘vdmR-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: vhist
> ### Title: Generate histogram with interactive functions
> ### Aliases: vhist
> 
> ### ** Examples
> 
> data(vsfuk2012)
> vhist(FertilityRate, vsfuk2012, "hist1", "vsfuk2012", fill=Type)
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
Error in grid.Call.graphics(L_downviewport, name$name, strict) : 
  Viewport 'panel.3-4-3-4' was not found
Calls: vhist ... downViewport -> downViewport.vpPath -> grid.Call.graphics
Execution halted

checking tests ... ERROR
Running the tests in ‘tests/run-all.R’ failed.
Last 13 lines of output:
  Viewport 'panel.3-4-3-4' was not found
  1: vhist(Sepal.Length, iris, "hist01", "iris", fill = Species) at /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks/vdmR.Rcheck/vdmR/tests/test-vdmR.R:7
  2: grid::downViewport("panel.3-4-3-4")
  3: downViewport.default("panel.3-4-3-4")
  4: downViewport(vpPath(name), strict, recording = recording)
  5: downViewport.vpPath(vpPath(name), strict, recording = recording)
  6: grid.Call.graphics(L_downviewport, name$name, strict)
  
  DONE ===========================================================================
  Error: Test failures
  In addition: Warning message:
  Placing tests in `inst/tests/` is deprecated. Please use `tests/testthat/` instead 
  Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
Quitting from lines 40-42 (vdmR-vignette.Rnw) 
Error: processing vignette 'vdmR-vignette.Rnw' failed with diagnostics:
Viewport 'panel.3-4-3-4' was not found
Execution halted

```

