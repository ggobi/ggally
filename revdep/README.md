# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.0 (2016-05-03) |
|system   |x86_64, darwin13.4.0         |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Indiana/Indianapolis |
|date     |2016-06-23                   |

## Packages

|package      |*  |version |date       |source               |
|:------------|:--|:-------|:----------|:--------------------|
|broom        |   |0.4.0   |2015-11-30 |CRAN (R 3.2.3)       |
|geosphere    |   |1.5-1   |2015-12-18 |CRAN (R 3.2.3)       |
|GGally       |*  |1.2.0   |2016-06-23 |local (ggobi/ggally) |
|ggplot2      |   |2.1.0   |2016-03-01 |CRAN (R 3.3.0)       |
|gtable       |   |0.2.0   |2016-02-26 |CRAN (R 3.2.3)       |
|igraph       |   |1.0.1   |2015-06-26 |CRAN (R 3.2.0)       |
|intergraph   |   |2.0-2   |2015-06-30 |CRAN (R 3.2.0)       |
|knitr        |   |1.13    |2016-05-09 |cran (@1.13)         |
|mapproj      |   |1.2-4   |2015-08-03 |CRAN (R 3.3.0)       |
|maps         |   |3.1.0   |2016-02-13 |CRAN (R 3.2.3)       |
|network      |   |1.13.0  |2015-09-19 |cran (@1.13.0)       |
|plyr         |   |1.8.4   |2016-06-08 |CRAN (R 3.3.0)       |
|RColorBrewer |   |1.1-2   |2014-12-07 |CRAN (R 3.2.0)       |
|reshape      |   |0.8.5   |2014-04-23 |CRAN (R 3.0.2)       |
|rmarkdown    |   |0.9.6   |2016-05-01 |cran (@0.9.6)        |
|roxygen2     |   |5.0.1   |2015-11-11 |CRAN (R 3.2.2)       |
|scagnostics  |   |0.2-4   |2012-11-05 |CRAN (R 3.1.0)       |
|scales       |   |0.4.0   |2016-02-26 |CRAN (R 3.2.3)       |
|sna          |   |2.3-2   |2014-01-14 |CRAN (R 3.2.2)       |
|survival     |   |2.39-4  |2016-05-11 |cran (@2.39-4)       |
|testthat     |   |1.0.2   |2016-04-23 |CRAN (R 3.3.0)       |

# Check results
22 packages

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

## ggbio (1.20.1)
Maintainer: Michael Lawrence <lawrence.michael@gene.com>  
Bug reports: https://github.com/tengfei/ggbio/issues

1 error  | 2 warnings | 6 notes

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
... 408 lines ...
> tracks(full = p1, reduce = p2, heights = c(5, 1)) + ylab("")
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

checking whether package ‘ggbio’ can be installed ... WARNING
Found the following significant warnings:
  Warning: subclass "ExonrankFilter" of class "BasicFilter" is not local and cannot be updated for new inheritance information; consider setClassUnion()
  Warning: replacing previous import ‘ggplot2::Position’ by ‘BiocGenerics::Position’ when loading ‘ggbio’
See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks/ggbio.Rcheck/00install.out’ for details.

checking for missing documentation entries ... WARNING
Undocumented S4 methods:
  generic '[' and siglist 'PlotList,numeric,missing,ANY'
  generic '[' and siglist 'Tracks,numeric,missing,ANY'
All user-level objects in a package (including S4 classes and methods)
should have documentation entries.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.

checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘BSgenome.Hsapiens.UCSC.hg19’ ‘Homo.sapiens’
  ‘TxDb.Mmusculus.UCSC.mm9.knownGene’ ‘EnsDb.Hsapiens.v75’

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

## ggmcmc (1.0)
Maintainer: Xavier Fernández i Marín <xavier.fim@gmail.com>  
Bug reports: https://github.com/xfim/ggmcmc/issues

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘ggmcmc-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: ggs_caterpillar
> ### Title: Caterpillar plot with thick and thin CI
> ### Aliases: ggs_caterpillar
> 
> ### ** Examples
> 
> data(linear)
> ggs_caterpillar(ggs(s))
> ggs_caterpillar(list(A=ggs(s), B=ggs(s))) # silly example duplicating the same model
Error: Unknown column 'description'
Execution halted
```

## imageData (0.1-21)
Maintainer: Chris Brien <Chris.Brien@unisa.edu.au>

0 errors | 0 warnings | 0 notes

## isomiRs (1.0.3)
Maintainer: Lorena Pantano <lorena.pantano@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Packages required but not available: ‘DiscriMiner’ ‘DESeq2’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## LANDD (1.0.0)
Maintainer: Shangzhao Qiu <qsz1328@gmail.com>

0 errors | 0 warnings | 0 notes

## MissingDataGUI (0.2-5)
Maintainer: Xiaoyue Cheng <xycheng@unomaha.edu>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘MissingDataGUI’ can be installed ... ERROR
Installation failed.
See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks/MissingDataGUI.Rcheck/00install.out’ for details.
```

## ParamHelpers (1.7)
Maintainer: Bernd Bischl <bernd_bischl@gmx.net>  
Bug reports: https://github.com/berndbischl/ParamHelpers/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/run-all.R’ failed.
Last 13 lines of output:
  1. Failure: dropParams (@test_dropParams.R#11) 
  2. Failure: filter empty paramset (@test_filterParams.R#5) 
  3. Failure: filter empty paramset (@test_filterParams.R#6) 
  4. Failure: filter empty paramset (@test_filterParams.R#7) 
  5. Failure: filter empty paramset (@test_filterParams.R#8) 
  6. Failure: filter empty paramset (@test_filterParams.R#9) 
  7. Failure: filter empty paramset (@test_filterParams.R#10) 
  8. Failure: filter empty paramset (@test_filterParams.R#11) 
  9. Failure: filter empty paramset (@test_filterParams.R#12) 
  1. ...
  
  Error: testthat unit tests failed
  Execution halted
```

## plotly (3.6.0)
Maintainer: Carson Sievert <cpsievert1@gmail.com>  
Bug reports: https://github.com/ropensci/plotly/issues

0 errors | 0 warnings | 0 notes

## PopGenReport (2.2.2)
Maintainer: Bernd Gruber <bernd.gruber@canberra.edu.au>

0 errors | 0 warnings | 0 notes

## qualvar (0.1.0)
Maintainer: Joel Gombin <joel.gombin@gmail.com>

0 errors | 0 warnings | 0 notes

## robCompositions (2.0.0)
Maintainer: Matthias Templ <templ@tuwien.ac.at>

0 errors | 0 warnings | 0 notes

## robustbase (0.92-6)
Maintainer: Martin Maechler <maechler@stat.math.ethz.ch>

0 errors | 0 warnings | 0 notes

## rwty (1.0.1)
Maintainer: Dan Warren <dan.l.warren@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘phangorn’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## specmine (1.0)
Maintainer: Christopher Costa <chrisbcl@hotmail.com>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘specmine’ can be installed ... ERROR
Installation failed.
See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks/specmine.Rcheck/00install.out’ for details.
```

## svdvis (0.1)
Maintainer: Neo Christopher Chung <nchchung@gmail.com>

0 errors | 0 warnings | 0 notes

## TCGAbiolinks (1.2.5)
Maintainer: Antonio Colaprico <antonio.colaprico@ulb.ac.be>,
 Tiago Chedraoui Silva <tiagochst@usp.br>  
Bug reports: https://github.com/BioinformaticsFMRP/TCGAbiolinks/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘TCGAbiolinks’ can be installed ... ERROR
Installation failed.
See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks/TCGAbiolinks.Rcheck/00install.out’ for details.
```

## toaster (0.5.1)
Maintainer: Gregory Kanevsky <gregory.kanevsky@teradata.com>  
Bug reports: https://github.com/teradata-aster-field/toaster/issues

0 errors | 0 warnings | 0 notes

## userfriendlyscience (0.4-1)
Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘userfriendlyscience’ can be installed ... ERROR
Installation failed.
See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks/userfriendlyscience.Rcheck/00install.out’ for details.
```

## vdmR (0.2.2)
Maintainer: Tomokazu Fujino <fujino@fwu.ac.jp>

0 errors | 0 warnings | 0 notes

