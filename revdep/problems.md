# auditor

Version: 0.2.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘DALEX’
    ```

# CINNA

Version: 1.1.41

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘circlize’
      All declared Imports should be used.
    ```

# eechidna

Version: 1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        data   5.1Mb
        doc    1.2Mb
    ```

# ggbio

Version: 1.28.0

## In both

*   checking examples ... ERROR
    ```
    ...
    > ###################################################
    > p1 <- autoplot(xRleList, stat = "identity")
    > p2 <- autoplot(xRleList, stat = "identity", geom = "point", color = "red")
    > tracks('line' = p1, "point" = p2)
    > 
    > 
    > ###################################################
    > ### code chunk number 25: rlel-slice
    > ###################################################
    > p1 <- autoplot(xRleList, type = "viewMaxs", stat = "slice", lower = 5)
    > p2 <- autoplot(xRleList, type = "viewMaxs", stat = "slice", lower = 5, geom = "heatmap")
    > tracks('bar' = p1, "heatmap" = p2)
    > 
    > 
    > ###################################################
    > ### code chunk number 26: txdb
    > ###################################################
    > library(TxDb.Hsapiens.UCSC.hg19.knownGene)
    Error in library(TxDb.Hsapiens.UCSC.hg19.knownGene) : 
      there is no package called 'TxDb.Hsapiens.UCSC.hg19.knownGene'
    Execution halted
    ```

*   checking whether package ‘ggbio’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: subclass "DoubleFilter" of class "AnnotationFilter" is not local and cannot be updated for new inheritance information; consider setClassUnion()
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks.noindex/ggbio/new/ggbio.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘BSgenome.Hsapiens.UCSC.hg19’ ‘Homo.sapiens’
      ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
      ‘TxDb.Mmusculus.UCSC.mm9.knownGene’ ‘EnsDb.Hsapiens.v75’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      'S4Vectors:::top_prenv' 'ggplot2:::add_ggplot' 'ggplot2:::cunion'
      'ggplot2:::rename_aes' 'ggplot2:::rescale01'
      'ggplot2:::set_last_plot'
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    layout_karyogram,GRanges: no visible binding for global variable 'xend'
    layout_karyogram,GRanges: no visible binding for global variable 'yend'
    layout_karyogram,GRanges: no visible binding for global variable 'y2'
    layout_karyogram,GRanges: no visible binding for global variable
      'yend2'
    layout_karyogram,GRanges: no visible binding for global variable 'name'
    plotFragLength,character-GRanges: no visible binding for global
      variable '.fragLength'
    plotSpliceSum,character-EnsDb: possible error in GRangesFilter(which,
      condition = "overlapping"): unused argument (condition =
      "overlapping")
    stat_mismatch,GRanges: no visible binding for global variable 'sts'
    stat_mismatch,GRanges: no visible binding for global variable 'eds'
    stat_mismatch,GRanges: no visible binding for global variable 'read'
    Undefined global functions or variables:
      .fragLength .layout_circle.stats .x breaks coefs data eds fe fl
      gieStain ideoCyto indexProbesProcessed midpoint mt name read se
      stepping sts value variable x xend y y.text y2 yend yend2
    Consider adding
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

# ggraptR

Version: 1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RSelenium’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DBI’ ‘GGally’ ‘RColorBrewer’ ‘Rcpp’ ‘assertthat’ ‘backports’
      ‘colorspace’ ‘colourpicker’ ‘evaluate’ ‘futile.options’ ‘gdtools’
      ‘gtable’ ‘htmltools’ ‘htmlwidgets’ ‘httpuv’ ‘labeling’ ‘lambda.r’
      ‘lazyeval’ ‘magrittr’ ‘miniUI’ ‘munsell’ ‘plyr’ ‘reshape’ ‘rprojroot’
      ‘scales’ ‘stringi’ ‘stringr’ ‘svglite’ ‘tibble’ ‘xtable’ ‘yaml’
      All declared Imports should be used.
    ```

# httk

Version: 1.8

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.1Mb
      sub-directories of 1Mb or more:
        data  13.1Mb
        doc    1.3Mb
    ```

# ICtest

Version: 0.3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘fICA’
    ```

# isomiRs

Version: 1.8.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘targetscan.Hs.eg.db’
    
    Package suggested but not available for checking: ‘org.Mm.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# ITNr

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Matrix’ ‘animation’ ‘comtradr’ ‘ndtv’ ‘statnet’ ‘xergm’
      All declared Imports should be used.
    ```

# jmv

Version: 0.8.6.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BayesFactor’ ‘GGally’ ‘GPArotation’ ‘MASS’ ‘PMCMR’ ‘R6’ ‘ROCR’
      ‘afex’ ‘car’ ‘emmeans’ ‘ggridges’ ‘lavaan’ ‘multcomp’ ‘mvnormtest’
      ‘nnet’ ‘psych’ ‘vcd’ ‘vcdExtra’
      All declared Imports should be used.
    ```

# LANDD

Version: 1.1.0

## In both

*   checking whether package ‘LANDD’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks.noindex/LANDD/new/LANDD.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘LANDD’ ...
** package ‘LANDD’ successfully unpacked and MD5 sums checked
** libs
/usr/local/clang4/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/Rcpp/include" -I"/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
/usr/local/clang4/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/Rcpp/include" -I"/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c normalize.cpp -o normalize.o
In file included from normalize.cpp:4:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/math/distributions/normal.hpp:19:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/math/special_functions/erf.hpp:15:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/math/special_functions/gamma.hpp:24:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/math/constants/constants.hpp:13:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/math/tools/convert_from_string.hpp:15:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/lexical_cast.hpp:32:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/lexical_cast/try_lexical_convert.hpp:42:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/lexical_cast/detail/converter_lexical.hpp:52:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/container/container_fwd.hpp:61:
/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/container/detail/std_fwd.hpp:27:1: warning: inline namespaces are a C++11 feature [-Wc++11-inline-namespace]
BOOST_MOVE_STD_NS_BEG
^
/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/move/detail/std_ns_begin.hpp:18:34: note: expanded from macro 'BOOST_MOVE_STD_NS_BEG'
   #define BOOST_MOVE_STD_NS_BEG _LIBCPP_BEGIN_NAMESPACE_STD
                                 ^
/usr/local/clang4/bin/../include/c++/v1/__config:388:52: note: expanded from macro '_LIBCPP_BEGIN_NAMESPACE_STD'
#define _LIBCPP_BEGIN_NAMESPACE_STD namespace std {inline namespace _LIBCPP_NAMESPACE {
                                                   ^
1 warning generated.
/usr/local/clang4/bin/clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang4/lib -o LANDD.so RcppExports.o normalize.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks.noindex/LANDD/new/LANDD.Rcheck/LANDD/libs
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘LANDD’
* removing ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks.noindex/LANDD/new/LANDD.Rcheck/LANDD’

```
### CRAN

```
* installing *source* package ‘LANDD’ ...
** package ‘LANDD’ successfully unpacked and MD5 sums checked
** libs
/usr/local/clang4/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/Rcpp/include" -I"/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
/usr/local/clang4/bin/clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/Rcpp/include" -I"/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c normalize.cpp -o normalize.o
In file included from normalize.cpp:4:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/math/distributions/normal.hpp:19:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/math/special_functions/erf.hpp:15:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/math/special_functions/gamma.hpp:24:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/math/constants/constants.hpp:13:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/math/tools/convert_from_string.hpp:15:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/lexical_cast.hpp:32:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/lexical_cast/try_lexical_convert.hpp:42:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/lexical_cast/detail/converter_lexical.hpp:52:
In file included from /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/container/container_fwd.hpp:61:
/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/container/detail/std_fwd.hpp:27:1: warning: inline namespaces are a C++11 feature [-Wc++11-inline-namespace]
BOOST_MOVE_STD_NS_BEG
^
/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/library.noindex/LANDD/BH/include/boost/move/detail/std_ns_begin.hpp:18:34: note: expanded from macro 'BOOST_MOVE_STD_NS_BEG'
   #define BOOST_MOVE_STD_NS_BEG _LIBCPP_BEGIN_NAMESPACE_STD
                                 ^
/usr/local/clang4/bin/../include/c++/v1/__config:388:52: note: expanded from macro '_LIBCPP_BEGIN_NAMESPACE_STD'
#define _LIBCPP_BEGIN_NAMESPACE_STD namespace std {inline namespace _LIBCPP_NAMESPACE {
                                                   ^
1 warning generated.
/usr/local/clang4/bin/clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang4/lib -o LANDD.so RcppExports.o normalize.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks.noindex/LANDD/old/LANDD.Rcheck/LANDD/libs
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘LANDD’
* removing ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally/revdep/checks.noindex/LANDD/old/LANDD.Rcheck/LANDD’

```
# MAST

Version: 1.6.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 36-54 (MAITAnalysis.Rmd) 
    Error: processing vignette 'MAITAnalysis.Rmd' failed with diagnostics:
    there is no package called 'TxDb.Hsapiens.UCSC.hg19.knownGene'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.1Mb
      sub-directories of 1Mb or more:
        R      1.0Mb
        data   4.1Mb
        doc    3.7Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    assay_idx: no visible global function definition for ‘assayNames’
    collectResiduals: no visible global function definition for
      ‘assayNames’
    collectResiduals: no visible global function definition for ‘assay<-’
    collectResiduals: no visible global function definition for
      ‘assayNames<-’
    primerAverage: no visible global function definition for ‘assay<-’
    primerAverage: no visible global function definition for ‘rowData<-’
    assay<-,SingleCellAssay-missing: no visible global function definition
      for ‘assay<-’
    assayNames<-,SingleCellAssay-character: no visible global function
      definition for ‘assayNames’
    assayNames<-,SingleCellAssay-character: no visible global function
      definition for ‘assays<-’
    exprs<-,SingleCellAssay-ANY: no visible global function definition for
      ‘assay<-’
    Undefined global functions or variables:
      assay<- assayNames assayNames<- assays<- rowData<-
    ```

# MCbiclust

Version: 1.4.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘GO.db’ ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# MissingDataGUI

Version: 0.2-5

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘gWidgetsRGtk2’ ‘cairoDevice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# NormalizeMets

Version: 0.25

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.3Mb
      sub-directories of 1Mb or more:
        doc   8.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘DiffCorr’
    ```

# nzelect

Version: 0.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6409 marked UTF-8 strings
    ```

# ParamHelpers

Version: 1.10

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/run-all.R’ failed.
    Last 13 lines of output:
         -2.53545942614891, 0.422975436239536, -2.47230070010972, -1.46055918222656), .algo = structure(c(1L, 
         1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L
         ), .Label = c("algo1", "algo2", "algo3"), class = "factor"), .repl = c(1L, 1L, 1L, 
         1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L)), row.names = c("5", 
         "7", "12", "15", "19", "2", "51", "6", "151", "20", "8", "22", "52", "21", "3", "11", 
         "17", "18", "1", "71", "81", "181"), class = "data.frame"), groups = .algo, maximise = c(y1 = FALSE, 
         y2 = FALSE))
      5: stop("formula missing")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 1036 SKIPPED: 1 FAILED: 1
      1. Error: plotEAF works (@test_plotEAF.R#27) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# Pi

Version: 1.8.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘XGR’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# plotly

Version: 4.7.1

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘sf’ ‘RSelenium’
    ```

# pmxTools

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘GGally’ ‘PKNCA’ ‘grid’ ‘magrittr’ ‘plyr’ ‘xpose’
      All declared Imports should be used.
    ```

# PopGenReport

Version: 3.0.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ecodist’
    ```

# randomForestExplainer

Version: 0.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘dtplyr’
      All declared Imports should be used.
    ```

# robCompositions

Version: 2.0.7

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘mvoutlier’, ‘StatDA’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5701 marked UTF-8 strings
    ```

# robustbase

Version: 0.93-0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘matrixStats’, ‘robustX’
    ```

# rrr

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# scPipe

Version: 1.2.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘org.Hs.eg.db’ ‘org.Mm.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# SeqSQC

Version: 1.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc       1.9Mb
        extdata   3.3Mb
    ```

# specmine

Version: 2.0.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rcytoscapejs’
    ```

# staRdom

Version: 1.0.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘readr’ ‘tools’
      All declared Imports should be used.
    ```

# survivALL

Version: 0.9.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘cowplot’
      All declared Imports should be used.
    ```

# toaster

Version: 0.5.5

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘igraph’
    ```

# TVTB

Version: 1.6.0

## In both

*   checking tests ...
    ```
     ERROR
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
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 225 SKIPPED: 2 FAILED: 2
      1. Error: all signatures work to completion (@test_plotInfo-methods.R#25) 
      2. Error: invalid metric/phenotype combination is detected (@test_plotInfo-methods.R#50) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
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
    Quitting from lines 574-581 (Introduction.Rmd) 
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    there is no package called 'EnsDb.Hsapiens.v75'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘EnsDb.Hsapiens.v75’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        R     2.1Mb
        doc   2.2Mb
    ```

# vdmR

Version: 0.2.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rdpack’ ‘maptools’ ‘rgeos’
      All declared Imports should be used.
    ```

# vidger

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
        doc    1.7Mb
    ```

