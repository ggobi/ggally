# adventr

<details>

* Version: 0.1.8
* GitHub: NA
* Source code: https://github.com/cran/adventr
* Date/Publication: 2020-05-05 16:50:06 UTC
* Number of recursive dependencies: 160

Run `revdep_details(, "adventr")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        tutorials   5.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BayesFactor’ ‘GGally’ ‘Hmisc’ ‘WRS2’ ‘boot’ ‘car’ ‘dplyr’ ‘effects’
      ‘effsize’ ‘forcats’ ‘ggplot2’ ‘lm.beta’ ‘nlme’ ‘readr’ ‘robust’
      ‘sandwich’ ‘sjstats’ ‘tidyr’
      All declared Imports should be used.
    ```

# AirSensor

<details>

* Version: 1.0.8
* GitHub: https://github.com/MazamaScience/AirSensor
* Source code: https://github.com/cran/AirSensor
* Date/Publication: 2021-03-12 21:40:09 UTC
* Number of recursive dependencies: 183

Run `revdep_details(, "AirSensor")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        data   4.7Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 90 marked UTF-8 strings
    ```

# alevinQC

<details>

* Version: 1.6.1
* GitHub: https://github.com/csoneson/alevinQC
* Source code: https://github.com/cran/alevinQC
* Date/Publication: 2021-02-02
* Number of recursive dependencies: 97

Run `revdep_details(, "alevinQC")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.7Mb
      sub-directories of 1Mb or more:
        extdata  11.6Mb
    ```

# ALPS

<details>

* Version: 1.4.0
* GitHub: https://github.com/itsvenu/ALPS
* Source code: https://github.com/cran/ALPS
* Date/Publication: 2020-10-27
* Number of recursive dependencies: 196

Run `revdep_details(, "ALPS")` for more info

</details>

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# AlpsNMR

<details>

* Version: 3.0.6
* GitHub: NA
* Source code: https://github.com/cran/AlpsNMR
* Date/Publication: 2021-03-31
* Number of recursive dependencies: 162

Run `revdep_details(, "AlpsNMR")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    ...
    > model <- nmr_pca_build_model(dataset_1D)
    > nmr_pca_plot_variance(model)
    > 
    > dir_to_demo_dataset <- system.file("dataset-demo", package = "AlpsNMR")
    > dataset <- nmr_read_samples_dir(dir_to_demo_dataset)
    > dataset_1D <- nmr_interpolate_1D(dataset, axis = c(min = -0.5, max = 10, by = 2.3E-4))
    > model <- nmr_pca_build_model(dataset_1D)
    > nmr_pca_scoreplot(dataset_1D, model)
    Error: Join columns must be present in data.
    ✖ Problem with `NMRExperiment`.
    Backtrace:
        █
     1. ├─AlpsNMR::nmr_pca_scoreplot(dataset_1D, model)
     2. │ └─`%>%`(...)
     3. ├─dplyr::left_join(., nmr_metadata, by = "NMRExperiment")
     4. └─dplyr:::left_join.data.frame(., nmr_metadata, by = "NMRExperiment")
     5.   └─dplyr:::join_mutate(...)
     6.     └─dplyr:::join_cols(...)
     7.       └─dplyr:::standardise_join_by(by, x_names = x_names, y_names = y_names)
     8.         └─dplyr:::check_join_vars(by$x, x_names)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ✖ Problem with `NMRExperiment`.
      Backtrace:
          █
       1. ├─AlpsNMR::nmr_pca_scoreplot(dataset, pca_built) test-outliers.R:12:2
       2. │ └─`%>%`(...)
       3. ├─dplyr::left_join(., nmr_metadata, by = "NMRExperiment")
       4. └─dplyr:::left_join.data.frame(., nmr_metadata, by = "NMRExperiment")
       5.   └─dplyr:::join_mutate(...)
       6.     └─dplyr:::join_cols(...)
       7.       └─dplyr:::standardise_join_by(by, x_names = x_names, y_names = y_names)
       8.         └─dplyr:::check_join_vars(by$x, x_names)
      
      [ FAIL 1 | WARN 2 | SKIP 1 | PASS 73 ]
      Error: Test failures
      Execution halted
    ```

*   checking Rd \usage sections ... WARNING
    ```
    ...
    
    Undocumented arguments in documentation object 'confusion_matrix'
      ‘MVObj’ ‘model’
    
    Undocumented arguments in documentation object 'model_VIP'
      ‘model’
    
    Undocumented arguments in documentation object 'rdCV_PLS_RF'
      ‘X’ ‘Y’ ‘ID’ ‘scale’ ‘nRep’ ‘nOuter’ ‘nInner’ ‘varRatio’ ‘DA’
      ‘fitness’ ‘method’ ‘nCompMax’ ‘methParam’ ‘ML’ ‘modReturn’ ‘logg’
      ‘parallel’
    
    Undocumented arguments in documentation object 'rdCV_PLS_RF_ML'
      ‘scale’ ‘nRep’ ‘nOuter’ ‘nInner’ ‘varRatio’ ‘DA’ ‘fitness’ ‘method’
      ‘ML’ ‘modReturn’ ‘logg’ ‘parallel’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘zip’
      All declared Imports should be used.
    ```

# BasketballAnalyzeR

<details>

* Version: 0.5.0
* GitHub: https://github.com/sndmrc/BasketballAnalyzeR
* Source code: https://github.com/cran/BasketballAnalyzeR
* Date/Publication: 2020-06-26 09:00:11 UTC
* Number of recursive dependencies: 74

Run `revdep_details(, "BasketballAnalyzeR")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘circlize’ ‘hexbin’ ‘scales’ ‘sna’
      All declared Imports should be used.
    ```

# BGGM

<details>

* Version: 2.0.3
* GitHub: https://github.com/donaldRwilliams/BGGM
* Source code: https://github.com/cran/BGGM
* Date/Publication: 2020-12-03 08:20:06 UTC
* Number of recursive dependencies: 165

Run `revdep_details(, "BGGM")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc    3.3Mb
        help   1.1Mb
    ```

# bigPint

<details>

* Version: 1.6.0
* GitHub: https://github.com/lindsayrutter/bigPint
* Source code: https://github.com/cran/bigPint
* Date/Publication: 2020-10-27
* Number of recursive dependencies: 162

Run `revdep_details(, "bigPint")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        data             2.1Mb
        doc              2.3Mb
        shiny-examples   3.0Mb
    ```

# bootcluster

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/bootcluster
* Date/Publication: 2021-06-10 08:20:04 UTC
* Number of recursive dependencies: 69

Run `revdep_details(, "bootcluster")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sna’
      All declared Imports should be used.
    ```

# CeTF

<details>

* Version: 1.2.4
* GitHub: NA
* Source code: https://github.com/cran/CeTF
* Date/Publication: 2020-11-23
* Number of recursive dependencies: 225

Run `revdep_details(, "CeTF")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   1.6Mb
        doc    2.5Mb
    ```

# CINNA

<details>

* Version: 1.1.54
* GitHub: NA
* Source code: https://github.com/cran/CINNA
* Date/Publication: 2021-01-28 15:00:02 UTC
* Number of recursive dependencies: 135

Run `revdep_details(, "CINNA")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘circlize’
      All declared Imports should be used.
    ```

# CluMSID

<details>

* Version: 1.6.0
* GitHub: https://github.com/tdepke/CluMSID
* Source code: https://github.com/cran/CluMSID
* Date/Publication: 2020-10-27
* Number of recursive dependencies: 167

Run `revdep_details(, "CluMSID")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        doc   5.6Mb
    ```

# communication

<details>

* Version: 0.1
* GitHub: NA
* Source code: https://github.com/cran/communication
* Date/Publication: 2021-02-25 09:20:02 UTC
* Number of recursive dependencies: 88

Run `revdep_details(, "communication")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘GGally’ ‘RColorBrewer’ ‘abind’ ‘diagram’ ‘ggplot2’ ‘grid’ ‘gtable’
      ‘igraph’ ‘magrittr’ ‘plyr’ ‘purrr’ ‘reshape2’ ‘scales’ ‘useful’
      All declared Imports should be used.
    ```

# comparer

<details>

* Version: 0.2.2
* GitHub: https://github.com/CollinErickson/comparer
* Source code: https://github.com/cran/comparer
* Date/Publication: 2021-03-29 09:10:09 UTC
* Number of recursive dependencies: 93

Run `revdep_details(, "comparer")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

# discourseGT

<details>

* Version: 1.1.6
* GitHub: NA
* Source code: https://github.com/cran/discourseGT
* Date/Publication: 2021-06-18 21:50:05 UTC
* Number of recursive dependencies: 103

Run `revdep_details(, "discourseGT")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BiocManager’ ‘ggpubr’ ‘sna’
      All declared Imports should be used.
    ```

# egoTERGM

<details>

* Version: 2.1.1
* GitHub: https://github.com/benjamin-w-campbell/egoTERGM
* Source code: https://github.com/cran/egoTERGM
* Date/Publication: 2019-05-17 13:20:03 UTC
* Number of recursive dependencies: 85

Run `revdep_details(, "egoTERGM")` for more info

</details>

## In both

*   checking whether package ‘egoTERGM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/egoTERGM/new/egoTERGM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘egoTERGM’ ...
** package ‘egoTERGM’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘btergm’:
 object ‘ergm.Cprepare’ is not exported by 'namespace:ergm'
Error: package ‘btergm’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘egoTERGM’
* removing ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/egoTERGM/new/egoTERGM.Rcheck/egoTERGM’

```
### CRAN

```
* installing *source* package ‘egoTERGM’ ...
** package ‘egoTERGM’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘btergm’:
 object ‘ergm.Cprepare’ is not exported by 'namespace:ergm'
Error: package ‘btergm’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘egoTERGM’
* removing ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/egoTERGM/old/egoTERGM.Rcheck/egoTERGM’

```
# ezEDA

<details>

* Version: 0.1.0
* GitHub: https://github.com/kviswana/ezEDA
* Source code: https://github.com/cran/ezEDA
* Date/Publication: 2020-06-25 09:20:06 UTC
* Number of recursive dependencies: 72

Run `revdep_details(, "ezEDA")` for more info

</details>

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      p$labels$y not identical to "count".
      Attributes: < Modes: list, NULL >
      Attributes: < Lengths: 1, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      ── Failure (test_two_category_tally.R:19:3): y axis is labeled 'count' ─────────
      p$labels$y not identical to "count".
      Attributes: < Modes: list, NULL >
      Attributes: < Lengths: 1, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      
      [ FAIL 8 | WARN 1 | SKIP 0 | PASS 79 ]
      Error: Test failures
      Execution halted
    ```

# fic

<details>

* Version: 1.0.0
* GitHub: https://github.com/chjackson/fic
* Source code: https://github.com/cran/fic
* Date/Publication: 2019-04-13 08:32:39 UTC
* Number of recursive dependencies: 103

Run `revdep_details(, "fic")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘numDeriv’
      All declared Imports should be used.
    ```

# fingerPro

<details>

* Version: 1.1
* GitHub: https://github.com/NA/NA
* Source code: https://github.com/cran/fingerPro
* Date/Publication: 2018-08-28 10:04:54 UTC
* Number of recursive dependencies: 162

Run `revdep_details(, "fingerPro")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RcppProgress’
      All declared Imports should be used.
    ```

# GDAtools

<details>

* Version: 1.7
* GitHub: https://github.com/nicolas-robette/GDAtools
* Source code: https://github.com/cran/GDAtools
* Date/Publication: 2021-05-31 09:40:08 UTC
* Number of recursive dependencies: 128

Run `revdep_details(, "GDAtools")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc   4.7Mb
    ```

# GENESIS

<details>

* Version: 2.20.1
* GitHub: https://github.com/UW-GAC/GENESIS
* Source code: https://github.com/cran/GENESIS
* Date/Publication: 2021-01-28
* Number of recursive dependencies: 164

Run `revdep_details(, "GENESIS")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        doc       3.2Mb
        extdata   3.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘survey:::saddle’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .pcrelate: no visible binding for global variable ‘k’
    calcISAFBeta: no visible binding for global variable ‘k’
    pcrelateSampBlock: no visible binding for global variable ‘k’
    Undefined global functions or variables:
      k
    ```

# ggbio

<details>

* Version: 1.38.0
* GitHub: https://github.com/tengfei/ggbio
* Source code: https://github.com/cran/ggbio
* Date/Publication: 2020-10-27
* Number of recursive dependencies: 166

Run `revdep_details(, "ggbio")` for more info

</details>

## In both

*   checking whether package ‘ggbio’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘BiocGenerics’ was built under R version 4.0.5
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/ggbio/new/ggbio.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    ':::' call which should be '::': 'ggplot2:::set_last_plot'
      See the note in ?`:::` about the use of this operator.
    Unexported objects imported by ':::' calls:
      'S4Vectors:::top_prenv' 'ggplot2:::add_ggplot' 'ggplot2:::cunion'
      'ggplot2:::rename_aes' 'ggplot2:::rescale01'
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

# GGMnonreg

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/GGMnonreg
* Date/Publication: 2021-04-08 11:30:06 UTC
* Number of recursive dependencies: 111

Run `revdep_details(, "GGMnonreg")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Matrix’ ‘Rdpack’
      All declared Imports should be used.
    ```

# GGPA

<details>

* Version: 1.2.0
* GitHub: https://github.com/dongjunchung/GGPA
* Source code: https://github.com/cran/GGPA
* Date/Publication: 2020-10-27
* Number of recursive dependencies: 68

Run `revdep_details(, "GGPA")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Packages in Depends field not imported from:
      ‘network’ ‘scales’ ‘sna’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking compiled code ... NOTE
    ```
    File ‘GGPA/libs/GGPA.so’:
      Found ‘__ZNSt3__14coutE’, possibly from ‘std::cout’ (C++)
        Object: ‘3_Param.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor use Fortran I/O
    nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# ggPMX

<details>

* Version: 1.2.3
* GitHub: https://github.com/ggPMXdevelopment/ggPMX
* Source code: https://github.com/cran/ggPMX
* Date/Publication: 2021-05-29 16:50:05 UTC
* Number of recursive dependencies: 110

Run `revdep_details(, "ggPMX")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.6Mb
      sub-directories of 1Mb or more:
        doc        1.1Mb
        help       2.1Mb
        testdata   4.8Mb
    ```

# ggquickeda

<details>

* Version: 0.2.0
* GitHub: https://github.com/smouksassi/ggquickeda
* Source code: https://github.com/cran/ggquickeda
* Date/Publication: 2021-02-15 12:40:02 UTC
* Number of recursive dependencies: 171

Run `revdep_details(, "ggquickeda")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘Formula’ ‘GGally’ ‘Hmisc’ ‘RPostgres’ ‘colourpicker’ ‘dplyr’
      ‘ggpmisc’ ‘ggpubr’ ‘ggrepel’ ‘gridExtra’ ‘markdown’ ‘plotly’
      ‘quantreg’ ‘rlang’ ‘shinyFiles’ ‘shinyjqui’ ‘shinyjs’ ‘survival’
      ‘survminer’ ‘table1’ ‘tidyr’ ‘zoo’
      All declared Imports should be used.
    ```

# greed

<details>

* Version: 0.5.1
* GitHub: https://github.com/comeetie/greed
* Source code: https://github.com/cran/greed
* Date/Publication: 2021-05-10 06:50:03 UTC
* Number of recursive dependencies: 129

Run `revdep_details(, "greed")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6693 marked UTF-8 strings
    ```

# httk

<details>

* Version: 2.0.4
* GitHub: https://github.com/USEPA/CompTox-ExpoCast-httk
* Source code: https://github.com/cran/httk
* Date/Publication: 2021-05-10 07:50:08 UTC
* Number of recursive dependencies: 118

Run `revdep_details(, "httk")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
        doc    1.5Mb
    ```

# isomiRs

<details>

* Version: 1.18.1
* GitHub: https://github.com/lpantano/isomiRs
* Source code: https://github.com/cran/isomiRs
* Date/Publication: 2021-01-29
* Number of recursive dependencies: 160

Run `revdep_details(, "isomiRs")` for more info

</details>

## In both

*   checking whether package ‘isomiRs’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘SummarizedExperiment’ was built under R version 4.0.3
      Warning: package ‘MatrixGenerics’ was built under R version 4.0.3
      Warning: package ‘GenomicRanges’ was built under R version 4.0.3
      Warning: package ‘BiocGenerics’ was built under R version 4.0.5
      Warning: package ‘S4Vectors’ was built under R version 4.0.3
      Warning: package ‘IRanges’ was built under R version 4.0.3
      Warning: package ‘GenomeInfoDb’ was built under R version 4.0.5
      Warning: package ‘Biobase’ was built under R version 4.0.3
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/isomiRs/new/isomiRs.Rcheck/00install.out’ for details.
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘targetscan.Hs.egMIRNA’
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Hs.egMIRBASE2FAMILY’
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Hs.egTARGETS’
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Hs.egTARGETSFULL’
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Mm.egMIRNA’
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Mm.egMIRBASE2FAMILY’
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Mm.egTARGETS’
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Mm.egTARGETSFULL’
    Undefined global functions or variables:
      as.tibble changes hits iso_sample pct targetscan.Hs.egMIRBASE2FAMILY
      targetscan.Hs.egMIRNA targetscan.Hs.egTARGETS
      targetscan.Hs.egTARGETSFULL targetscan.Mm.egMIRBASE2FAMILY
      targetscan.Mm.egMIRNA targetscan.Mm.egTARGETS
      targetscan.Mm.egTARGETSFULL total
    ```

# jmv

<details>

* Version: 1.2.23
* GitHub: NA
* Source code: https://github.com/cran/jmv
* Date/Publication: 2020-06-26 10:00:09 UTC
* Number of recursive dependencies: 186

Run `revdep_details(, "jmv")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BayesFactor’ ‘GGally’ ‘GPArotation’ ‘MASS’ ‘PMCMR’ ‘R6’ ‘ROCR’
      ‘afex’ ‘car’ ‘emmeans’ ‘ggridges’ ‘lavaan’ ‘multcomp’ ‘mvnormtest’
      ‘nnet’ ‘psych’ ‘vcd’ ‘vcdExtra’
      All declared Imports should be used.
    ```

# jsmodule

<details>

* Version: 1.1.7
* GitHub: https://github.com/jinseob2kim/jsmodule
* Source code: https://github.com/cran/jsmodule
* Date/Publication: 2021-03-14 00:20:02 UTC
* Number of recursive dependencies: 213

Run `revdep_details(, "jsmodule")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Cairo’ ‘survC1’
      All declared Imports should be used.
    ```

# LANDD

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/LANDD
* Date/Publication: 2016-10-01 01:14:24
* Number of recursive dependencies: 108

Run `revdep_details(, "LANDD")` for more info

</details>

## In both

*   checking R code for possible problems ... NOTE
    ```
    getGO: no visible global function definition for ‘new’
    getGO: no visible global function definition for ‘getGeneric’
    graph.kd: no visible global function definition for ‘as’
    Undefined global functions or variables:
      as getGeneric new
    Consider adding
      importFrom("methods", "as", "getGeneric", "new")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# loon.ggplot

<details>

* Version: 1.2.1
* GitHub: https://github.com/great-northern-diver/loon.ggplot
* Source code: https://github.com/cran/loon.ggplot
* Date/Publication: 2021-06-10 12:50:06 UTC
* Number of recursive dependencies: 88

Run `revdep_details(, "loon.ggplot")` for more info

</details>

## In both

*   checking whether package ‘loon.ggplot’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/loon.ggplot/new/loon.ggplot.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘loon.ggplot’ ...
** package ‘loon.ggplot’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘loon’:
 .onLoad failed in loadNamespace() for 'loon', details:
  call: structure(.External(.C_dotTcl, ...), class = "tclObj")
  error: [tcl] couldn't connect to display "".

Error: package ‘loon’ could not be loaded
In addition: Warning message:
In fun(libname, pkgname) : couldn't connect to display ""
Execution halted
ERROR: lazy loading failed for package ‘loon.ggplot’
* removing ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/loon.ggplot/new/loon.ggplot.Rcheck/loon.ggplot’

```
### CRAN

```
* installing *source* package ‘loon.ggplot’ ...
** package ‘loon.ggplot’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘loon’:
 .onLoad failed in loadNamespace() for 'loon', details:
  call: structure(.External(.C_dotTcl, ...), class = "tclObj")
  error: [tcl] couldn't connect to display "".

Error: package ‘loon’ could not be loaded
In addition: Warning message:
In fun(libname, pkgname) : couldn't connect to display ""
Execution halted
ERROR: lazy loading failed for package ‘loon.ggplot’
* removing ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/loon.ggplot/old/loon.ggplot.Rcheck/loon.ggplot’

```
# MAINT.Data

<details>

* Version: 2.6.1
* GitHub: NA
* Source code: https://github.com/cran/MAINT.Data
* Date/Publication: 2021-05-04 13:00:02 UTC
* Number of recursive dependencies: 77

Run `revdep_details(, "MAINT.Data")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        data   3.0Mb
        libs   1.2Mb
    ```

# MAST

<details>

* Version: 1.16.0
* GitHub: https://github.com/RGLab/MAST
* Source code: https://github.com/cran/MAST
* Date/Publication: 2020-10-27
* Number of recursive dependencies: 205

Run `revdep_details(, "MAST")` for more info

</details>

## In both

*   checking whether package ‘MAST’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘SingleCellExperiment’ was built under R version 4.0.3
      Warning: package ‘SummarizedExperiment’ was built under R version 4.0.3
      Warning: package ‘MatrixGenerics’ was built under R version 4.0.3
      Warning: package ‘GenomicRanges’ was built under R version 4.0.3
      Warning: package ‘BiocGenerics’ was built under R version 4.0.5
      Warning: package ‘S4Vectors’ was built under R version 4.0.3
      Warning: package ‘IRanges’ was built under R version 4.0.3
      Warning: package ‘GenomeInfoDb’ was built under R version 4.0.5
      Warning: package ‘Biobase’ was built under R version 4.0.3
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/MAST/new/MAST.Rcheck/00install.out’ for details.
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
      installed size is 10.2Mb
      sub-directories of 1Mb or more:
        data   3.7Mb
        doc    5.2Mb
    ```

# MCbiclust

<details>

* Version: 1.14.0
* GitHub: NA
* Source code: https://github.com/cran/MCbiclust
* Date/Publication: 2020-10-27
* Number of recursive dependencies: 174

Run `revdep_details(, "MCbiclust")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.6Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
        doc    5.5Mb
    ```

# mgcViz

<details>

* Version: 0.1.6
* GitHub: https://github.com/mfasiolo/mgcViz
* Source code: https://github.com/cran/mgcViz
* Date/Publication: 2020-03-04 15:10:02 UTC
* Number of recursive dependencies: 109

Run `revdep_details(, "mgcViz")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc   4.4Mb
    ```

# MissingDataGUI

<details>

* Version: 0.2-5
* GitHub: NA
* Source code: https://github.com/cran/MissingDataGUI
* Date/Publication: 2016-04-25 08:58:53
* Number of recursive dependencies: 91

Run `revdep_details(, "MissingDataGUI")` for more info

</details>

## In both

*   checking S3 generic/method consistency ... WARNING
    ```
    
    (R:85952): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking replacement functions ... WARNING
    ```
    
    (R:86126): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
    The argument of a replacement function which corresponds to the right
    hand side must be named ‘value’.
    ```

*   checking for missing documentation entries ... WARNING
    ```
    
    (R:86447): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
    All user-level objects in a package should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    
    (R:86546): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
    
    (R:86610): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
    
    (R:86648): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
    ```

*   checking dependencies in R code ... NOTE
    ```
    
    (R:85878): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
    ```

*   checking foreign function calls ... NOTE
    ```
    
    (R:86151): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    
    (R:86220): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
    ```

*   checking Rd \usage sections ... NOTE
    ```
    
    (R:86679): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# MOFA

<details>

* Version: 1.6.2
* GitHub: NA
* Source code: https://github.com/cran/MOFA
* Date/Publication: 2021-02-09
* Number of recursive dependencies: 90

Run `revdep_details(, "MOFA")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘MOFA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DataOptions
    > ### Title: DataOptions: set and retrieve data options
    > ### Aliases: DataOptions DataOptions<- DataOptions,MOFAmodel-method
    > ###   DataOptions<-,MOFAmodel,list-method
    > 
    > ### ** Examples
    > 
    > # load a trained MOFAmodel object
    > filepath <- system.file("extdata", "scMT_model.hdf5", package = "MOFAdata")
    > MOFAobject <- loadModel(filepath)
    Error in h5checktypeOrOpenLoc(file, readonly = TRUE, fapl = NULL, native = native) : 
      Error in h5checktypeOrOpenLoc(). Cannot open file. File '' does not exist.
    Calls: loadModel -> h5read -> h5checktypeOrOpenLoc
    Execution halted
    ```

*   checking whether package ‘MOFA’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: Please use MOFA2 instead of MOFA.  Package 'MOFA' is deprecated and
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/MOFA/new/MOFA.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘MOFAdata’
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        doc   4.6Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    plotEnrichmentDetailed: no visible binding for global variable
      ‘pathway’
    plotEnrichmentDetailed: no visible binding for global variable
      ‘feature.statistic’
    Undefined global functions or variables:
      feature.statistic pathway
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘PCGSE’
    ```

# MOFA2

<details>

* Version: 1.0.1
* GitHub: https://github.com/bioFAM/MOFA2
* Source code: https://github.com/cran/MOFA2
* Date/Publication: 2020-11-03
* Number of recursive dependencies: 222

Run `revdep_details(, "MOFA2")` for more info

</details>

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `p <- plot_data_overview(test_mofa2)` produced warnings.
      ── Failure (test_plot.R:18:2): plot data scatter ───────────────────────────────
      `p <- plot_data_scatter(test_mofa2, view = 1, factor = 1)` produced warnings.
      ── Failure (test_plot.R:34:2): plot weights ────────────────────────────────────
      `p <- plot_weights(test_mofa2, view = 1, factors = 1:2)` produced warnings.
      ── Failure (test_plot.R:36:2): plot weights ────────────────────────────────────
      `p <- plot_weights(test_mofa2, factors = 1)` produced warnings.
      ── Failure (test_plot.R:48:2): plot factor values ──────────────────────────────
      `p <- plot_factor(test_mofa2)` produced warnings.
      ── Failure (test_plot.R:52:2): plot factor values ──────────────────────────────
      `p <- plot_factors(test_mofa2, factors = 1:2)` produced warnings.
      
      [ FAIL 6 | WARN 4 | SKIP 0 | PASS 20 ]
      Error: Test failures
      Execution halted
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking whether package ‘MOFA2’ can be installed ... NOTE
    ```
    Found the following notes/warnings:
      Non-staged installation was used
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/MOFA2/new/MOFA2.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc       3.0Mb
        extdata   1.7Mb
    ```

*   checking top-level files ... NOTE
    ```
    File
      LICENSE
    is not mentioned in the DESCRIPTION file.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      definition for ‘colData’
    create_mofa_from_SingleCellExperiment: no visible global function
      definition for ‘rowData’
    plot_data_overview: no visible binding for global variable ‘view’
    plot_data_overview: no visible binding for global variable ‘ptotal’
    plot_data_overview: no visible binding for global variable ‘ntotal’
    plot_data_overview: no visible binding for global variable ‘group’
    plot_dimred: no visible binding for global variable ‘.’
    plot_enrichment_detailed: no visible binding for global variable
      ‘pathway’
    plot_enrichment_detailed: no visible binding for global variable
      ‘feature.statistic’
    plot_top_weights: no visible binding for global variable ‘value’
    plot_weights: no visible binding for global variable ‘value’
    plot_weights: no visible binding for global variable ‘.’
    summarise_factors: no visible binding for global variable ‘value’
    summarise_factors: no visible binding for global variable ‘level’
    summarise_factors: no visible binding for global variable ‘group’
    Undefined global functions or variables:
      . colData feature.statistic group level ntotal pathway ptotal rowData
      value view
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘PCGSE’
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' call not declared from: ‘data.table’
    ```

# nanny

<details>

* Version: 0.1.8
* GitHub: NA
* Source code: https://github.com/cran/nanny
* Date/Publication: 2020-06-13 13:50:03 UTC
* Number of recursive dependencies: 166

Run `revdep_details(, "nanny")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘lifecycle’ ‘lme4’ ‘methods’
      All declared Imports should be used.
    ```

# NetworkChange

<details>

* Version: 0.7
* GitHub: NA
* Source code: https://github.com/cran/NetworkChange
* Date/Publication: 2020-07-11 22:00:14 UTC
* Number of recursive dependencies: 124

Run `revdep_details(, "NetworkChange")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sna’
      All declared Imports should be used.
    ```

# nzelect

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/nzelect
* Date/Publication: 2017-10-02 20:35:23 UTC
* Number of recursive dependencies: 75

Run `revdep_details(, "nzelect")` for more info

</details>

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

# ORFik

<details>

* Version: 1.10.13
* GitHub: https://github.com/Roleren/ORFik
* Source code: https://github.com/cran/ORFik
* Date/Publication: 2021-03-26
* Number of recursive dependencies: 149

Run `revdep_details(, "ORFik")` for more info

</details>

## In both

*   checking whether package ‘ORFik’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘IRanges’ was built under R version 4.0.3
      Warning: package ‘BiocGenerics’ was built under R version 4.0.5
      Warning: package ‘S4Vectors’ was built under R version 4.0.3
      Warning: package ‘GenomicRanges’ was built under R version 4.0.3
      Warning: package ‘GenomeInfoDb’ was built under R version 4.0.5
      Warning: package ‘GenomicAlignments’ was built under R version 4.0.3
      Warning: package ‘SummarizedExperiment’ was built under R version 4.0.3
      Warning: package ‘MatrixGenerics’ was built under R version 4.0.3
      Warning: package ‘Biobase’ was built under R version 4.0.3
      Warning: package ‘Biostrings’ was built under R version 4.0.3
      Warning: package ‘XVector’ was built under R version 4.0.3
      Warning: package ‘Rsamtools’ was built under R version 4.0.3
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/ORFik/new/ORFik.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        doc   3.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘GenomicFeatures:::.merge_seqinfo_and_infer_missing_seqlengths’
      ‘IRanges:::regroupBySupergroup’ ‘S4Vectors:::normarg_mcols’
      ‘biomartr:::getENSEMBL.Seq’ ‘biomartr:::getENSEMBL.gtf’
      See the note in ?`:::` about the use of this operator.
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘findFa’ ‘find_url_ebi’ ‘hasHits’ ‘trimming.table’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      . CDS CDSGrouping Hx IR LEADERS LFC_TE ORFGrouping ORFScores RRS RSS
      Regulation Run StartCodons StopCodons TE_log2 TOP chr cigar1 cigar2
      codonSums count countRFP count_seq_pos_with_count counts_per_sample
      detectCores df.rfp df.rna dif difPer disengagementScores distORFCDS
      entropyRFP exon_rank feature forward fpkmRFP fpkmRNA fraction
      fraction.x fraction.y fractionLengths fraction_min fractions frame
      frame_one_RP frame_two_RP gene_id gene_sum grnames inFrameCDS ioScore
      isOverlappingCds kozak leaders mRNA mean_IR mean_per_gene
      median_per_gene median_score name ones pShifted
      perc_of_counts_per_sample percent percentage percentage_mrna_aligned
      pick position random rankInTx ranks ratio_cds_leader ratio_cds_mrna
      read length rfp rfp_log2 rna rna_log10 rna_log2 rowMin rowSums2
      sample_id sample_total scalingFactor sd_per_gene seq1 seq2 seq3 seq4
      seq5 size spots start1 start2 startCodonCoverage startRegionRelative
      subtitle sum.count sum_per_gene te trailers tx tx_len
      upstream_kozak_strength utr3_len utr5_len value variable widths
      windowMean windowSD zscore
    Consider adding
      importFrom("base", "length")
      importFrom("graphics", "frame")
    to your NAMESPACE file.
    ```

# PAFway

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/PAFway
* Date/Publication: 2020-02-05 16:40:02 UTC
* Number of recursive dependencies: 69

Run `revdep_details(, "PAFway")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘network’ ‘scales’ ‘sna’
      All declared Imports should be used.
    ```

# Pi

<details>

* Version: 2.2.1
* GitHub: https://github.com/hfang-bristol/Pi
* Source code: https://github.com/cran/Pi
* Date/Publication: 2020-11-24
* Number of recursive dependencies: 228

Run `revdep_details(, "Pi")` for more info

</details>

## In both

*   checking whether package ‘Pi’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘supraHex’ was built under R version 4.0.5
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/Pi/new/Pi.Rcheck/00install.out’ for details.
    ```

# plotly

<details>

* Version: 4.9.4.1
* GitHub: https://github.com/ropensci/plotly
* Source code: https://github.com/cran/plotly
* Date/Publication: 2021-06-18 09:00:02 UTC
* Number of recursive dependencies: 154

Run `revdep_details(, "plotly")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        htmlwidgets   3.8Mb
    ```

# PopGenReport

<details>

* Version: 3.0.4
* GitHub: https://github.com/green-striped-gecko/PopGenReport
* Source code: https://github.com/cran/PopGenReport
* Date/Publication: 2019-02-04 12:13:23 UTC
* Number of recursive dependencies: 120

Run `revdep_details(, "PopGenReport")` for more info

</details>

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ecodist’
    ```

# pubh

<details>

* Version: 1.1.20
* GitHub: https://github.com/josie-athens/pubh
* Source code: https://github.com/cran/pubh
* Date/Publication: 2021-02-16 17:30:06 UTC
* Number of recursive dependencies: 208

Run `revdep_details(, "pubh")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Hmisc’
      All declared Imports should be used.
    ```

# randomForestExplainer

<details>

* Version: 0.10.1
* GitHub: https://github.com/ModelOriented/randomForestExplainer
* Source code: https://github.com/cran/randomForestExplainer
* Date/Publication: 2020-07-11 20:30:02 UTC
* Number of recursive dependencies: 84

Run `revdep_details(, "randomForestExplainer")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    ...
    337   97 Petal.Length             0
    338   97  Petal.Width             2
    339   97 Sepal.Length             1
    340   97  Sepal.Width             2
    341   98 Petal.Length             1
    342   98  Petal.Width             0
    343   98 Sepal.Length             2
    344   98  Sepal.Width             4
    345   99 Petal.Length             0
    346   99  Petal.Width             3
    347   99 Sepal.Length             1
    348  100 Petal.Length             0
    349  100  Petal.Width             1
    350  100 Sepal.Length             2
    351  100  Sepal.Width             3
    > min_depth_distribution(ranger::ranger(Species ~ ., data = iris, num.trees = 100))
    Warning in (function (n)  : internal error -3 in R_decompress1
    Error in (function (n)  : 
      lazy-load database '/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/library.noindex/randomForestExplainer/Matrix/R/Matrix.rdb' is corrupt
    Calls: min_depth_distribution ... <Anonymous> -> .updateMethodsInTable -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test_ranger.R:1:1): (code run outside of `test_that()`) ──────────────
      Error: package or namespace load failed for 'ranger' in (function (n) :
       lazy-load database '/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/library.noindex/randomForestExplainer/Matrix/R/Matrix.rdb' is corrupt
      Backtrace:
          █
       1. └─base::library(ranger) test_ranger.R:1:0
       2.   └─base::tryCatch(...)
       3.     └─base:::tryCatchList(expr, classes, parentenv, handlers)
       4.       └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       5.         └─value[[3L]](cond)
      
      [ FAIL 1 | WARN 139 | SKIP 0 | PASS 31 ]
      Error: Test failures
      Execution halted
    ```

# robCompositions

<details>

* Version: 2.3.0
* GitHub: NA
* Source code: https://github.com/cran/robCompositions
* Date/Publication: 2020-11-18 21:10:02 UTC
* Number of recursive dependencies: 147

Run `revdep_details(, "robCompositions")` for more info

</details>

## Newly fixed

*   checking S3 generic/method consistency ... WARNING
    ```
    Error in get(fname, envir = envir, inherits = FALSE) : 
      lazy-load database '/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/library.noindex/robCompositions/rrcov/R/rrcov.rdb' is corrupt
    Calls: <Anonymous> -> unique -> .get_S3_generics_as_seen_from_package
    Execution halted
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘mvoutlier’, ‘StatDA’
    ```

# robustbase

<details>

* Version: 0.93-8
* GitHub: NA
* Source code: https://github.com/cran/robustbase
* Date/Publication: 2021-06-02 10:20:02 UTC
* Number of recursive dependencies: 66

Run `revdep_details(, "robustbase")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘robustX’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘robustX’
    ```

# rosetta

<details>

* Version: 0.3.6
* GitHub: NA
* Source code: https://github.com/cran/rosetta
* Date/Publication: 2021-03-29 10:00:03 UTC
* Number of recursive dependencies: 159

Run `revdep_details(, "rosetta")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘diptest’ ‘methods’ ‘viridis’
      All declared Imports should be used.
    ```

# rrr

<details>

* Version: 1.0.0
* GitHub: https://github.com/chrisaddy/rrr
* Source code: https://github.com/cran/rrr
* Date/Publication: 2016-12-09 15:15:55
* Number of recursive dependencies: 87

Run `revdep_details(, "rrr")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# scPipe

<details>

* Version: 1.12.0
* GitHub: https://github.com/LuyiTian/scPipe
* Source code: https://github.com/cran/scPipe
* Date/Publication: 2020-10-27
* Number of recursive dependencies: 148

Run `revdep_details(, "scPipe")` for more info

</details>

## In both

*   checking whether package ‘scPipe’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘SingleCellExperiment’ was built under R version 4.0.3
      Warning: package ‘SummarizedExperiment’ was built under R version 4.0.3
      Warning: package ‘MatrixGenerics’ was built under R version 4.0.3
      Warning: package ‘GenomicRanges’ was built under R version 4.0.3
      Warning: package ‘BiocGenerics’ was built under R version 4.0.5
      Warning: package ‘S4Vectors’ was built under R version 4.0.3
      Warning: package ‘IRanges’ was built under R version 4.0.3
      Warning: package ‘GenomeInfoDb’ was built under R version 4.0.5
      Warning: package ‘Biobase’ was built under R version 4.0.3
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/scPipe/new/scPipe.Rcheck/00install.out’ for details.
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scater’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    anno_to_saf: no visible binding for global variable ‘GeneID’
    convert_geneid: no visible global function definition for ‘useMart’
    get_genes_by_GO: no visible global function definition for ‘useMart’
    infer_gene_id_from_parent: no visible binding for global variable
      ‘type’
    plot_demultiplex: no visible binding for global variable ‘status’
    plot_demultiplex: no visible binding for global variable ‘count’
    plot_demultiplex: no visible binding for global variable ‘label_y’
    plot_demultiplex: no visible binding for global variable ‘label_tx’
    Undefined global functions or variables:
      GeneID count label_tx label_y status type useMart
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# seer

<details>

* Version: 1.1.6
* GitHub: NA
* Source code: https://github.com/cran/seer
* Date/Publication: 2021-06-01 04:50:02 UTC
* Number of recursive dependencies: 110

Run `revdep_details(, "seer")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘MASS’
      All declared Imports should be used.
    ```

# SeqSQC

<details>

* Version: 1.12.0
* GitHub: https://github.com/Liubuntu/SeqSQC
* Source code: https://github.com/cran/SeqSQC
* Date/Publication: 2020-10-27
* Number of recursive dependencies: 140

Run `revdep_details(, "SeqSQC")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    ...
    Method: exacting biallelic SNPs
    Number of samples: 5
    Parsing "/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/SeqSQC/new/SeqSQC.Rcheck/SeqSQC/extdata/example_sub.vcf" ...
    	import 1000 variants.
    + genotype   { Bit2 5x1000, 1.2K } *
    SNP genotypes: 5 samples, 1000 SNPs
    Genotype matrix is being transposed ...
    Optimize the access efficiency ...
    Clean up the fragments of GDS file:
        open the file '/var/folders/0k/bxg5lhr92sq74mb1d446ql540000gp/T//RtmppI5gy5/fileaa154757dbb4' (12.4K)
        # of fragments: 48
        save to '/var/folders/0k/bxg5lhr92sq74mb1d446ql540000gp/T//RtmppI5gy5/fileaa154757dbb4.tmp'
        rename '/var/folders/0k/bxg5lhr92sq74mb1d446ql540000gp/T//RtmppI5gy5/fileaa154757dbb4.tmp' (10.7K, reduced: 1.6K)
        # of fragments: 20
    Load study cohort annotation file ...
    Load 1kg data to temp directory...
    Error: Corrupt Cache: sqlite file
      See vignette section on corrupt cache
      cache: ~/Library/Caches/ExperimentHub
      filename: experimenthub.sqlite3
    Execution halted
    ```

*   checking whether package ‘SeqSQC’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘ExperimentHub’ was built under R version 4.0.5
      Warning: package ‘BiocGenerics’ was built under R version 4.0.5
      Warning: package ‘AnnotationHub’ was built under R version 4.0.5
      Warning: package ‘BiocFileCache’ was built under R version 4.0.3
      Warning: package ‘SNPRelate’ was built under R version 4.0.3
      Warning: package ‘gdsfmt’ was built under R version 4.0.3
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/SeqSQC/new/SeqSQC.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        doc       1.7Mb
        extdata   3.3Mb
    ```

# SmartEDA

<details>

* Version: 0.3.8
* GitHub: https://github.com/daya6489/SmartEDA
* Source code: https://github.com/cran/SmartEDA
* Date/Publication: 2021-06-05 10:10:02 UTC
* Number of recursive dependencies: 94

Run `revdep_details(, "SmartEDA")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘qpdf’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘InformationValue’
    ```

# specmine

<details>

* Version: 3.1.5
* GitHub: https://github.com/BioSystemsUM/specmine
* Source code: https://github.com/cran/specmine
* Date/Publication: 2021-05-16 14:10:02 UTC
* Number of recursive dependencies: 286

Run `revdep_details(, "specmine")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rcytoscapejs’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘e1071’ ‘specmine.datasets’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘hyperSpec’
    ```

# spinifex

<details>

* Version: 0.2.8
* GitHub: https://github.com/nspyrison/spinifex
* Source code: https://github.com/cran/spinifex
* Date/Publication: 2021-04-18 14:50:02 UTC
* Number of recursive dependencies: 138

Run `revdep_details(, "spinifex")` for more info

</details>

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error: argument 'fps' must be a factor of 100
      Backtrace:
          █
       1. └─spinifex::play_tour_path(...) test-3_visualize.r:18:0
       2.   └─spinifex:::render_type(frames = tour_df, ...)
       3.     ├─base::do.call(anim_func, args = gganimate_args)
       4.     └─(function (...) ...
       5.       ├─gganimate::animate(...)
       6.       └─gganimate:::animate.gganim(...)
       7.         └─args$renderer(frames_vars$frame_source, args$fps)
       8.           └─magick::image_animate(anim, fps, loop = if (loop) 0 else 1)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 50 ]
      Error: Test failures
      Execution halted
    ```

# spup

<details>

* Version: 1.3-2
* GitHub: NA
* Source code: https://github.com/cran/spup
* Date/Publication: 2020-04-30 22:20:06 UTC
* Number of recursive dependencies: 83

Run `revdep_details(, "spup")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sp’
      All declared Imports should be used.
    ```

# statVisual

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/statVisual
* Date/Publication: 2020-02-20 19:30:02 UTC
* Number of recursive dependencies: 172

Run `revdep_details(, "statVisual")` for more info

</details>

## In both

*   checking whether package ‘statVisual’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘Biobase’ was built under R version 4.0.3
      Warning: package ‘BiocGenerics’ was built under R version 4.0.5
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/statVisual/new/statVisual.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gbm’ ‘ggfortify’ ‘tibble’ ‘tidyverse’
      All declared Imports should be used.
    ```

# tidybulk

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/tidybulk
* Date/Publication: 2021-04-06
* Number of recursive dependencies: 271

Run `revdep_details(, "tidybulk")` for more info

</details>

## In both

*   checking for missing documentation entries ... WARNING
    ```
    Warning: package ‘DESeq2’ was built under R version 4.0.3
    Warning: package ‘S4Vectors’ was built under R version 4.0.3
    Warning: package ‘BiocGenerics’ was built under R version 4.0.5
    Warning: package ‘IRanges’ was built under R version 4.0.3
    Warning: package ‘GenomicRanges’ was built under R version 4.0.3
    Warning: package ‘GenomeInfoDb’ was built under R version 4.0.5
    Warning: package ‘SummarizedExperiment’ was built under R version 4.0.3
    Warning: package ‘MatrixGenerics’ was built under R version 4.0.3
    Warning: package ‘Biobase’ was built under R version 4.0.3
    All user-level objects in a package should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        data   6.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      variable ‘.’
    tidybulk,RangedSummarizedExperiment: no visible binding for global
      variable ‘feature’
    tidybulk,SummarizedExperiment: no visible binding for global variable
      ‘.’
    tidybulk,SummarizedExperiment: no visible binding for global variable
      ‘feature’
    Undefined global functions or variables:
      (Intercept) . .abundance_scaled .abundant .cell_type .proportion
      .transcript EPIC GeneID Status X_cibersort Y buildIdx
      cell_type_proportions cluster cluster kmeans cmdscale.out correlation
      counts cov_data ct_data data_base egsea ensembl_id entrez feature
      geneID genes gs_cat item1 m med med.rank min_proportion multiplier
      my_n n_aggr name nf pathway plogis qlogis rc read count ref_genome
      rotated dimensions sample 1 sample 2 sample a sample b sample_idx
      samples sdev seurat_clusters surv_test temp test tot tot_filt
      transcript tt_columns value variable x
    Consider adding
      importFrom("base", "sample")
      importFrom("stats", "kmeans", "plogis", "qlogis")
    to your NAMESPACE file.
    ```

*   checking Rd files ... NOTE
    ```
    prepare_Rd: remove_redundancy-methods.Rd:136-138: Dropping empty section \details
    ```

# tidySingleCellExperiment

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/tidySingleCellExperiment
* Date/Publication: 2020-10-27
* Number of recursive dependencies: 204

Run `revdep_details(, "tidySingleCellExperiment")` for more info

</details>

## In both

*   checking whether package ‘tidySingleCellExperiment’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘SingleCellExperiment’ was built under R version 4.0.3
      Warning: package ‘SummarizedExperiment’ was built under R version 4.0.3
      Warning: package ‘MatrixGenerics’ was built under R version 4.0.3
      Warning: package ‘GenomicRanges’ was built under R version 4.0.3
      Warning: package ‘BiocGenerics’ was built under R version 4.0.5
      Warning: package ‘S4Vectors’ was built under R version 4.0.3
      Warning: package ‘IRanges’ was built under R version 4.0.3
      Warning: package ‘GenomeInfoDb’ was built under R version 4.0.5
      Warning: package ‘Biobase’ was built under R version 4.0.3
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/tidySingleCellExperiment/new/tidySingleCellExperiment.Rcheck/00install.out’ for details.
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    bind_cols_: no visible global function definition for ‘colData<-’
    extract.tidySingleCellExperiment: no visible global function definition
      for ‘colData<-’
    get_abundance_sc_long: no visible global function definition for
      ‘assays’
    get_abundance_sc_wide: no visible global function definition for
      ‘assays’
    join_transcripts.tidySingleCellExperiment: no visible binding for
      global variable ‘cell’
    join_transcripts.tidySingleCellExperiment: no visible binding for
      global variable ‘transcript’
    mutate.tidySingleCellExperiment: no visible global function definition
      for ‘colData<-’
    rename.tidySingleCellExperiment: no visible global function definition
      for ‘colData<-’
    separate.tidySingleCellExperiment: no visible global function
      definition for ‘colData<-’
    unite.tidySingleCellExperiment: no visible global function definition
      for ‘colData<-’
    Undefined global functions or variables:
      assays cell colData<- transcript
    ```

# TNBC.CMS

<details>

* Version: 1.6.0
* GitHub: NA
* Source code: https://github.com/cran/TNBC.CMS
* Date/Publication: 2020-10-27
* Number of recursive dependencies: 150

Run `revdep_details(, "TNBC.CMS")` for more info

</details>

## In both

*   checking whether package ‘TNBC.CMS’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘SummarizedExperiment’ was built under R version 4.0.3
      Warning: package ‘MatrixGenerics’ was built under R version 4.0.3
      Warning: package ‘GenomicRanges’ was built under R version 4.0.3
      Warning: package ‘BiocGenerics’ was built under R version 4.0.5
      Warning: package ‘S4Vectors’ was built under R version 4.0.3
      Warning: package ‘IRanges’ was built under R version 4.0.3
      Warning: package ‘GenomeInfoDb’ was built under R version 4.0.5
      Warning: package ‘Biobase’ was built under R version 4.0.3
    See ‘/Users/barret/odrive/AmazonCloudDrive/git/R/ggobi_org/ggally/ggally.nosync/revdep/checks.noindex/TNBC.CMS/new/TNBC.CMS.Rcheck/00install.out’ for details.
    ```

# TVTB

<details>

* Version: 1.16.0
* GitHub: https://github.com/kevinrue/TVTB
* Source code: https://github.com/cran/TVTB
* Date/Publication: 2020-10-27
* Number of recursive dependencies: 171

Run `revdep_details(, "TVTB")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        R     2.0Mb
        doc   2.7Mb
    ```

# ubiquity

<details>

* Version: 1.0.5
* GitHub: https://github.com/john-harrold/ubiquity
* Source code: https://github.com/cran/ubiquity
* Date/Publication: 2021-04-25 18:50:03 UTC
* Number of recursive dependencies: 114

Run `revdep_details(, "ubiquity")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.9Mb
      sub-directories of 1Mb or more:
        doc     6.6Mb
        ubinc   2.2Mb
    ```

# ufs

<details>

* Version: 0.4.3
* GitHub: NA
* Source code: https://github.com/cran/ufs
* Date/Publication: 2021-02-02 00:10:02 UTC
* Number of recursive dependencies: 149

Run `revdep_details(, "ufs")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘ufs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggSave
    > ### Title: Save a ggplot with specific defaults
    > ### Aliases: ggSave
    > 
    > ### ** Examples
    > 
    > plot <- ufs::ggBoxplot(mtcars, 'mpg');
    > ggSave(file=tempfile(fileext=".png"), plot=plot);
    Error in png_dev(..., res = dpi, units = "in") : 
      unused argument (type = "cairo-png")
    Calls: ggSave -> <Anonymous> -> dev
    Execution halted
    ```

# vidger

<details>

* Version: 1.10.0
* GitHub: https://github.com/btmonier/vidger
* Source code: https://github.com/cran/vidger
* Date/Publication: 2020-10-27
* Number of recursive dependencies: 124

Run `revdep_details(, "vidger")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.5Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
        doc    6.1Mb
    ```

# vivid

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/vivid
* Date/Publication: 2021-04-09 09:10:02 UTC
* Number of recursive dependencies: 205

Run `revdep_details(, "vivid")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘colorspace’ ‘tidyr’
      All declared Imports should be used.
    ```

