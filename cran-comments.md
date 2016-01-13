## Test environments and R CMD check results

I did update from knitr::knit to installing rmarkdown and using knitr::rmarkdown as per request of knitr.  This has increased my vignette file sizes.


* local OS X install (x86_64-apple-darwin13.4.0), R 3.2.3
  * There were no ERRORs or WARNINGs.
  * There is one NOTE.
    * checking installed package size ... NOTE
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        doc   6.8Mb

* ubuntu 12.04 (on travis-ci, x86_64-pc-linux-gnu), R 3.2.3
  * There were no ERRORs, WARNINGs.  
  * There are two NOTEs. The libcurl NOTE is a false positive.
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Barret Schloerke <schloerke@gmail.com>’
    Checking URLs requires 'libcurl' support in the R build

    * checking installed package size ... NOTE
    installed size is  7.5Mb
    sub-directories of 1Mb or more:
      doc   6.7Mb


* win-builder (devel and release)
  * There were no ERRORs, WARNINGs.  
  * There are two NOTEs.
    * checking installed package size ... NOTE
    installed size is  7.6Mb
    sub-directories of 1Mb or more:
      doc   6.8Mb

    * checking CRAN incoming feasibility ... NOTE
    Possibly mis-spelled words in DESCRIPTION:
      geoms (24:40)
      ggplot (5:21, 23:45)
      scatterplot (25:49)

  * a couple examples take more than five seconds. But it is not user time.
    ** running examples for arch 'i386' ... [43s] OK
    Examples with CPU or elapsed time > 5s
                 user system elapsed
    ggnetworkmap 3.79   0.36    5.89
    ggcorr       1.24   0.08    6.10
    ** running examples for arch 'x64' ... [41s] OK
    Examples with CPU or elapsed time > 5s
                 user system elapsed
    ggnetworkmap  3.7   0.41    5.77


## Downstream dependencies
I have run R CMD check on downstream dependencies of GGally on my local machine.
* Summary - https://github.com/ggobi/ggally/blob/master/revdep/summary.md
* Check result folders - https://github.com/ggobi/ggally/blob/master/revdep/

All downstream authors were not contacted as the update is minor.  Only in contact with authors who requested minor update.


* Non-GGally error
  * freqparcoord (1.0.0) - Maintainer: Norm Matloff <normmatloff@gmail.com>
    * missing package 'mgcv' is not related to the GGally package
  * userfriendlyscience (0.3-0) - Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
    * ggplot2 error. ggplot2::geom_dotplot doesn't know what size is.

* Can not install
  * DescribeDisplay (0.2.4) - Maintainer: Di Cook <dicook@iastate.edu>
    * ggplot2 error on install
  * LANDD (1.0.0) - Maintainer: Shangzhao Qiu <qsz1328@gmail.com>
    * Packages required but not available: ‘GOstats’ ‘GOSemSim’
  * MissingDataGUI (0.2-2) - Maintainer: Xiaoyue Cheng <xycheng@iastate.edu>
    * difficulty installing RGtk2. Can't find any solutions for El Capitan / my computer
  * specmine (1.0) - Maintainer: Christopher Costa <chrisbcl@hotmail.com>
    * Packages required but not available: ‘xcms’ ‘MAIT’ ‘genefilter’ ‘impute’

* No error (Works!)
  * ggmcmc (0.7.3) - Maintainer: Xavier Fernández i Marín <xavier.fim@gmail.com>  
  * ParamHelpers (1.6) - Maintainer: Bernd Bischl <bernd_bischl@gmx.net>  
  * PopGenReport (2.2) - Maintainer: Bernd Gruber <bernd.gruber@canberra.edu.au>
  * qualvar (0.1.0) - Maintainer: Joel Gombin <joel.gombin@gmail.com>
  * robCompositions (1.9.1) - Maintainer: Matthias Templ <templ@tuwien.ac.at>
  * robustbase (0.92-5) - Maintainer: Martin Maechler <maechler@stat.math.ethz.ch>
  * svdvis (0.1) - Maintainer: Neo Christopher Chung <nchchung@gmail.com>
  * toaster (0.4.1) - Maintainer: Gregory Kanevsky <gregory.kanevsky@teradata.com>  
  * vdmR (0.2.0) - Maintainer: Tomokazu Fujino <fujino@fwu.ac.jp>
