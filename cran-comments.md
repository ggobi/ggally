## Test environments
* local OS X install (x86_64-apple-darwin13.4.0), R 3.2.3
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)
* local OS X install

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
I have run R CMD check on downstream dependencies of GGally on my local machine.
* Summary - https://github.com/ggobi/ggally/blob/master/revdep/summary.md
* Check result folders - https://github.com/ggobi/ggally/blob/master/revdep/


* GGally update related errors
  * ggmcmc (0.7.2) - ERROR
    * Maintainer: Xavier Fernández i Marín <xavier.fim@gmail.com>  
    * Submitted working PR to ggmcmc
    * !! Xavier is waiting until GGally and ggplot2 are updated to submit his updated package !!

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
    * difficulty installing RGtk2
  * specmine (1.0) - Maintainer: Christopher Costa <chrisbcl@hotmail.com>
    * Packages required but not available: ‘xcms’ ‘MAIT’ ‘genefilter’ ‘impute’

* No error (Works!)
  * ParamHelpers (1.6) - Maintainer: Bernd Bischl <bernd_bischl@gmx.net>  
  * PopGenReport (2.2) - Maintainer: Bernd Gruber <bernd.gruber@canberra.edu.au>
  * qualvar (0.1.0) - Maintainer: Joel Gombin <joel.gombin@gmail.com>
  * robCompositions (1.9.1) - Maintainer: Matthias Templ <templ@tuwien.ac.at>
  * robustbase (0.92-5) - Maintainer: Martin Maechler <maechler@stat.math.ethz.ch>
  * svdvis (0.1) - Maintainer: Neo Christopher Chung <nchchung@gmail.com>
  * toaster (0.4.1) - Maintainer: Gregory Kanevsky <gregory.kanevsky@teradata.com>  
  * vdmR (0.2.0) - Maintainer: Tomokazu Fujino <fujino@fwu.ac.jp>
