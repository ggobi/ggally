## Test environments
* local OS X install (x86_64-apple-darwin13.4.0), R 3.2.3
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)
* local OS X install

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of GGally
(https://github.com/ggobi/ggally/blob/master/revdep/summary.md).  

* DescribeDisplay (0.2.4) - ERROR
  * Maintainer: Di Cook <dicook@iastate.edu>
  * Maintainer has been in contact with me.  Will post update

* freqparcoord (1.0.0) - ERROR
  * Maintainer: Norm Matloff <normmatloff@gmail.com>
  * ggplot2 error. GGally doesn't ask for 'mgcv'

* ggmcmc (0.7.2) - ERROR
  * Maintainer: Xavier Fernández i Marín <xavier.fim@gmail.com>  
  * GGally error. Have worked with Xavier, and he is waiting until GGally and ggplot2 are updated to submit his updated package

* userfriendlyscience (0.3-0) - ERROR
  * Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
  * ggplot2 error. ggplot2::geom_dotplot doesn't know what size is.

* vdmR (0.2.0) - ERROR
  * Maintainer: Tomokazu Fujino <fujino@fwu.ac.jp>
  * Non GGally error.  rgeos is not a part of the GGally package

* Can not install
  * LANDD (1.0.0) - Maintainer: Shangzhao Qiu <qsz1328@gmail.com>
  * MissingDataGUI (0.2-2) - Maintainer: Xiaoyue Cheng <xycheng@iastate.edu>
  * robCompositions (1.9.1) - Maintainer: Matthias Templ <templ@tuwien.ac.at>
  * specmine (1.0) - Maintainer: Christopher Costa <chrisbcl@hotmail.com>

* No error (Works!)
  * ParamHelpers (1.6) - Maintainer: Bernd Bischl <bernd_bischl@gmx.net>  
  * PopGenReport (2.2) - Maintainer: Bernd Gruber <bernd.gruber@canberra.edu.au>
  * qualvar (0.1.0) - Maintainer: Joel Gombin <joel.gombin@gmail.com>
  * robustbase (0.92-5) - Maintainer: Martin Maechler <maechler@stat.math.ethz.ch>
  * svdvis (0.1) - Maintainer: Neo Christopher Chung <nchchung@gmail.com>
