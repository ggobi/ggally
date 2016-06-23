
## Comments

I know my vignettes are getting larger.  Any idea on how to reduce the pdf size, even though they are visualization vignettes?  Otherwise, if it's not a big deal, I'll keep them as is.

All three environments have the same NOTE on package size.  Linux has a NOTE on mis-spelled words, it is a false positive.

All downstream authors were not contacted as the update is minor.  Only in contact with authors who requested minor update.

Thank you for your time,
Barret


## Test environments and R CMD check results

* local OS X install (x86_64-apple-darwin13.4.0), R 3.3.0
  * There were no ERRORs or WARNINGs.
  * There is one NOTE.
    * checking installed package size ... NOTE
      installed size is 10.9Mb
      sub-directories of 1Mb or more:
        doc   9.7Mb
* ubuntu 12.04 (on travis-ci, x86_64-pc-linux-gnu), R 3.3.0
  * There were no ERRORs or WARNINGs.  
  * There is one NOTE
    * checking installed package size ... NOTE
      installed size is 10.2Mb
      sub-directories of 1Mb or more:
        data   1.9Mb
        doc    7.9Mb
* win-builder (devel and release)
  * There were no ERRORs or WARNINGs.  
  * There are two NOTEs.
    * checking CRAN incoming feasibility ... NOTE
      Maintainer: 'Barret Schloerke <schloerke@gmail.com>'

      Possibly mis-spelled words in DESCRIPTION:
        geoms (25:43)
        ggplot (5:21)
        scatterplot (26:49)

    * checking installed package size ... NOTE
      installed size is 10.9Mb
      sub-directories of 1Mb or more:
        doc   9.7Mb


## Reverse dependencies
I have run R CMD check on downstream dependencies of GGally on my local machine.
* Summary - https://github.com/ggobi/ggally/blob/master/revdep/README.md

### Quick RevDep Table
Checked DescribeDisplay    : 0 errors | 0 warnings | 0 notes
Checked eechidna           : 0 errors | 0 warnings | 0 notes
Checked freqparcoord       : 0 errors | 0 warnings | 0 notes
Checked ggbio              : 1 error  | 2 warnings | 6 notes
Checked ggmcmc             : 1 error  | 0 warnings | 0 notes
Checked imageData          : 0 errors | 0 warnings | 0 notes
Checked isomiRs            : 1 error  | 0 warnings | 0 notes
Checked LANDD              : 0 errors | 0 warnings | 0 notes
Checked MissingDataGUI     : 1 error  | 0 warnings | 0 notes
Checked ParamHelpers       : 1 error  | 0 warnings | 0 notes
Checked 10/22. Elapsed 00:15. Remaining ~00:18
Checked plotly             : 0 errors | 0 warnings | 0 notes
Checked PopGenReport       : 0 errors | 0 warnings | 0 notes
Checked qualvar            : 0 errors | 0 warnings | 0 notes
Checked robCompositions    : 0 errors | 0 warnings | 0 notes
Checked robustbase         : 0 errors | 0 warnings | 0 notes
Checked rwty               : 1 error  | 0 warnings | 0 notes
Checked specmine           : 1 error  | 0 warnings | 0 notes
Checked svdvis             : 0 errors | 0 warnings | 0 notes
Checked TCGAbiolinks       : 1 error  | 0 warnings | 0 notes
Checked toaster            : 0 errors | 0 warnings | 0 notes
Checked 20/22. Elapsed 00:30. Remaining ~00:03
Checked userfriendlyscience: 1 error  | 0 warnings | 0 notes
Checked vdmR               : 0 errors | 0 warnings | 0 notes
Saving check results to `revdep/check.rds` -------------------------------------
Cleaning up --------------------------------------------------------------------
* Failed to install dependencies for: isomiRs, rwty
* Failed to install: MissingDataGUI, specmine, TCGAbiolinks, userfriendlyscience
* ggbio: checking examples ... ERROR
* ggmcmc: checking examples ... ERROR
* ParamHelpers: checking tests ... ERROR


### RevDep Notes

* Can install, but non-GGally error
  * ggmcmc (1.0) - Maintainer: Xavier Fernández i Marín <xavier.fim@gmail.com>  
    * Failed with example(ggs_caterpillar). Issue not related to GGally
  * ggbio (1.20.1) - Maintainer: Michael Lawrence <lawrence.michael@gene.com>  
    * Can install, but there are all sorts of issues in the check that are not related to GGally

* Can not install
  * isomiRs (1.0.2) - Maintainer: Lorena Pantano <lorena.pantano@gmail.com>
    * Packages required but not available: ‘DiscriMiner’ ‘DESeq2’
  * MissingDataGUI (0.2-2) - Maintainer: Xiaoyue Cheng <xycheng@iastate.edu>
    * difficulty installing RGtk2. Can't find any solutions for El Capitan / my computer
  * ParamHelpers (1.7) - Maintainer: Bernd Bischl <bernd_bischl@gmx.net>  
    * failed unit tests. Not related to GGally
  * rwty (1.0.1) - Maintainer: Dan Warren <dan.l.warren@gmail.com>
    * Packages required but not available: ‘phangorn’
  * specmine (1.0) - Maintainer: Christopher Costa <chrisbcl@hotmail.com>
    * Trouble installing some BioConductor deps
  * TCGAbiolinks (1.2.4) - Maintainer: Antonio Colaprico <antonio.colaprico@ulb.ac.be>,
   Tiago Chedraoui Silva <tiagochst@usp.br>  
    * Trouble installing some BioConductor deps
  * userfriendlyscience (0.4-1) - Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
    * Can't figure out why it will not install

* No error (Works!)
  * DescribeDisplay (0.2.5) - Maintainer: Di Cook <dicook@monash.edu>  
  * eechidna (0.1) - Maintainer: Ben Marwick <benmarwick@gmail.com>
  * freqparcoord (1.0.1) - Maintainer: Norm Matloff <normmatloff@gmail.com>
  * imageData (0.1-21) - Maintainer: Chris Brien <Chris.Brien@unisa.edu.au>
  * LANDD (1.0.0) - Maintainer: Shangzhao Qiu <qsz1328@gmail.com>
  * plotly (3.6.0) - Maintainer: Carson Sievert <cpsievert1@gmail.com>  
  * PopGenReport (2.2.2) - Maintainer: Bernd Gruber <bernd.gruber@canberra.edu.au>
  * qualvar (0.1.0) - Maintainer: Joel Gombin <joel.gombin@gmail.com>
  * robCompositions (2.0.0) - Maintainer: Matthias Templ <templ@tuwien.ac.at>
  * robustbase (0.92-6) - Maintainer: Martin Maechler <maechler@stat.math.ethz.ch>
  * svdvis (0.1) - Maintainer: Neo Christopher Chung <nchchung@gmail.com>
  * toaster (0.5.1) - Maintainer: Gregory Kanevsky <gregory.kanevsky@teradata.com>  
  * vdmR (0.2.2) - Maintainer: Tomokazu Fujino <fujino@fwu.ac.jp>
