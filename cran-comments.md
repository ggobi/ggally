
## Comments

### 2016-11-04
Linux has a NOTE on mis-spelled words, it is a false positive.

All downstream authors were emailed on 2016-10-17. Have only been in contact with the authors of 'plotly'.

Thank you for your time.

Best,
Barret


#### Test environments and R CMD check results

* local OS X install (x86_64-apple-darwin13.4.0), R 3.3.0
  * There were no ERRORs or WARNINGs.
  * There are two NOTEs.
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Barret Schloerke <schloerke@gmail.com>’

    Size of tarball: 5972829 bytes
    * checking installed package size ... NOTE
      installed size is 10.9Mb
      sub-directories of 1Mb or more:
        doc   9.7Mb
* travis-ci
  * ubuntu 12.04 (on travis-ci, x86_64-pc-linux-gnu), R 3.1.3
  * ubuntu 12.04 (on travis-ci, x86_64-pc-linux-gnu), R 3.3.0
  * ubuntu 12.04 (on travis-ci, x86_64-pc-linux-gnu), R Under development (unstable) (2016-06-23 r70826)
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

### RevDep Notes

* Failed to install dependencies for: MissingDataGUI, specmine, toaster, userfriendlyscience

* In contact with author and have resolved issues.
  * plotly: checking examples ... ERROR

* Does not appear to be a GGally issue.
  * ParamHelpers: checking tests ... ERROR
  * SHELF: checking re-building of vignette outputs ... WARNING

* Does not appear to be a GGally issue. Seems like ggplot2 issue
  * robustbase: checking re-building of vignette outputs ... WARNING
  * vdmR: checking examples ... ERROR
