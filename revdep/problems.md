# adventr (0.1.8)

* Email: <mailto:andyf@sussex.ac.uk>
* GitHub mirror: <https://github.com/cran/adventr>

Run `revdepcheck::revdep_details(, "adventr")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespaces in Imports field not imported from:
       ‘BayesFactor’ ‘GGally’ ‘Hmisc’ ‘WRS2’ ‘boot’ ‘car’ ‘dplyr’ ‘effects’
       ‘effsize’ ‘forcats’ ‘ggplot2’ ‘lm.beta’ ‘nlme’ ‘readr’ ‘robust’
       ‘sandwich’ ‘sjstats’ ‘tidyr’
       All declared Imports should be used.
     ```

# airGR (1.7.9)

* Email: <mailto:airGR@inrae.fr>
* GitHub mirror: <https://github.com/cran/airGR>

Run `revdepcheck::revdep_details(, "airGR")` for more info

## In both

*   checking whether package ‘airGR’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/airGR/new/airGR.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘airGR’ ...
** this is package ‘airGR’ version ‘1.7.9’
** package ‘airGR’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 21.0.0 (clang-2100.4.1.1)’
using Fortran compiler: ‘GNU Fortran (Homebrew GCC 15.1.0) 15.1.0’
using SDK: ‘MacOSX27.2.sdk’
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/usr/local/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/include -c airGR.c -o airGR.o
/opt/homebrew/bin/gfortran  -fPIC  -Wall -g -O2  -c  frun_CEMANEIGE.f90 -o frun_CEMANEIGE.o
clang: error: invalid version number in '-mmacosx-version-min=18.0'
make: *** [frun_CEMANEIGE.o] Error 1
ERROR: compilation failed for package ‘airGR’
* removing ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/airGR/new/airGR.Rcheck/airGR’


```
### CRAN

```
* installing *source* package ‘airGR’ ...
** this is package ‘airGR’ version ‘1.7.9’
** package ‘airGR’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 21.0.0 (clang-2100.4.1.1)’
using Fortran compiler: ‘GNU Fortran (Homebrew GCC 15.1.0) 15.1.0’
using SDK: ‘MacOSX27.2.sdk’
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/usr/local/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/include -c airGR.c -o airGR.o
/opt/homebrew/bin/gfortran  -fPIC  -Wall -g -O2  -c  frun_CEMANEIGE.f90 -o frun_CEMANEIGE.o
clang: error: invalid version number in '-mmacosx-version-min=18.0'
make: *** [frun_CEMANEIGE.o] Error 1
ERROR: compilation failed for package ‘airGR’
* removing ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/airGR/old/airGR.Rcheck/airGR’


```
# BAS (2.0.2)

* GitHub: <https://github.com/merliseclyde/BAS>
* Email: <mailto:clyde@duke.edu>
* GitHub mirror: <https://github.com/cran/BAS>

Run `revdepcheck::revdep_details(, "BAS")` for more info

## In both

*   checking whether package ‘BAS’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/BAS/new/BAS.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘BAS’ ...
** this is package ‘BAS’ version ‘2.0.2’
** package ‘BAS’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 21.0.0 (clang-2100.4.1.1)’
using Fortran compiler: ‘GNU Fortran (Homebrew GCC 15.1.0) 15.1.0’
using SDK: ‘MacOSX27.2.sdk’
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/usr/local/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/include -c E_ZS.c -o E_ZS.o
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/usr/local/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/include -c MH-funs.c -o MH-funs.o
...
betapriorfamily.c:239:38: warning: unused variable 'u' [-Wunused-variable]
  239 |   double a, b, s, r, v, theta, n, p, u, shrinkage;
      |                                      ^
1 warning generated.
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/usr/local/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/include -c cch.c -o cch.o
/opt/homebrew/bin/gfortran  -fPIC  -Wall -g -O2  -c dch1up.f -o dch1up.o
clang: error: invalid version number in '-mmacosx-version-min=18.0'
make: *** [dch1up.o] Error 1
ERROR: compilation failed for package ‘BAS’
* removing ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/BAS/new/BAS.Rcheck/BAS’


```
### CRAN

```
* installing *source* package ‘BAS’ ...
** this is package ‘BAS’ version ‘2.0.2’
** package ‘BAS’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 21.0.0 (clang-2100.4.1.1)’
using Fortran compiler: ‘GNU Fortran (Homebrew GCC 15.1.0) 15.1.0’
using SDK: ‘MacOSX27.2.sdk’
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/usr/local/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/include -c E_ZS.c -o E_ZS.o
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/usr/local/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/include -c MH-funs.c -o MH-funs.o
...
betapriorfamily.c:239:38: warning: unused variable 'u' [-Wunused-variable]
  239 |   double a, b, s, r, v, theta, n, p, u, shrinkage;
      |                                      ^
1 warning generated.
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/usr/local/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2  -I/opt/homebrew/include -c cch.c -o cch.o
/opt/homebrew/bin/gfortran  -fPIC  -Wall -g -O2  -c dch1up.f -o dch1up.o
clang: error: invalid version number in '-mmacosx-version-min=18.0'
make: *** [dch1up.o] Error 1
ERROR: compilation failed for package ‘BAS’
* removing ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/BAS/old/BAS.Rcheck/BAS’


```
# CINNA (1.2.2)

* Email: <mailto:ashtiani.minoo@gmail.com>
* GitHub mirror: <https://github.com/cran/CINNA>

Run `revdepcheck::revdep_details(, "CINNA")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘circlize’
       All declared Imports should be used.
     ```

# CluMSID (1.28.0)

* GitHub: <https://github.com/tdepke/CluMSID>
* Email: <mailto:depke@mailbox.org>

Run `revdepcheck::revdep_details(, "CluMSID")` for more info

## In both

*   checking running R code from vignettes ...
     ```
       ‘CluMSID_DI-MSMS.Rmd’ using ‘UTF-8’... OK
       ‘CluMSID_GC-EI-MS.Rmd’ using ‘UTF-8’... failed
       ‘CluMSID_MTBLS.Rmd’ using ‘UTF-8’... OK
       ‘CluMSID_lowres-LC-MSMS.Rmd’ using ‘UTF-8’... OK
       ‘CluMSID_tutorial.Rmd’ using ‘UTF-8’... OK
      ERROR
     Errors in running code in vignettes:
     when running code in ‘CluMSID_GC-EI-MS.Rmd’
       ...
     
     > xA <- metaMS::runCAMERA(xcms::xcmsSet(pool), chrom = "GC", 
     +     settings = metaMS::metaSetting(TSQXLS.GC, "CAMERA"))
     Warning in parallel::mccollect(wait = FALSE, timeout = 1) :
       1 parallel job did not deliver a result
     
       When sourcing ‘CluMSID_GC-EI-MS.R’:
     Error: wrong args for environment subassignment
     Execution halted
     shell-init: error retrieving current directory: getcwd: cannot access parent directories: No such file or directory
     ```

*   checking whether package ‘CluMSID’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘dbscan::as.dendrogram’ by ‘stats::as.dendrogram’ when loading ‘CluMSID’
     See ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/CluMSID/new/CluMSID.Rcheck/00install.out’ for details.
     ```

*   checking DESCRIPTION meta-information ... NOTE
     ```
     License stub is invalid DCF.
     ```

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) OPTICSplot.Rd:24: Lost braces
         24 | see code{\link[dbscan:optics]{extractDBSCAN}}.
            |         ^
     ```

# clustrd (1.4.0)

* Email: <mailto:amarkos@gmail.com>
* GitHub mirror: <https://github.com/cran/clustrd>

Run `revdepcheck::revdep_details(, "clustrd")` for more info

## In both

*   checking Rd files ... NOTE
     ```
     ...
            |                                                                                                                                                                                                                                            ^
     checkRd: (-1) local_bootclus.Rd:39: Lost braces; missing escapes or markup?
         39 | \emph{Step 2. Mapping}: Assign each observation x_i to the closest centers of C^{S_i} and C^{T_i} using Euclidean distance, resulting in partitions C^{XS_i} and C^{XT_i}.
            |                                                                                 ^
     checkRd: (-1) local_bootclus.Rd:39: Lost braces; missing escapes or markup?
         39 | \emph{Step 2. Mapping}: Assign each observation x_i to the closest centers of C^{S_i} and C^{T_i} using Euclidean distance, resulting in partitions C^{XS_i} and C^{XT_i}.
            |                                                                                             ^
     checkRd: (-1) local_bootclus.Rd:39: Lost braces; missing escapes or markup?
         39 | \emph{Step 2. Mapping}: Assign each observation x_i to the closest centers of C^{S_i} and C^{T_i} using Euclidean distance, resulting in partitions C^{XS_i} and C^{XT_i}.
            |                                                                                                                                                       ^
     checkRd: (-1) local_bootclus.Rd:39: Lost braces; missing escapes or markup?
         39 | \emph{Step 2. Mapping}: Assign each observation x_i to the closest centers of C^{S_i} and C^{T_i} using Euclidean distance, resulting in partitions C^{XS_i} and C^{XT_i}.
            |                                                                                                                                                                    ^
     checkRd: (-1) local_bootclus.Rd:41: Lost braces; missing escapes or markup?
         41 | \emph{Step 3. Evaluation}: Obtain the maximum Jaccard agreement between each original cluster C_k and each one of the two bootstrap clusters, C_^k'{XS_i} and C_^k'{XT_i} as measure of agreement and stability, and take the average of each pair.
            |                                                                                                                                                    ^
     checkRd: (-1) local_bootclus.Rd:41: Lost braces; missing escapes or markup?
         41 | \emph{Step 3. Evaluation}: Obtain the maximum Jaccard agreement between each original cluster C_k and each one of the two bootstrap clusters, C_^k'{XS_i} and C_^k'{XT_i} as measure of agreement and stability, and take the average of each pair.
            |                                                                                                                                                                    ^
     checkRd: (-1) local_bootclus.Rd:54: Lost braces; missing escapes or markup?
         54 | \item{clust1}{Partitions, C^{XS_i} of the original data, X, predicted from clustering bootstrap sample S_i (see Details)}
            |                             ^
     checkRd: (-1) local_bootclus.Rd:55: Lost braces; missing escapes or markup?
         55 | \item{clust2}{Partitions, C^{XT_i} of the original data, X, predicted from clustering bootstrap sample T_i (see Details)}
            |                             ^
     ```

# conserveR (1.0.4)

* GitHub: <https://github.com/azizka/conserveR>
* Email: <mailto:alexander.zizka@idiv.de>
* GitHub mirror: <https://github.com/cran/conserveR>

Run `revdepcheck::revdep_details(, "conserveR")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespaces in Imports field not imported from:
       ‘scales’ ‘sna’
       All declared Imports should be used.
     ```

# corrViz (0.1.0)

* Email: <mailto:alan.n.inglis@gmail.com>
* GitHub mirror: <https://github.com/cran/corrViz>

Run `revdepcheck::revdep_details(, "corrViz")` for more info

## In both

*   checking DESCRIPTION meta-information ... NOTE
     ```
       Missing dependency on R >= 4.1.0 because package code uses the pipe
       |> or function shorthand \(...) syntax added in R 4.1.0.
       File(s) using such syntax:
         ‘corrBarplot.R’ ‘corrNetwork.R’
     ```

# covsim (1.2.0)

* Email: <mailto:njal.foldnes@gmail.com>
* GitHub mirror: <https://github.com/cran/covsim>

Run `revdepcheck::revdep_details(, "covsim")` for more info

## In both

*   checking running R code from vignettes ...
     ```
       ‘calibrate-simulate.Rmd’ using ‘UTF-8’... OK
       ‘simulation-study.Rmd’ using ‘UTF-8’... OK
       ‘vignette.Rmd’ using ‘UTF-8’... failed
      ERROR
     Errors in running code in vignettes:
     when running code in ‘vignette.Rmd’
       ...
     
     > margins <- list(list(distr = "norm"), list(distr = "chisq", 
     +     df = 1), list(distr = "t", df = 5))
     
     > pcs <- list(list(bicop_dist("clayton"), bicop_dist("joe")), 
     +     list(bicop_dist("frank")))
     
       When sourcing ‘vignette.R’:
     Error: could not find function "bicop_dist"
     Execution halted
     ```

# cubble (1.0.0)

* GitHub: <https://github.com/huizezhang-sherry/cubble>
* Email: <mailto:huizezhangsh@gmail.com>
* GitHub mirror: <https://github.com/cran/cubble>

Run `revdepcheck::revdep_details(, "cubble")` for more info

## In both

*   checking DESCRIPTION meta-information ... NOTE
     ```
       Missing dependency on R >= 4.1.0 because package code uses the pipe
       |> or function shorthand \(...) syntax added in R 4.1.0.
       File(s) using such syntax:
         ‘accessors.Rd’ ‘as-cubble.R’ ‘as_cubble.Rd’ ‘check-key.R’
         ‘climate-data.Rd’ ‘covid.Rd’ ‘cubble-attrs.R’ ‘cubble-class.R’
         ‘cubble-print.R’ ‘dplyr.R’ ‘dplyr.Rd’ ‘face-spatial-temporal.R’
         ‘face.Rd’ ‘glyph.R’ ‘glyph.Rd’ ‘make_spatial_sf.Rd’
         ‘make_temporal_tsibble.Rd’ ‘matching.R’ ‘matching.Rd’ ‘netcdf.R’
         ‘sf.R’ ‘test-data.Rd’ ‘tsibble.R’ ‘tsibble.Rd’ ‘unfold.R’ ‘unfold.Rd’
     ```

# fic (1.0.1)

* GitHub: <https://github.com/chjackson/fic>
* Email: <mailto:chris.jackson@mrc-bsu.cam.ac.uk>
* GitHub mirror: <https://github.com/cran/fic>

Run `revdepcheck::revdep_details(, "fic")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespaces in Imports field not imported from:
       ‘mgcv’ ‘numDeriv’
       All declared Imports should be used.
     ```

# fingerPro (2.1)

* GitHub: <https://github.com/eead-csic-eesa/fingerPro>
* Email: <mailto:borja.latorre@csic.es>
* GitHub mirror: <https://github.com/cran/fingerPro>

Run `revdepcheck::revdep_details(, "fingerPro")` for more info

## In both

*   checking whether package ‘fingerPro’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/fingerPro/new/fingerPro.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘fingerPro’ ...
** this is package ‘fingerPro’ version ‘2.1’
** package ‘fingerPro’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.4.1.1)’
using SDK: ‘MacOSX27.2.sdk’
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/fingerPro/Rcpp/include' -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/fingerPro/RcppGSL/include' -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/fingerPro/RcppProgress/include' -I/opt/R/arm64/include -I/usr/local/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
In file included from RcppExports.cpp:4:
In file included from /Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/fingerPro/RcppGSL/include/RcppGSL.h:25:
/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/fingerPro/RcppGSL/include/RcppGSLForward.h:25:10: fatal error: 'gsl/gsl_vector.h' file not found
   25 | #include <gsl/gsl_vector.h> 
      |          ^~~~~~~~~~~~~~~~~~
1 error generated.
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘fingerPro’
* removing ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/fingerPro/new/fingerPro.Rcheck/fingerPro’


```
### CRAN

```
* installing *source* package ‘fingerPro’ ...
** this is package ‘fingerPro’ version ‘2.1’
** package ‘fingerPro’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.4.1.1)’
using SDK: ‘MacOSX27.2.sdk’
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/fingerPro/Rcpp/include' -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/fingerPro/RcppGSL/include' -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/fingerPro/RcppProgress/include' -I/opt/R/arm64/include -I/usr/local/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
In file included from RcppExports.cpp:4:
In file included from /Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/fingerPro/RcppGSL/include/RcppGSL.h:25:
/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/fingerPro/RcppGSL/include/RcppGSLForward.h:25:10: fatal error: 'gsl/gsl_vector.h' file not found
   25 | #include <gsl/gsl_vector.h> 
      |          ^~~~~~~~~~~~~~~~~~
1 error generated.
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘fingerPro’
* removing ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/fingerPro/old/fingerPro.Rcheck/fingerPro’


```
# GENESIS (2.42.0)

* GitHub: <https://github.com/UW-GAC/GENESIS>
* Email: <mailto:sdmorris@uw.edu>

Run `revdepcheck::revdep_details(, "GENESIS")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     Unexported object imported by a ':::' call: ‘survey:::saddle’
       See the note in ?`:::` about the use of this operator.
     ```

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) sample_annotation_1KG.Rd:14: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) sample_annotation_1KG.Rd:15: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) sample_annotation_1KG.Rd:16: Lost braces in \itemize; meant \describe ?
     ```

# GeomxTools (3.16.0)

* Email: <mailto:mgriswold@nanostring.com>

Run `revdepcheck::revdep_details(, "GeomxTools")` for more info

## In both

*   checking DESCRIPTION meta-information ... NOTE
     ```
     License components which are templates and need '+ file LICENSE':
       MIT
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
       different formal arguments
     qgrubbs: no visible global function definition for ‘qtable’
     readNanoStringGeoMxSet: no visible global function definition for
       ‘read.table’
     readPKCFile: no visible binding for global variable ‘RTS_ID’
     readPKCFile: no visible binding for global variable ‘Module’
     setGrubbsFlags: no visible binding for global variable ‘TargetName’
     setGrubbsFlags : <anonymous>: no visible binding for global variable
       ‘Module’
     setProbeRatioFlags: no visible binding for global variable ‘TargetName’
     setTargetFlags: no visible global function definition for ‘analtye’
     subtractBackground : <anonymous>: no visible binding for global
       variable ‘Module’
     setSignatureFuncs<-,NanoStringGeoMxSet-character: no visible global
       function definition for ‘setSigFuncs<-’
     signatureScores,NanoStringGeoMxSet: no visible global function
       definition for ‘assayDataElement2’
     signatureScores,NanoStringGeoMxSet: no visible global function
       definition for ‘.sigCalc’
     Undefined global functions or variables:
       .sigCalc Module RTS_ID TargetName analtye assayDataElement2 qtable
       read.table setSigFuncs<-
     Consider adding
       importFrom("utils", "read.table")
     to your NAMESPACE file.
     ```

# ggmcmc (1.5.1.2)

* GitHub: <https://github.com/xfim/ggmcmc>
* Email: <mailto:xavier.fim@gmail.com>
* GitHub mirror: <https://github.com/cran/ggmcmc>

Run `revdepcheck::revdep_details(, "ggmcmc")` for more info

## In both

*   checking running R code from vignettes ...
     ```
       ‘using_ggmcmc.Rmd’ using ‘UTF-8’... OK
       ‘v70i09.Rnw’ using ‘UTF-8’... failed
      ERROR
     Errors in running code in vignettes:
     when running code in ‘v70i09.Rnw’
       ...
     
     > knit_hooks$set(par = function(before, options, envir) {
     +     if (before && options$fig.show != "none") {
     +         par(mar = c(4, 4, 0.1, 0.1), cex .... [TRUNCATED] 
     
     > knit_theme$set("print")
     
       When sourcing 'v70i09.R':
     Error: argument is of length zero
     Execution halted
     ```

# GGPA (1.24.0)

* GitHub: <https://github.com/dongjunchung/GGPA>
* Email: <mailto:dongjun.chung@gmail.com>

Run `revdepcheck::revdep_details(, "GGPA")` for more info

## In both

*   checking whether package ‘GGPA’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       3_Param.cpp:74:12: warning: use of bitwise '&' with boolean operands [-Wbitwise-instead-of-logical]
     See ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/GGPA/new/GGPA.Rcheck/00install.out’ for details.
     ```

*   checking dependencies in R code ... NOTE
     ```
     Packages in Depends field not imported from:
       ‘network’ ‘scales’ ‘sna’
       These packages need to be imported from (in the NAMESPACE file)
       for when this namespace is loaded but not attached.
     ```

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) GGPA-class.Rd:37: Lost braces; missing escapes or markup?
         37 |     Nodes i and j are connected if the posterior probability of E_{ij} > \code{pCutoff} and the posterior probability of beta_{ij} > \code{betaCI}. }
            |                                                                   ^
     checkRd: (-1) GGPA-class.Rd:37: Lost braces; missing escapes or markup?
         37 |     Nodes i and j are connected if the posterior probability of E_{ij} > \code{pCutoff} and the posterior probability of beta_{ij} > \code{betaCI}. }
            |                                                                                                                               ^
     ```

*   checking compiled code ... NOTE
     ```
     File ‘GGPA/libs/GGPA.so’:
       Found ‘__ZNSt3__14coutE’, possibly from ‘std::cout’ (C++)
         Object: ‘3_Param.o’
     
     Compiled code should not call entry points which might terminate R nor
     write to stdout/stderr instead of to the console, nor use Fortran I/O
     nor system RNGs nor [v]sprintf.
     
     See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
     ```

# ggpcp (0.2.0)

* GitHub: <https://github.com/heike/ggpcp>
* Email: <mailto:hofmann@iastate.edu>
* GitHub mirror: <https://github.com/cran/ggpcp>

Run `revdepcheck::revdep_details(, "ggpcp")` for more info

## In both

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) pcp_scale.Rd:50: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) pcp_scale.Rd:51: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) pcp_scale.Rd:52: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) pcp_scale.Rd:53: Lost braces in \itemize; meant \describe ?
     checkRd: (-1) pcp_scale.Rd:54-55: Lost braces in \itemize; meant \describe ?
     ```

# ggquickeda (0.3.3)

* GitHub: <https://github.com/smouksassi/ggquickeda>
* Email: <mailto:samermouksassi@gmail.com>
* GitHub mirror: <https://github.com/cran/ggquickeda>

Run `revdepcheck::revdep_details(, "ggquickeda")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespaces in Imports field not imported from:
       ‘DT’ ‘Formula’ ‘GGally’ ‘Hmisc’ ‘RPostgres’ ‘colourpicker’ ‘ggpmisc’
       ‘ggpubr’ ‘glue’ ‘gridExtra’ ‘markdown’ ‘patchwork’ ‘plotly’
       ‘quantreg’ ‘shinyFiles’ ‘shinyjqui’ ‘shinyjs’ ‘table1’ ‘zoo’
       All declared Imports should be used.
     ```

# ggtreeSpace (1.8.0)

* GitHub: <https://github.com/YuLab-SMU/ggtreeSpace>
* Email: <mailto:guangchuangyu@gmail.com>

Run `revdepcheck::revdep_details(, "ggtreeSpace")` for more info

## In both

*   checking S3 generic/method consistency ... WARNING
     ```
     ggplot_add:
       function(object, plot, ...)
     ggplot_add.tsheatmap:
       function(object, plot, object_name)
     
     ggplot_add:
       function(object, plot, ...)
     ggplot_add.treespace:
       function(object, plot, object_name)
     See section ‘Generic functions and methods’ in the ‘Writing R
     Extensions’ manual.
     ```

*   checking for code/documentation mismatches ... WARNING
     ```
     ...
         data
       Mismatches in argument names:
         Position: 2 Code: trait Docs: data
     
     Codoc mismatches from Rd file 'ggtreespace.Rd':
     ggtreespace
       Code: function(tr, trait, mapping = NULL, ...)
       Docs: function(tr, data, mapping = NULL, ...)
       Argument names in code not in docs:
         trait
       Argument names in docs not in code:
         data
       Mismatches in argument names:
         Position: 2 Code: trait Docs: data
     
     Codoc mismatches from Rd file 'make_ts_data.Rd':
     make_ts_data
       Code: function(tr, trait)
       Docs: function(tr, data)
       Argument names in code not in docs:
         trait
       Argument names in docs not in code:
         data
       Mismatches in argument names:
         Position: 2 Code: trait Docs: data
     ```

*   checking R code for possible problems ... NOTE
     ```
     intern_call: no visible binding for global variable ‘dat’
     Undefined global functions or variables:
       dat
     ```

# InflectSSP (1.6)

* Email: <mailto:almosley@iu.edu>
* GitHub mirror: <https://github.com/cran/InflectSSP>

Run `revdepcheck::revdep_details(, "InflectSSP")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘svglite’
       All declared Imports should be used.
     ```

# innsight (0.3.2)

* GitHub: <https://github.com/bips-hb/innsight>
* Email: <mailto:niklas.koenen@gmail.com>
* GitHub mirror: <https://github.com/cran/innsight>

Run `revdepcheck::revdep_details(, "innsight")` for more info

## In both

*   checking running R code from vignettes ...
     ```
     ...
     Execution halted
     when running code in ‘detailed_overview.Rmd’
       ...
     
     > library(torch)
     
     > torch_model <- nn_sequential(nn_conv2d(3, 5, c(2, 
     +     2), stride = 2, padding = 3), nn_relu(), nn_avg_pool2d(c(2, 
     +     2)), nn_flatten(), nn_li .... [TRUNCATED] 
     
       When sourcing ‘detailed_overview.R’:
     Error: Lantern is not loaded. Please use `install_torch()` to install additional dependencies.
     Execution halted
     when running code in ‘innsight.Rmd’
       ...
     
     > library(torch)
     
     > library(innsight)
     
     > torch_manual_seed(123)
     
       When sourcing ‘innsight.R’:
     Error: Lantern is not loaded. Please use `install_torch()` to install additional dependencies.
     Execution halted
     ```

*   checking Rd cross-references ... NOTE
     ```
     Unknown package ‘fastshap’ in Rd xrefs
     ```

# isomiRs (1.40.0)

* GitHub: <https://github.com/lpantano/isomiRs>
* Email: <mailto:lorena.pantano@gmail.com>

Run `revdepcheck::revdep_details(, "isomiRs")` for more info

## In both

*   checking for hidden files and directories ... NOTE
     ```
     Found the following hidden files and directories:
       .travis.yml
     These were most likely included in error. See section ‘Package
     structure’ in the ‘Writing R Extensions’ manual.
     ```

*   checking DESCRIPTION meta-information ... NOTE
     ```
     License stub is invalid DCF.
     ```

*   checking R code for possible problems ... NOTE
     ```
     ...
     .clean_noise: no visible binding for global variable ‘hits’
     .plot_all_iso: no visible binding for global variable ‘pct_abundance’
     .plot_all_iso: no visible binding for global variable ‘iso_sample’
     .remove_gt_n_changes: no visible binding for global variable ‘changes’
     isoAnnotate: no visible binding for global variable ‘pct’
     isoCounts: no visible global function definition for ‘as.tibble’
     isoPlot: no visible global function definition for ‘across’
     isoPlot: no visible global function definition for ‘everything’
     isoPlot: no visible binding for global variable ‘iso_sample’
     isoPlot: no visible binding for global variable ‘.data’
     isoPlotPosition: no visible global function definition for ‘across’
     isoPlotPosition: no visible global function definition for ‘everything’
     isoPlotPosition: no visible binding for global variable ‘iso_sample’
     isoPlotPosition: no visible binding for global variable ‘.data’
     isoSelect.IsomirDataSeq: no visible global function definition for
       ‘across’
     isoSelect.IsomirDataSeq: no visible global function definition for
       ‘everything’
     isoSelect,IsomirDataSeq: no visible global function definition for
       ‘across’
     isoSelect,IsomirDataSeq: no visible global function definition for
       ‘everything’
     Undefined global functions or variables:
       .data across as.tibble changes everything hits iso_sample pct
       pct_abundance total
     ```

# jarbes (2.5.0)

* Email: <mailto:pabloemilio.verde@hhu.de>
* GitHub mirror: <https://github.com/cran/jarbes>

Run `revdepcheck::revdep_details(, "jarbes")` for more info

## In both

*   checking whether package ‘jarbes’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/jarbes/new/jarbes.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘jarbes’ ...
** this is package ‘jarbes’ version ‘2.5.0’
** package ‘jarbes’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: NULL
...
	The rjags package requires JAGS 4.x.y to be installed.

	The error returned from library.dynam(...) was:
	unable to load shared object '/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/jarbes/rjags/libs/rjags.so':
  dlopen(/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/jarbes/rjags/libs/rjags.so, 0x000A): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: <3DAC8692-DA01-3CFF-80FC-EA79C4DB9E0A> /Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/jarbes/rjags/libs/rjags.so
  Reason: tried: '/usr/local/lib/libjags.4.dylib' (no such file), '/System/Volumes/Preboot/Cryptexes/OS/usr/local/lib/libjags.4.dylib' (no such file), '/usr/local/lib/libjags.4.dylib' (no such file), '/Library/Frameworks/R.framework/Resources/lib/libjags.4.dylib' 
Execution halted
ERROR: lazy loading failed for package ‘jarbes’
* removing ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/jarbes/new/jarbes.Rcheck/jarbes’


```
### CRAN

```
* installing *source* package ‘jarbes’ ...
** this is package ‘jarbes’ version ‘2.5.0’
** package ‘jarbes’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: NULL
...
	The rjags package requires JAGS 4.x.y to be installed.

	The error returned from library.dynam(...) was:
	unable to load shared object '/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/jarbes/rjags/libs/rjags.so':
  dlopen(/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/jarbes/rjags/libs/rjags.so, 0x000A): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: <3DAC8692-DA01-3CFF-80FC-EA79C4DB9E0A> /Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/jarbes/rjags/libs/rjags.so
  Reason: tried: '/usr/local/lib/libjags.4.dylib' (no such file), '/System/Volumes/Preboot/Cryptexes/OS/usr/local/lib/libjags.4.dylib' (no such file), '/usr/local/lib/libjags.4.dylib' (no such file), '/Library/Frameworks/R.framework/Resources/lib/libjags.4.dylib' 
Execution halted
ERROR: lazy loading failed for package ‘jarbes’
* removing ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/jarbes/old/jarbes.Rcheck/jarbes’


```
# jmv (2.8.0)

* GitHub: <https://github.com/jamovi/jmv>
* Email: <mailto:jon@thon.cc>
* GitHub mirror: <https://github.com/cran/jmv>

Run `revdepcheck::revdep_details(, "jmv")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespaces in Imports field not imported from:
       ‘BayesFactor’ ‘GGally’ ‘GPArotation’ ‘Hmisc’ ‘MASS’ ‘R6’ ‘ROCR’
       ‘afex’ ‘car’ ‘dplyr’ ‘ggrepel’ ‘ggridges’ ‘lavaan’ ‘matrixStats’
       ‘multcomp’ ‘mvnormtest’ ‘nnet’ ‘psych’ ‘vcd’ ‘vcdExtra’
       All declared Imports should be used.
     ```

# jsmodule (2.0.1)

* GitHub: <https://github.com/jinseob2kim/jsmodule>
* Email: <mailto:jinseob2kim@gmail.com>
* GitHub mirror: <https://github.com/cran/jsmodule>

Run `revdepcheck::revdep_details(, "jsmodule")` for more info

## In both

*   checking running R code from vignettes ...
     ```
     ...
     Errors in running code in vignettes:
     when running code in ‘ai-assistant-guide-ko.Rmd’
       ...
     ☐ Edit '/Users/barret/.Renviron'.
     ☐ Restart R for changes to take effect.
     
     > library(jsmodule)
     
     > jsBasicGadget()
     
       When sourcing ‘ai-assistant-guide-ko.R’:
     Error: argument "data" is missing, with no default
     Execution halted
     when running code in ‘ai-assistant-guide.Rmd’
       ...
     ☐ Edit '/Users/barret/.Renviron'.
     ☐ Restart R for changes to take effect.
     
     > library(jsmodule)
     
     > jsBasicGadget()
     
       When sourcing ‘ai-assistant-guide.R’:
     Error: argument "data" is missing, with no default
     Execution halted
     ```

# loon.ggplot (1.3.5)

* GitHub: <https://github.com/great-northern-diver/loon.ggplot>
* Email: <mailto:z267xu@gmail.com>
* GitHub mirror: <https://github.com/cran/loon.ggplot>

Run `revdepcheck::revdep_details(, "loon.ggplot")` for more info

## In both

*   checking whether package ‘loon.ggplot’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/loon.ggplot/new/loon.ggplot.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘loon.ggplot’ ...
** this is package ‘loon.ggplot’ version ‘1.3.5’
** package ‘loon.ggplot’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘loon’:
 .onLoad failed in loadNamespace() for 'loon', details:
  call: structure(.External(.C_dotTcl, ...), class = "tclObj")
  error: [tcl] couldn't connect to display "".
In addition: Warning message:
In fun(libname, pkgname) : couldn't connect to display ""
Execution halted
ERROR: lazy loading failed for package ‘loon.ggplot’
* removing ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/loon.ggplot/new/loon.ggplot.Rcheck/loon.ggplot’


```
### CRAN

```
* installing *source* package ‘loon.ggplot’ ...
** this is package ‘loon.ggplot’ version ‘1.3.5’
** package ‘loon.ggplot’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘loon’:
 .onLoad failed in loadNamespace() for 'loon', details:
  call: structure(.External(.C_dotTcl, ...), class = "tclObj")
  error: [tcl] couldn't connect to display "".
In addition: Warning message:
In fun(libname, pkgname) : couldn't connect to display ""
Execution halted
ERROR: lazy loading failed for package ‘loon.ggplot’
* removing ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/loon.ggplot/old/loon.ggplot.Rcheck/loon.ggplot’


```
# MAST (1.38.0)

* GitHub: <https://github.com/RGLab/MAST>
* Email: <mailto:andrew.n.mcdavid@gmail.com>

Run `revdepcheck::revdep_details(, "MAST")` for more info

## In both

*   checking running R code from vignettes ...
     ```
       ‘MAITAnalysis.Rmd’ using ‘UTF-8’... failed
       ‘MAST-Intro.Rmd’ using ‘UTF-8’... OK
       ‘MAST-interoperability.Rmd’ using ‘UTF-8’... OK
      ERROR
     Errors in running code in vignettes:
     when running code in ‘MAITAnalysis.Rmd’
       ...
     > mat_to_plot <- assay(sca[entrez_to_plot, ])
     
     > rownames(mat_to_plot) <- symbols_to_plot
     
     > aheatmap(mat_to_plot, annCol = colData(sca)[, "condition"], 
     +     main = "DE genes", col = rev(colorRampPalette(colors = brewer.pal(name = "PiYG",  .... [TRUNCATED] 
     
       When sourcing ‘MAITAnalysis.R’:
     Error: invalid graphics state
     Execution halted
     ```

*   checking for hidden files and directories ... NOTE
     ```
     Found the following hidden files and directories:
       .travis.yml
     These were most likely included in error. See section ‘Package
     structure’ in the ‘Writing R Extensions’ manual.
     ```

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) GSEATests-class.Rd:14: Lost braces; missing escapes or markup?
         14 | \item{\code{tests}}{array: gene sets X {discrete,continuous} X {stat, variance, degrees of freedom, avg correlation} X {test, null}}
            |                                        ^
     checkRd: (-1) GSEATests-class.Rd:14: Lost braces; missing escapes or markup?
         14 | \item{\code{tests}}{array: gene sets X {discrete,continuous} X {stat, variance, degrees of freedom, avg correlation} X {test, null}}
            |                                                                ^
     checkRd: (-1) GSEATests-class.Rd:14: Lost braces; missing escapes or markup?
         14 | \item{\code{tests}}{array: gene sets X {discrete,continuous} X {stat, variance, degrees of freedom, avg correlation} X {test, null}}
            |                                                                                                                        ^
     ```

# mitch (1.24.0)

* GitHub: <https://github.com/markziemann/mitch>
* Email: <mailto:mark.ziemann@gmail.com>

Run `revdepcheck::revdep_details(, "mitch")` for more info

## In both

*   checking running R code from vignettes ...
     ```
       ‘infiniumMethArrayWorkflow.Rmd’ using ‘UTF-8’... failed
       ‘mitchWorkflow.Rmd’ using ‘UTF-8’... OK
      ERROR
     Errors in running code in vignettes:
     when running code in ‘infiniumMethArrayWorkflow.Rmd’
       ...
     5    TBL1Y cg01707559
     6   TMSB4Y cg02004872
     
     > new.hgnc.table <- readRDS("../inst/extdata/new.hgnc.table.rds")
     Warning in gzfile(file, "rb") :
       cannot open compressed file '../inst/extdata/new.hgnc.table.rds', probable reason 'No such file or directory'
     
       When sourcing ‘infiniumMethArrayWorkflow.R’:
     Error: cannot open the connection
     Execution halted
     ```

# mlr3shiny (0.5.0)

* Email: <mailto:laurens.tetzlaff@outlook.de>
* GitHub mirror: <https://github.com/cran/mlr3shiny>

Run `revdepcheck::revdep_details(, "mlr3shiny")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespaces in Imports field not imported from:
       ‘DT’ ‘GGally’ ‘Metrics’ ‘data.table’ ‘dplyr’ ‘ggparty’ ‘mlr3’
       ‘mlr3learners’ ‘mlr3measures’ ‘mlr3pipelines’ ‘mlr3viz’ ‘patchwork’
       ‘plyr’ ‘purrr’ ‘shinyWidgets’ ‘shinyalert’ ‘shinydashboard’ ‘shinyjs’
       ‘stats’ ‘stringr’
       All declared Imports should be used.
     ```

# MOFA2 (1.22.1)

* GitHub: <https://github.com/bioFAM/MOFA2>
* Email: <mailto:ricard.argelaguet@gmail.com>

Run `revdepcheck::revdep_details(, "MOFA2")` for more info

## In both

*   R CMD check timed out


*   checking whether package ‘MOFA2’ can be installed ... NOTE
     ```
     Found the following notes/warnings:
       Non-staged installation was used
     See ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/MOFA2/new/MOFA2.Rcheck/00install.out’ for details.
     ```

*   checking R code for possible problems ... NOTE
     ```
     ...
     plot_interpolation_vs_covariate: no visible binding for global variable
       ‘group’
     plot_interpolation_vs_covariate: no visible binding for global variable
       ‘value.covariate’
     plot_interpolation_vs_covariate: no visible binding for global variable
       ‘value.factor’
     plot_interpolation_vs_covariate: no visible binding for global variable
       ‘ymin’
     plot_interpolation_vs_covariate: no visible binding for global variable
       ‘ymax’
     plot_sharedness: no visible binding for global variable ‘sharedness’
     plot_sharedness: no visible binding for global variable ‘value’
     plot_smoothness: no visible binding for global variable ‘value’
     plot_smoothness: no visible binding for global variable ‘smoothness’
     plot_top_weights: no visible binding for global variable ‘value’
     plot_weights: no visible binding for global variable ‘value’
     plot_weights: no visible binding for global variable ‘.’
     summarise_factors: no visible binding for global variable ‘value’
     summarise_factors: no visible binding for global variable ‘level’
     summarise_factors: no visible binding for global variable ‘group’
     Undefined global functions or variables:
       . CreateDimReducObject E2 L1 L2 colData covariate covariate_value
       feature.statistic group group_label level ntotal pathway ptotal
       sample_id sharedness smoothness value value.covariate value.factor
       value.unaligned value.warped value_scaled variance view ymax ymin
     ```

# morseDR (0.1.3)

* Email: <mailto:virgile.baudrot@qonfluens.com>
* GitHub mirror: <https://github.com/cran/morseDR>

Run `revdepcheck::revdep_details(, "morseDR")` for more info

## In both

*   checking whether package ‘morseDR’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/morseDR/new/morseDR.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘morseDR’ ...
** this is package ‘morseDR’ version ‘0.1.3’
** package ‘morseDR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
...
	The rjags package requires JAGS 4.x.y to be installed.

	The error returned from library.dynam(...) was:
	unable to load shared object '/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/morseDR/rjags/libs/rjags.so':
  dlopen(/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/morseDR/rjags/libs/rjags.so, 0x000A): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: <3DAC8692-DA01-3CFF-80FC-EA79C4DB9E0A> /Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/morseDR/rjags/libs/rjags.so
  Reason: tried: '/usr/local/lib/libjags.4.dylib' (no such file), '/System/Volumes/Preboot/Cryptexes/OS/usr/local/lib/libjags.4.dylib' (no such file), '/usr/local/lib/libjags.4.dylib' (no such file), '/Library/Frameworks/R.framework/Resources/lib/libjags.4.dyli
Execution halted
ERROR: lazy loading failed for package ‘morseDR’
* removing ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/morseDR/new/morseDR.Rcheck/morseDR’


```
### CRAN

```
* installing *source* package ‘morseDR’ ...
** this is package ‘morseDR’ version ‘0.1.3’
** package ‘morseDR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
...
	The rjags package requires JAGS 4.x.y to be installed.

	The error returned from library.dynam(...) was:
	unable to load shared object '/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/morseDR/rjags/libs/rjags.so':
  dlopen(/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/morseDR/rjags/libs/rjags.so, 0x000A): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: <3DAC8692-DA01-3CFF-80FC-EA79C4DB9E0A> /Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/morseDR/rjags/libs/rjags.so
  Reason: tried: '/usr/local/lib/libjags.4.dylib' (no such file), '/System/Volumes/Preboot/Cryptexes/OS/usr/local/lib/libjags.4.dylib' (no such file), '/usr/local/lib/libjags.4.dylib' (no such file), '/Library/Frameworks/R.framework/Resources/lib/libjags.4.dyli
Execution halted
ERROR: lazy loading failed for package ‘morseDR’
* removing ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/morseDR/old/morseDR.Rcheck/morseDR’


```
# mvMAPIT (2.0.4)

* GitHub: <https://github.com/lcrawlab/mvMAPIT>
* Email: <mailto:julian.d.stamp@gmail.com>
* GitHub mirror: <https://github.com/cran/mvMAPIT>

Run `revdepcheck::revdep_details(, "mvMAPIT")` for more info

## In both

*   checking running R code from vignettes ...
     ```
       ‘mvMAPIT.Rmd’ using ‘UTF-8’... OK
       ‘study-compare-p-value-combine-methods.Rmd’ using ‘UTF-8’... OK
       ‘study-phillips-bnabs.Rmd’ using ‘UTF-8’... failed
       ‘study-wtccc-mice.Rmd’ using ‘UTF-8’... OK
       ‘tutorial-docker-mvmapit.Rmd’ using ‘UTF-8’... OK
       ‘tutorial-lt-mapit.Rmd’ using ‘UTF-8’... OK
       ‘tutorial-simulations.Rmd’ using ‘UTF-8’... OK
      ERROR
     Errors in running code in vignettes:
     when running code in ‘study-phillips-bnabs.Rmd’
       ...
     Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
       font family 'Arial' not found in PostScript font database
     Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
       font family 'Arial' not found in PostScript font database
     Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
       font family 'Arial' not found in PostScript font database
     
       When sourcing ‘study-phillips-bnabs.R’:
     Error: invalid font type
     Execution halted
     ```

# PAGE (0.4.0)

* Email: <mailto:jessica306a@gmail.com>
* GitHub mirror: <https://github.com/cran/PAGE>

Run `revdepcheck::revdep_details(, "PAGE")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘RSQLite’
       All declared Imports should be used.
     ```

# PhosR (1.22.0)

* Email: <mailto:di.xiao@sydney.edu.au>

Run `revdepcheck::revdep_details(, "PhosR")` for more info

## In both

*   checking for hidden files and directories ... NOTE
     ```
     Found the following hidden files and directories:
       .travis.yml
     These were most likely included in error. See section ‘Package
     structure’ in the ‘Writing R Extensions’ manual.
     ```

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘stringi’
       All declared Imports should be used.
     Unexported objects imported by ':::' calls:
       ‘BiocGenerics:::replaceSlots’ ‘S4Vectors:::disableValidity’
       ‘SummarizedExperiment:::.SummarizedExperiment.charbound’
       See the note in ?`:::` about the use of this operator.
     ```

*   checking R code for possible problems ... NOTE
     ```
     abundPlot: no visible binding for global variable ‘.’
     Undefined global functions or variables:
       .
     ```

# protGear (1.16.0)

* GitHub: <https://github.com/Keniajin/protGear>
* Email: <mailto:keniajin@gmail.com>

Run `revdepcheck::revdep_details(, "protGear")` for more info

## In both

*   checking R code for possible problems ... NOTE
     ```
     ...
     rlm_normalise_matrix: no visible binding for global variable
       ‘sample_array_ID’
     rlm_normalise_matrix: no visible binding for global variable ‘Block’
     tag_subtract: no visible binding for global variable ‘TAG’
     tag_subtract: no visible binding for global variable ‘TAG_name’
     tag_subtract: no visible binding for global variable ‘TAG_mfi’
     visualize_slide: no visible binding for global variable ‘Block’
     visualize_slide: no visible binding for global variable ‘X’
     visualize_slide: no visible binding for global variable ‘Y’
     visualize_slide: no visible binding for global variable ‘meanX’
     visualize_slide: no visible binding for global variable ‘meanY’
     visualize_slide_2d: no visible binding for global variable ‘Block’
     visualize_slide_2d: no visible binding for global variable ‘X’
     visualize_slide_2d: no visible binding for global variable ‘Y’
     visualize_slide_2d: no visible binding for global variable ‘meanX’
     visualize_slide_2d: no visible binding for global variable ‘meanY’
     Undefined global functions or variables:
       .id Array BGMedian BG_Median Block CVX CVX1_X2 CVX1_X3 CVX2_X3 Column
       Description FBG_Median FMedian FMedianBG_correct MFI MFI_val Name Row
       TAG TAG_mfi TAG_name X Y antigen antigen_name best_CV buffers
       day_batches global_BGMedian index log_bg log_fb machines
       meanBest2_RLM meanX meanX1_X2 meanX1_X3 meanX2_X3 meanY mean_all_anti
       mean_mfi minimum_BGMedian prop rank_mean_all_anti sampleID sampleID2
       sample_array_ID sample_index sdX sdX1_X2 sdX1_X3 sdX2_X3 sd_mfi
       selected slide stdev_all_anti temp v1 v2 val value variable x xbar
     ```

# RHMS (1.7)

* Email: <mailto:rezgararabzadeh@ut.ac.ir>
* GitHub mirror: <https://github.com/cran/RHMS>

Run `revdepcheck::revdep_details(, "RHMS")` for more info

## In both

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) reachRouting.Rd:15: Lost braces
         15 |   \item{inflow}{a vector of runoff (cms) presenting a runoff event generated by excess rainfall computed by \code{loss} methods or an object inherited from any of the following classes :{\code{transform} ; \code{reachRouting} ; \code{reservoirRouting}}.}
            |                                                                                                                                                                                           ^
     checkRd: (-1) reachRouting.base.Rd:14: Lost braces
         14 |   \item{inflow}{a vector of runoff (cms) or an object inherited from any of the following classes :{\code{transform} ; \code{reachRouting} ; \code{reservoirRouting}}.}
            |                                                                                                    ^
     checkRd: (-1) reachRouting.default.Rd:21: Lost braces
         21 |   \item{inflow}{a vector of runoff (cms) or an object inherited from any of the following classes :{\code{transform} ; \code{reachRouting} ; \code{reservoirRouting}}.}
            |                                                                                                    ^
     checkRd: (-1) reservoirRouting.Rd:13: Lost braces
         13 |   \item{inflow}{a vector of in (cms) presenting a runoff event generated by excess rainfall computed by \code{loss} methods or an object inherited from any of the following classes :{\code{transform} ; \code{reachRouting} ; \code{reservoirRouting}}.}
            |                                                                                                                                                                                       ^
     checkRd: (-1) reservoirRouting.base.Rd:13: Lost braces
         13 |   \item{inflow}{a vector of in (cms) presenting a runoff event generated by excess rainfall computed by \code{loss} methods or an object inherited from any of the following classes :{\code{transform} ; \code{reachRouting} ; \code{reservoirRouting}}.}
            |                                                                                                                                                                                       ^
     checkRd: (-1) reservoirRouting.default.Rd:18: Lost braces
         18 |   \item{inflow}{a vector of in (cms) presenting a runoff event generated by excess rainfall computed by \code{loss} methods or an object inherited from any of the following classes :{\code{transform} ; \code{reachRouting} ; \code{reservoirRouting}}.}
            |                                                                                                                                                                                       ^
     ```

# rrr (1.0.0)

* GitHub: <https://github.com/chrisaddy/rrr>
* Email: <mailto:chris.william.addy@gmail.com>
* GitHub mirror: <https://github.com/cran/rrr>

Run `revdepcheck::revdep_details(, "rrr")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘Rcpp’
       All declared Imports should be used.
     ```

# rsurv (0.0.2)

* GitHub: <https://github.com/fndemarqui/rsurv>
* Email: <mailto:fndemarqui@est.ufmg.br>
* GitHub mirror: <https://github.com/cran/rsurv>

Run `revdepcheck::revdep_details(, "rsurv")` for more info

## In both

*   checking DESCRIPTION meta-information ... NOTE
     ```
       Missing dependency on R >= 4.1.0 because package code uses the pipe
       |> or function shorthand \(...) syntax added in R 4.1.0.
       File(s) using such syntax:
         ‘lp.Rd’
     ```

# s20x (3.4.0)

* GitHub: <https://github.com/STATS-UOA/s20x>
* Email: <mailto:j.curran@auckland.ac.nz>
* GitHub mirror: <https://github.com/cran/s20x>

Run `revdepcheck::revdep_details(, "s20x")` for more info

## In both

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       /Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/R/wrap.R:133:     rlang::inject(original_fn(!!!allParams))
       /Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/R/wrap.R:162:       cli::cli_abort("all parameters must be named arguments")
       /Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/R/wrap.R:165:       cli::cli_abort("all parameters must be named arguments")
       /Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/R/zzz.R:8:     S7::methods_register()
       ── Failure ('test-stage14-style-compliance.R:94:3'): Stage 14 diagnostic internals avoid qualified namespace calls ──
       Expected `offendingLines` to have length 0.
       Actual length: 417.
       ── Failure ('test-stage14-style-compliance.R:106:3'): Stage 14 internal diagnostic helpers have roxygen comments ──
       Expected `helperFiles` to have length 2.
       Actual length: 0.
       
       [ FAIL 7 | WARN 0 | SKIP 0 | PASS 536 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# scPipe (2.12.0)

* GitHub: <https://github.com/LuyiTian/scPipe>
* Email: <mailto:su.s@wehi.edu.au>

Run `revdepcheck::revdep_details(, "scPipe")` for more info

## In both

*   checking whether package ‘scPipe’ can be installed ... NOTE
     ```
     Found the following notes/warnings:
       SystemRequirements specified C++11: support has been removed
     See ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/scPipe/new/scPipe.Rcheck/00install.out’ for details.
     ```

*   checking C++ specification ... NOTE
     ```
       Obsolete C++11 standard request will be ignored
     ```

*   checking dependencies in R code ... NOTE
     ```
     Namespaces in Imports field not imported from:
       ‘flexmix’ ‘vctrs’
       All declared Imports should be used.
     ```

*   checking R code for possible problems ... NOTE
     ```
     ...
     sc_interactive_umap_plot : server: no visible binding for global
       variable ‘UMAP1’
     sc_interactive_umap_plot : server: no visible binding for global
       variable ‘UMAP2’
     sc_interactive_umap_plot : server: no visible binding for global
       variable ‘barcode’
     sc_mae_plot_umap : <anonymous>: no visible global function definition
       for ‘irlba’
     sc_mae_plot_umap : <anonymous>: no visible binding for global variable
       ‘value’
     sc_mae_plot_umap : <anonymous>: no visible binding for global variable
       ‘cluster’
     sc_mae_plot_umap : <anonymous>: no visible global function definition
       for ‘experiments’
     sc_mae_plot_umap: no visible binding for global variable ‘UMAP1’
     sc_mae_plot_umap: no visible binding for global variable ‘UMAP2’
     Undefined global functions or variables:
       ..count.. .N .SD Freq UMAP1 UMAP2 Var1 barcode cluster count end
       experiments irlba log_cells_per_feature log_counts_per_cell
       log_counts_per_feature log_features_per_cell seqnames start value
       write.csv
     Consider adding
       importFrom("stats", "end", "start")
       importFrom("utils", "write.csv")
     to your NAMESPACE file.
     ```

# SeqSQC (1.34.0)

* GitHub: <https://github.com/Liubuntu/SeqSQC>
* Email: <mailto:qliu7@buffalo.edu>

Run `revdepcheck::revdep_details(, "SeqSQC")` for more info

## In both

*   checking whether package ‘SeqSQC’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘e1071::element’ by ‘ggplot2::element’ when loading ‘SeqSQC’
       Warning: replacing previous import ‘S4Vectors::rename’ by ‘plotly::rename’ when loading ‘SeqSQC’
       Warning: replacing previous import ‘IRanges::slice’ by ‘plotly::slice’ when loading ‘SeqSQC’
       Warning: replacing previous import ‘ggplot2::last_plot’ by ‘plotly::last_plot’ when loading ‘SeqSQC’
     See ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/SeqSQC/new/SeqSQC.Rcheck/00install.out’ for details.
     ```

# simmr (0.5.3)

* GitHub: <https://github.com/andrewcparnell/simmr>
* Email: <mailto:andrew.parnell1@ucd.ie>
* GitHub mirror: <https://github.com/cran/simmr>

Run `revdepcheck::revdep_details(, "simmr")` for more info

## In both

*   checking whether package ‘simmr’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/simmr/new/simmr.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘simmr’ ...
** this is package ‘simmr’ version ‘0.5.3’
** package ‘simmr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.4.1.1)’
using SDK: ‘MacOSX27.2.sdk’
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/Rcpp/include' -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/RcppArmadillo/include' -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/RcppDist/include' -I/opt/R/arm64/include -I/usr/local/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/Rcpp/include' -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/RcppArmadillo/include' -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/RcppDist/include' -I/opt/R/arm64/include -I/usr/local/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2   -c run_VB.cpp -o run_VB.o
run_VB.cpp:6:21: warning: unused variable 'log2pi' [-Wunused-const-variable]
...
	The rjags package requires JAGS 4.x.y to be installed.

	The error returned from library.dynam(...) was:
	unable to load shared object '/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/rjags/libs/rjags.so':
  dlopen(/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/rjags/libs/rjags.so, 0x000A): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: <3DAC8692-DA01-3CFF-80FC-EA79C4DB9E0A> /Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/rjags/libs/rjags.so
  Reason: tried: '/usr/local/lib/libjags.4.dylib' (no such file), '/System/Volumes/Preboot/Cryptexes/OS/usr/local/lib/libjags.4.dylib' (no such file), '/usr/local/lib/libjags.4.dylib' (no such file), '/Library/Framew
Execution halted
ERROR: lazy loading failed for package ‘simmr’
* removing ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/simmr/new/simmr.Rcheck/simmr’


```
### CRAN

```
* installing *source* package ‘simmr’ ...
** this is package ‘simmr’ version ‘0.5.3’
** package ‘simmr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.4.1.1)’
using SDK: ‘MacOSX27.2.sdk’
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/Rcpp/include' -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/RcppArmadillo/include' -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/RcppDist/include' -I/opt/R/arm64/include -I/usr/local/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/Rcpp/include' -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/RcppArmadillo/include' -I'/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/RcppDist/include' -I/opt/R/arm64/include -I/usr/local/include -Xclang -fopenmp    -fPIC  -falign-functions=64 -Wall -g -O2   -c run_VB.cpp -o run_VB.o
run_VB.cpp:6:21: warning: unused variable 'log2pi' [-Wunused-const-variable]
...
	The rjags package requires JAGS 4.x.y to be installed.

	The error returned from library.dynam(...) was:
	unable to load shared object '/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/rjags/libs/rjags.so':
  dlopen(/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/rjags/libs/rjags.so, 0x000A): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: <3DAC8692-DA01-3CFF-80FC-EA79C4DB9E0A> /Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/library.noindex/simmr/rjags/libs/rjags.so
  Reason: tried: '/usr/local/lib/libjags.4.dylib' (no such file), '/System/Volumes/Preboot/Cryptexes/OS/usr/local/lib/libjags.4.dylib' (no such file), '/usr/local/lib/libjags.4.dylib' (no such file), '/Library/Framew
Execution halted
ERROR: lazy loading failed for package ‘simmr’
* removing ‘/Users/barret/conductor/workspaces/ggally.nosync/daegu-v1/revdep/checks.noindex/simmr/old/simmr.Rcheck/simmr’


```
# SmartEDA (0.3.10)

* GitHub: <https://github.com/daya6489/SmartEDA>
* Email: <mailto:daya6489@gmail.com>
* GitHub mirror: <https://github.com/cran/SmartEDA>

Run `revdepcheck::revdep_details(, "SmartEDA")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘qpdf’
       All declared Imports should be used.
     ```

# specmine (4.0.1)

* GitHub: <https://github.com/PedroFontao/specmine>
* Email: <mailto:pedrofontao812004@gmail.com>
* GitHub mirror: <https://github.com/cran/specmine>

Run `revdepcheck::revdep_details(, "specmine")` for more info

## In both

*   checking running R code from vignettes ...
     ```
       ‘dataset_2d_analysis.Rmd’ using ‘UTF-8’... failed
      ERROR
     Errors in running code in vignettes:
     when running code in ‘dataset_2d_analysis.Rmd’
       ...
     > library(pins)
     
     > board_register_github(repo = "BrunoMiguelPereira/test_2d", 
     +     token = "7ae9e5b34ae6417fc2a6cbb249acb90374430bef")
     
       When sourcing ‘dataset_2d_analysis.R’:
     Error: `board_register_github()` was deprecated in pins 1.4.0 and is now
     defunct.
     ℹ Learn more at <https://pins.rstudio.com/articles/pins-update.html>
     Execution halted
     ```

# survstan (0.0.7.1)

* GitHub: <https://github.com/fndemarqui/survstan>
* Email: <mailto:fndemarqui@est.ufmg.br>
* GitHub mirror: <https://github.com/cran/survstan>

Run `revdepcheck::revdep_details(, "survstan")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespaces in Imports field not imported from:
       ‘RcppParallel’ ‘rstantools’
       All declared Imports should be used.
     ```

# systemPipeTools (1.20.0)

* Email: <mailto:danicassol@gmail.com>

Run `revdepcheck::revdep_details(, "systemPipeTools")` for more info

## In both

*   checking dependencies in R code ... WARNING
     ```
     Missing or unexported object: ‘BiocGenerics::rowSums’
     ```

# tidybulk (2.2.0)

* GitHub: <https://github.com/stemangiola/tidybulk>
* Email: <mailto:mangiolastefano@gmail.com>

Run `revdepcheck::revdep_details(, "tidybulk")` for more info

## In both

*   checking examples ... ERROR
     ```
     ...
     tidybulk says: to access the DE object do `metadata(.)$tidybulk$deseq2_object`
     tidybulk says: to access the raw results (fitted GLM) do `metadata(.)$tidybulk$deseq2_fit`
     > 
     > # Use random intercept and random effect models
     > 
     >  airway[1:50,] |>
     +   identify_abundant(factor_of_interest = condition) |>
     +   test_differential_abundance(
     +     ~ condition + (1 + condition | cell),
     +     method = "glmmseq_lme4", cores = 1
     +   )
     Warning: the ‘nobars’ function has moved to the reformulas package. Please update your imports, or ask an upstream package maintainer to do so.
     calcNormFactors has been renamed to normLibSizes
     Warning: the ‘findbars’ function has moved to the reformulas package. Please update your imports, or ask an upstream package maintainer to do so.
     Warning: the ‘subbars’ function has moved to the reformulas package. Please update your imports, or ask an upstream package maintainer to do so.
     
     n = 8 samples, 4 individuals
     
     All genes returned an error. Check call. Check sufficient data in each group
                                                                                                                                     ENSG00000000003 
     "Error in model.frame.default(data = data, offset = offset, drop.unused.levels = TRUE,  : \n  invalid type (closure) for variable '(offset)'\n" 
     Error in object@stats : 
       no applicable method for `@` applied to an object of class "character"
     Calls: test_differential_abundance ... reattach_metadata -> is -> mutate -> as_tibble -> summary_lmmSeq
     Execution halted
     ```

*   checking running R code from vignettes ...
     ```
       ‘comparison_coding.Rmd’ using ‘UTF-8’... OK
       ‘introduction.Rmd’ using ‘UTF-8’... failed
      ERROR
     Errors in running code in vignettes:
     when running code in ‘introduction.Rmd’
       ...
     
     n = 8 samples, 4 individuals
     
     All genes returned an error. Check call. Check sufficient data in each group
                                                                                                                                              TSPAN6 
     "Error in model.frame.default(data = data, offset = offset, drop.unused.levels = TRUE,  : \n  invalid type (closure) for variable '(offset)'\n" 
     
       When sourcing ‘introduction.R’:
     Error: no applicable method for `@` applied to an object of class "character"
     Execution halted
     ```

*   checking dependencies in R code ... NOTE
     ```
     Package in Depends field not imported from: ‘ttservice’
       These packages need to be imported from (in the NAMESPACE file)
       for when this namespace is loaded but not attached.
     ```

*   checking R code for possible problems ... NOTE
     ```
     .aggregate_duplicates_se: no visible global function definition for
       ‘where’
     aggregate_duplicates,RangedSummarizedExperiment: no visible global
       function definition for ‘where’
     aggregate_duplicates,SummarizedExperiment: no visible global function
       definition for ‘where’
     Undefined global functions or variables:
       where
     ```

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) remove_redundancy-methods.Rd:104-113: Lost braces
        104 | select_closest_pairs = function(df) {
            |                                     ^
     checkRd: (-1) remove_redundancy-methods.Rd:106-111: Lost braces
        106 |   while (df |> nrow() > 0) {
            |                            ^
     checkRd: (-1) rotate_dimensions-methods.Rd:76-82: Lost braces
         76 | rotation = function(m, d) {
            |                           ^
     ```

*   checking for unstated dependencies in vignettes ... NOTE
     ```
     'library' or 'require' call not declared from: ‘forcats’
     ```

# tidyseurat (0.8.10)

* GitHub: <https://github.com/stemangiola/tidyseurat>
* Email: <mailto:mangiolastefano@gmail.com>
* GitHub mirror: <https://github.com/cran/tidyseurat>

Run `revdepcheck::revdep_details(, "tidyseurat")` for more info

## In both

*   checking running R code from vignettes ...
     ```
       ‘figures_article.Rmd’ using ‘UTF-8’... OK
       ‘introduction.Rmd’ using ‘UTF-8’... failed
      ERROR
     Errors in running code in vignettes:
     when running code in ‘introduction.Rmd’
       ...
     
     Quitting from introduction.Rmd:30-31 [unnamed-chunk-2]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     file(con, "r")
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     
       When tangling ‘introduction.Rmd’:
     Error: cannot open the connection
     Execution halted
     ```

# tip (0.1.0)

* Email: <mailto:charleswharrison@knights.ucf.edu>
* GitHub mirror: <https://github.com/cran/tip>

Run `revdepcheck::revdep_details(, "tip")` for more info

## In both

*   checking Rd files ... NOTE
     ```
     checkRd: (-1) partition_undirected_graph.Rd:20: Lost braces; missing escapes or markup?
         20 | \item{cutoff}{Numeric. The value max(0, g_{i,j} - cutoff) so that there are <\code{.num_components}> components in the graph.}
            |                                           ^
     ```

# TVTB (1.38.0)

* GitHub: <https://github.com/kevinrue/TVTB>
* Email: <mailto:kevinrue67@gmail.com>

Run `revdepcheck::revdep_details(, "TVTB")` for more info

## In both

*   checking Rd files ... NOTE
     ```
     checkRd: (7) NEWS.Rd:369-371: Tag \code is invalid in a \code block
     ```

# VISTA (1.0.0)

* GitHub: <https://github.com/cparsania/VISTA>
* Email: <mailto:chirag.parsania@gmail.com>

Run `revdepcheck::revdep_details(, "VISTA")` for more info

## In both

*   checking R code for possible problems ... NOTE
     ```
     ...
     print.VISTA: no visible global function definition for ‘as’
     run_cell_deconvolution: no visible global function definition for
       ‘getFromNamespace’
     run_edger_analysis: no visible binding for global variable ‘regulation’
     run_limma_analysis: no visible binding for global variable ‘regulation’
     run_vista_report: no visible global function definition for ‘head’
     run_vista_report : <anonymous>: no visible global function definition
       for ‘head’
     enrichMsigDB,VISTA : convert_ids: no visible global function definition
       for ‘na.omit’
     Undefined global functions or variables:
       ..count.. ..stratum.. .data := Description Dim1 Dim2 PC1 PC2 UMAP1
       UMAP2 Var1 Var2 aes_string as as.dist avg_log2FC cell_type chr
       cluster comparison count_data data display_gene err expr_x expr_y
       fc_dir gene gene_id gene_label gene_symbol getFromNamespace group
       hclust head label log10_baseMean log2FoldChange log2fc log2fc1
       log2fc2 logFDR m mean_score n na.omit name pos_mb regulation
       sample_comparisons sample_metadata score sd_score stat_ellipse status
       symbol value value_clipped x_dodge
     Consider adding
       importFrom("methods", "as")
       importFrom("stats", "as.dist", "hclust", "na.omit")
       importFrom("utils", "data", "getFromNamespace", "head")
     to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
     contains 'methods').
     ```

# WOTPLY (0.1.0)

* Email: <mailto:gabriele.lubatti@helmholtz-muenchen.de>
* GitHub mirror: <https://github.com/cran/WOTPLY>

Run `revdepcheck::revdep_details(, "WOTPLY")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘sna’
       All declared Imports should be used.
     ```

