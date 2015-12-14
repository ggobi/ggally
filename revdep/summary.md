# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.2.2 (2015-08-14) |
|system   |x86_64, darwin13.4.0         |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Indiana/Indianapolis |
|date     |2015-12-14                   |

## Packages

|package      |*  |version    |date       |source                           |
|:------------|:--|:----------|:----------|:--------------------------------|
|arm          |   |1.8-6      |2015-07-11 |CRAN (R 3.2.2)                   |
|geosphere    |   |1.4-3      |2015-07-02 |CRAN (R 3.2.0)                   |
|ggmap        |   |2.5.2      |2015-08-21 |CRAN (R 3.2.0)                   |
|ggplot2      |*  |1.0.1.9003 |2015-10-25 |Github (hadley/ggplot2@ef33dc7)  |
|gtable       |   |0.1.2      |2012-12-05 |CRAN (R 3.0.0)                   |
|igraph       |   |1.0.1      |2015-06-26 |CRAN (R 3.2.0)                   |
|intergraph   |   |2.0-2      |2015-06-30 |CRAN (R 3.2.0)                   |
|knitr        |   |1.11       |2015-08-14 |CRAN (R 3.2.2)                   |
|lintr        |   |0.3.3      |2015-09-19 |Github (jimhester/lintr@56c35a3) |
|maps         |   |2.3-11     |2015-08-03 |CRAN (R 3.2.0)                   |
|network      |   |1.12.0     |2015-04-07 |CRAN (R 3.2.0)                   |
|plyr         |   |1.8.3      |2015-06-12 |CRAN (R 3.2.0)                   |
|RColorBrewer |   |1.1-2      |2014-12-07 |CRAN (R 3.2.0)                   |
|reshape      |   |0.8.5      |2014-04-23 |CRAN (R 3.0.2)                   |
|roxygen2     |   |5.0.1      |2015-11-11 |CRAN (R 3.2.2)                   |
|scagnostics  |   |0.2-4      |2012-11-05 |CRAN (R 3.1.0)                   |
|scales       |   |0.3.0      |2015-08-25 |CRAN (R 3.2.0)                   |
|sna          |   |2.3-2      |2014-01-14 |CRAN (R 3.2.2)                   |
|survival     |   |2.38-3     |2015-07-02 |CRAN (R 3.2.0)                   |
|testthat     |   |0.10.0     |2015-05-22 |CRAN (R 3.2.0)                   |
|tnet         |   |3.0.11     |2012-11-20 |CRAN (R 3.1.0)                   |

# Check results
14 checked out of 14 dependencies 

## DescribeDisplay (0.2.4)
Maintainer: Di Cook <dicook@iastate.edu>

```
checking whether package ‘DescribeDisplay’ can be installed ... ERROR
Installation failed.
See ‘/private/var/folders/b_/p70ksf2n01n6bjj3zs47s8rc0000gn/T/Rtmpjs0Cml/check_cran10132209662f3/DescribeDisplay.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## freqparcoord (1.0.0)
Maintainer: Norm Matloff <normmatloff@gmail.com>

```
checking dependencies in R code ... NOTE
Package in Depends field not imported from: ‘parallel’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
smoothz: no visible global function definition for ‘splitIndices’
smoothz: no visible global function definition for ‘clusterApply’
smoothzpred: no visible global function definition for ‘splitIndices’
smoothzpred: no visible global function definition for ‘clusterApply’
```
```
checking examples ... ERROR
Running examples in ‘freqparcoord-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: smoothz
> ### Title: Smoothing functions.
> ### Aliases: smoothz smoothzpred knnreg knndens
> 
> ### ** Examples
> 
> 
> # programmers and engineers in Silicon Valley, 2000 census, age 25-65
> data(prgeng)
> pg <- prgeng
> pg1 <- pg[pg$age >= 25 & pg$age <= 65,]
> estreg <- smoothz(pg1[,c(1,8)],sf=knnreg,k=100)
> age <- pg1[,1]
> p <- ggplot(data.frame(age,estreg))
> p + geom_smooth(aes(x=age,y=estreg))
Error in loadNamespace(name) : there is no package called ‘mgcv’
Calls: <Anonymous> ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
Execution halted
```
```
DONE
Status: 1 ERROR, 2 NOTEs
```

## ggmcmc (0.7.2)
Maintainer: Xavier Fernández i Marín <xavier.fim@gmail.com>  
Bug reports: https://github.com/xfim/ggmcmc/issues

```
checking examples ... ERROR
Running examples in ‘ggmcmc-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: ggs_pairs
> ### Title: Create a plot matrix of posterior simulations
> ### Aliases: ggs_pairs
> 
> ### ** Examples
> 
> data(linear)
> 
> # default ggpairs plot
> ggs_pairs(ggs(s))
> 
> # change alpha transparency of points
> ggs_pairs(ggs(s), lower=list(params=c(alpha=.2)))
Error in display_param_error() : 
  'params' is a depricated argument.  Please 'wrap' the function to supply arguments. help("wrap", package = "GGally")
Calls: ggs_pairs ... ggpairs -> check_and_set_defaults -> display_param_error
Execution halted
```
```
DONE
Status: 1 ERROR
```

## LANDD (1.0.0)
Maintainer: Shangzhao Qiu <qsz1328@gmail.com>

```
checking package dependencies ... ERROR
Packages required but not available: ‘GOstats’ ‘GOSemSim’ ‘modeest’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## MissingDataGUI (0.2-2)
Maintainer: Xiaoyue Cheng <xycheng@iastate.edu>

```
checking whether package ‘MissingDataGUI’ can be installed ... ERROR
Installation failed.
See ‘/private/var/folders/b_/p70ksf2n01n6bjj3zs47s8rc0000gn/T/Rtmpjs0Cml/check_cran10132209662f3/MissingDataGUI.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## ParamHelpers (1.6)
Maintainer: Bernd Bischl <bernd_bischl@gmx.net>  
Bug reports: https://github.com/berndbischl/ParamHelpers/issues

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘eaf’
```
```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘eaf’
```
```
checking tests ... ERROR
Running the tests in ‘tests/run-all.R’ failed.
Last 13 lines of output:
         warning = function(c) invokeRestart("muffleWarning"))
  2: eval(code, new_test_environment)
  3: eval(expr, envir, enclos)
  4: plotEAF(opt.paths) at test_plotEAF.R:27
  5: requirePackages("eaf", why = "plotEAF")
  6: stopf("For %s please install the following packages: %s", why, ps)
  
  testthat results ================================================================
  OK: 836 SKIPPED: 0 FAILED: 1
  1. Error: plotEAF works 
  
  Error: testthat unit tests failed
  Execution halted
```
```
DONE
Status: 1 ERROR, 2 NOTEs
```

## PopGenReport (2.2)
Maintainer: Bernd Gruber <bernd.gruber@canberra.edu.au>

```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘ecodist’
```
```
DONE
Status: 1 NOTE
```

## qualvar (0.1.0)
Maintainer: Joel Gombin <joel.gombin@gmail.com>

__OK__

## robCompositions (1.9.1)
Maintainer: Matthias Templ <templ@tuwien.ac.at>

```
checking whether package ‘robCompositions’ can be installed ... ERROR
Installation failed.
See ‘/private/var/folders/b_/p70ksf2n01n6bjj3zs47s8rc0000gn/T/Rtmpjs0Cml/check_cran10132209662f3/robCompositions.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## robustbase (0.92-5)
Maintainer: Martin Maechler <maechler@stat.math.ethz.ch>

```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘robustX’
```
```
DONE
Status: 1 NOTE
```

## specmine (1.0)
Maintainer: Christopher Costa <chrisbcl@hotmail.com>

```
checking package dependencies ... ERROR
Packages required but not available:
  ‘hyperSpec’ ‘ChemoSpec’ ‘rgl’ ‘caret’ ‘qdap’ ‘xcms’ ‘MAIT’
  ‘genefilter’ ‘impute’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## svdvis (0.1)
Maintainer: Neo Christopher Chung <nchchung@gmail.com>

```
checking examples ... ERROR
Running examples in ‘svdvis-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: svd.scatter
> ### Title: Visualizing Singular Vectors or Principal Components by
> ###   Scatterplot Matrices
> ### Aliases: svd.scatter
> 
> ### ** Examples
> 
> set.seed(1234)
> dat = matrix(rnorm(1000), 100, 10)
> svd.obj = svd(dat)
> colnames(svd.obj$v) = paste0("V",1:10)
> svd.scatter(svd.obj, r=3, group=c(rep("Group1",5), rep("Group2",5)))
[1] "Your input data is treated as a SVD output, with u, d, v corresponding to left singular vector, singular values, and right singular vectors, respectively."
[1] "Multiple Scatter Plots"
Warning in ggpairs(mv, columns = 1:(r), color = "group", alpha = alpha,  :
  Extra arguments: 'color', 'alpha' are being ignored.  If these are meant to be aesthetics, submit them using the 'mapping' variable within ggpairs with ggplot2::aes or ggplot2::aes_string.
Warning in min(x, na.rm = na.rm) :
  no non-missing arguments to min; returning Inf
Warning in max(x, na.rm = na.rm) :
  no non-missing arguments to max; returning -Inf
Warning: Computation failed in `stat_density2d()`:
'x' is NULL
Error in seq.default(from = best$lmin, to = best$lmax, by = best$lstep) : 
  'from' must be of length 1
Calls: <Anonymous> ... f -> <Anonymous> -> <Anonymous> -> seq -> seq.default
Execution halted
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Quitting from lines 50-51 (svdvis-vignette.Rmd) 
Error: processing vignette 'svdvis-vignette.Rmd' failed with diagnostics:
'from' must be of length 1
Execution halted

```
```
DONE
Status: 1 ERROR, 1 NOTE
```

## userfriendlyscience (0.3-0)
Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>

```
checking examples ... ERROR
Running examples in ‘userfriendlyscience-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: meanDiff.multi
> ### Title: meanDiff.multi
> ### Aliases: meanDiff.multi
> ### Keywords: utilities
> 
> ### ** Examples
> 
> ### Create simple dataset
> dat <- data.frame(x1 = factor(rep(c(0,1), 20)),
+                   x2 = factor(c(rep(0, 20), rep(1, 20))),
+                   y=rep(c(4,5), 20) + rnorm(40));
> ### Compute mean difference and show it
> meanDiff.multi(dat, x=c('x1', 'x2'), y='y', var.equal="yes");
Warning: `show_guide` has been deprecated. Please use `show.legend` instead.
Error: Unknown parameters: size
Execution halted
```
```
DONE
Status: 1 ERROR
```

## vdmR (0.2.0)
Maintainer: Tomokazu Fujino <fujino@fwu.ac.jp>

```
checking examples ... ERROR
Running examples in ‘vdmR-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: vcmap
> ### Title: Generate choropleth map with interactive functions
> ### Aliases: vcmap
> 
> ### ** Examples
> 
> data(vsfuk2012)
> shp.path <- file.path(system.file(package="vdmR"), "etc/shapes/kitakyu2012.shp")
> kk2012 <- dplyr::filter(vsfuk2012, CityCode<40110&CityCode>40100)
> vcmap(shp.path, kk2012, "CityCode", "CityCode", "map1", "kk2012")
Error in get("rgeos", envir = .MAPTOOLS_CACHE) : object 'rgeos' not found
Calls: vcmap ... fortify.SpatialPolygonsDataFrame -> <Anonymous> -> rgeosStatus -> get
Execution halted
```
```
checking tests ... ERROR
Running the tests in ‘tests/run-all.R’ failed.
Last 13 lines of output:
  1: withCallingHandlers(eval(code, new_test_environment), error = capture_calls, message = function(c) invokeRestart("muffleMessage"), 
         warning = function(c) invokeRestart("muffleWarning"))
  2: eval(code, new_test_environment)
  3: eval(expr, envir, enclos)
  4: vcmap(shp.path, vsfuk2012, "CityCode", "CityCode", "map01", "vsfuk2012", fill = FertilityRate, 
         ggscale = frcol) at test-vdmR.R:20
  5: ggplot2::fortify(spdf, region = mid)
  6: fortify.SpatialPolygonsDataFrame(spdf, region = mid)
  7: maptools::unionSpatialPolygons(cp, attr[, region])
  8: rgeosStatus()
  9: get("rgeos", envir = .MAPTOOLS_CACHE)
  Error: Test failures
  Execution halted
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Quitting from lines 40-42 (vdmR-vignette.Rnw) 
Error: processing vignette 'vdmR-vignette.Rnw' failed with diagnostics:
unused argument (geom_params = params)
Execution halted

```
```
DONE
Status: 2 ERRORs, 1 NOTE
```

