# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.2.3 (2015-12-10) |
|system   |x86_64, darwin13.4.0         |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Indiana/Indianapolis |
|date     |2015-12-29                   |

## Packages

|package      |*  |version |date       |source         |
|:------------|:--|:-------|:----------|:--------------|
|arm          |   |1.8-6   |2015-07-11 |CRAN (R 3.2.2) |
|geosphere    |   |1.5-1   |2015-12-18 |CRAN (R 3.2.3) |
|ggmap        |   |2.6     |2015-12-19 |CRAN (R 3.2.3) |
|ggplot2      |   |2.0.0   |2015-12-18 |CRAN (R 3.2.3) |
|gtable       |   |0.1.2   |2012-12-05 |CRAN (R 3.0.0) |
|igraph       |   |1.0.1   |2015-06-26 |CRAN (R 3.2.0) |
|intergraph   |   |2.0-2   |2015-06-30 |CRAN (R 3.2.0) |
|knitr        |   |1.11    |2015-08-14 |CRAN (R 3.2.2) |
|lintr        |   |0.3.3   |2015-09-15 |CRAN (R 3.2.3) |
|maps         |   |3.0.1   |2015-12-04 |CRAN (R 3.2.3) |
|network      |   |1.12.0  |2015-04-07 |CRAN (R 3.2.0) |
|plyr         |   |1.8.3   |2015-06-12 |CRAN (R 3.2.0) |
|RColorBrewer |   |1.1-2   |2014-12-07 |CRAN (R 3.2.0) |
|reshape      |   |0.8.5   |2014-04-23 |CRAN (R 3.0.2) |
|roxygen2     |   |5.0.1   |2015-11-11 |CRAN (R 3.2.2) |
|scagnostics  |   |0.2-4   |2012-11-05 |CRAN (R 3.1.0) |
|scales       |   |0.3.0   |2015-08-25 |CRAN (R 3.2.0) |
|sna          |   |2.3-2   |2014-01-14 |CRAN (R 3.2.2) |
|survival     |   |2.38-3  |2015-07-02 |CRAN (R 3.2.0) |
|testthat     |   |0.11.0  |2015-10-14 |CRAN (R 3.2.3) |
|tnet         |   |3.0.11  |2012-11-20 |CRAN (R 3.1.0) |

# Check results
15 checked out of 15 dependencies 

## DescribeDisplay (0.2.4)
Maintainer: Di Cook <dicook@iastate.edu>

```
checking whether package ‘DescribeDisplay’ can be installed ... ERROR
Installation failed.
See ‘/private/var/folders/b_/p70ksf2n01n6bjj3zs47s8rc0000gn/T/RtmpbPiubW/check_cran12f4445b08432/DescribeDisplay.Rcheck/00install.out’ for details.
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
  'params' is a deprecated argument.  Please 'wrap' the function to supply arguments. help("wrap", package = "GGally")
Calls: ggs_pairs ... check_and_set_ggpairs_defaults -> display_param_error
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
Packages required but not available: ‘GOstats’ ‘GOSemSim’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## MissingDataGUI (0.2-4)
Maintainer: Xiaoyue Cheng <xycheng@unomaha.edu>

```
checking whether package ‘MissingDataGUI’ can be installed ... ERROR
Installation failed.
See ‘/private/var/folders/b_/p70ksf2n01n6bjj3zs47s8rc0000gn/T/RtmpbPiubW/check_cran12f4445b08432/MissingDataGUI.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## ParamHelpers (1.6)
Maintainer: Bernd Bischl <bernd_bischl@gmx.net>  
Bug reports: https://github.com/berndbischl/ParamHelpers/issues

__OK__

## PopGenReport (2.2)
Maintainer: Bernd Gruber <bernd.gruber@canberra.edu.au>

__OK__

## qualvar (0.1.0)
Maintainer: Joel Gombin <joel.gombin@gmail.com>

__OK__

## robCompositions (1.9.1)
Maintainer: Matthias Templ <templ@tuwien.ac.at>

```
checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: ‘VIM’, ‘StatDA’
```
```
DONE
Status: 1 NOTE
```

## robustbase (0.92-5)
Maintainer: Martin Maechler <maechler@stat.math.ethz.ch>

__OK__

## specmine (1.0)
Maintainer: Christopher Costa <chrisbcl@hotmail.com>

```
checking package dependencies ... ERROR
Packages required but not available: ‘xcms’ ‘MAIT’ ‘genefilter’ ‘impute’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## svdvis (0.1)
Maintainer: Neo Christopher Chung <nchchung@gmail.com>

__OK__

## toaster (0.4.1)
Maintainer: Gregory Kanevsky <gregory.kanevsky@teradata.com>  
Bug reports: https://github.com/teradata-aster-field/toaster/issues

__OK__

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

## vdmR (0.2.1)
Maintainer: Tomokazu Fujino <fujino@fwu.ac.jp>

__OK__

