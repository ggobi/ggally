# Check results
11 checked out of 11 dependencies

## DescribeDisplay (0.2.4)
Maintainer: Di Cook <dicook@iastate.edu>

```
checking whether package ‘DescribeDisplay’ can be installed ... ERROR
Installation failed.
DONE
Status: 1 ERROR
```

## freqparcoord (1.0.0)
Maintainer: Norm Matloff <normmatloff@gmail.com>

```
checking whether package ‘freqparcoord’ can be installed ... ERROR
Installation failed.
DONE
Status: 1 ERROR
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
DONE
Status: 1 ERROR
```

## MissingDataGUI (0.2-2)
Maintainer: Xiaoyue Cheng <xycheng@iastate.edu>

```
checking whether package ‘MissingDataGUI’ can be installed ... ERROR
Installation failed.
DONE
Status: 1 ERROR
```

## PopGenReport (2.2)
Maintainer: Bernd Gruber <bernd.gruber@canberra.edu.au>

```
checking whether package ‘PopGenReport’ can be installed ... ERROR
Installation failed.
DONE
Status: 1 ERROR
```

## qualvar (0.1.0)
Maintainer: Joel Gombin <joel.gombin@gmail.com>

__OK__

## robCompositions (1.9.1)
Maintainer: Matthias Templ <templ@tuwien.ac.at>

```
checking whether package ‘robCompositions’ can be installed ... ERROR
Installation failed.
DONE
Status: 1 ERROR
```

## robustbase (0.92-5)
Maintainer: Martin Maechler <maechler@stat.math.ethz.ch>

```
checking whether package ‘robustbase’ can be installed ... ERROR
Installation failed.
DONE
Status: 1 ERROR
```

## userfriendlyscience (0.3-0)
Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>

```
checking whether package ‘userfriendlyscience’ can be installed ... ERROR
Installation failed.
DONE
Status: 1 ERROR
```

## vdmR (0.1.1)
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
