# ---- cache ----
ignore <- suppressMessages(library(GGally))

knitr::opts_chunk$set(
  fig.width = 9, fig.height = 7,
  fig.retina = 1
)
cache_wd <- function(name) {
  knitr::opts_chunk$set(
    cache = TRUE,
    cache.path = file.path("cache", name, ""),
    fig.path = file.path("cache", name, "_")
  )
}

