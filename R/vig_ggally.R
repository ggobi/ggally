#' View GGally vignettes
#'
#' This function will open the directly to the vignette requested. If no \code{name} is provided, the index of all \pkg{GGally} vignettes will be opened.
#'
#' This method allows for vignettes to be hosted remotely, reducing \pkg{GGally}'s package size, and installation time.
#'
#' @param name Vignette name to open. If no name is provided, the vignette index will be opened
#' @export
#' @examples
#' \donttest{
#' # View `ggnostic` vignette
#' vig_ggally("ggnostic")
#'
#' # View all vignettes by GGally
#' vig_ggally()
#' }
vig_ggally <- function(name) {

  vig_url <-
    if (missing(name) || is.null(name)) {
      "https://ggobi.github.io/ggally/articles/"
    } else {
      tryCatch({
        paste0(
          "https://ggobi.github.io/ggally/articles/",
          match.arg(name, vignettes_for_ggally),
          ".html"
        )
      }, error = function(e) {
        message("Unknown vignette: ", name, ". Opening Vignette index page")
        "https://ggobi.github.io/ggally/articles/"
      })
    }

  browseURL(vig_url)
}

vignettes_for_ggally <- c(
  "ggally_plots",
  "ggally_stats",
  "ggbivariate",
  "ggcoef",
  "ggduo",
  "ggmatrix",
  "ggnetworkmap",
  "ggnostic",
  "ggpairs",
  "ggscatmat",
  "ggsurv",
  "ggtable",
  "glyph"
)
