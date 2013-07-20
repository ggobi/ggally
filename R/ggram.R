#' Plot n-gram frequencies
#'
#' \code{ggram} downloads data from the Google Ngram Viewer website and
#' plots it in \code{ggplot2} style.
#'
#' @param phrases vector of phrases
#' @param ignore_case if \code{TRUE} then the frequencies are case insensitive.
#'   Default is \code{FALSE}.
#' @param ignore.case alternative syntax for \code{ignore_case}. Only used if
#'   \code{ignore_case} is not specified.
#' @param google_theme use a Google Ngram-style plot theme.
#' @param geom the ggplot2 geom used to plot the data; defaults to "line"
#' @param geom_options list of additional parameters passed to the ggplot2 geom.
#' @param ... additional parameters passed to \code{ngram}
#' @details 
#'  Google generated two datasets drawn from digitised books in the Google
#'  books collection. One was generated in July 2009, the second in July 2012.
#'  Google will update these datasets as book scanning continues.
#'  
#' @examples 
#' ggram(c("hacker", "programmer"), year_start = 1950)
#' 
#' # Changing the geom.
#' ggram(c("cancer", "fumer", "cigarette"),
#'       year_start = 1900,
#'       corpus = "fre_2012", 
#'       geom = "step")
#'      
#' # Passing more options.
#' require(ggplot2)
#' ggram(c("cancer", "smoking", "tobacco"),
#'       year_start = 1900, 
#'       corpus = "eng_fiction_2012", 
#'       geom = "point", 
#'       geom_options = list(alpha = .5)) + 
#'   stat_smooth(method="loess", se = FALSE)
#'  
#' # Setting the layers manually.
#' ggram(c("cancer", "smoking", "tobacco"),
#'       year_start = 1900, 
#'       corpus = "eng_fiction_2012", 
#'       geom = NULL) +
#'   stat_smooth(method="loess", se=FALSE, span = 0.3)
#'  
#' # Setting the legend placement on a long query.
#' # Example posted by Ben Zimmer at Language Log.
#' x = c("(The United States is + The United States has) / The United States",
#'       "(The United States are + The United States have) / The United States")
#' ggram(x, year_start = 1800) +
#'   theme(legend.direction = "vertical", legend.position = "bottom")

#' @export

ggram <- function(phrases, ignore_case=FALSE, geom="line", ignore.case=ignore_case,
                  geom_options=list(), google_theme = FALSE, ...) {
  # The require below was suggested briatte but it results in a WARNING
  # when running package checks. Is it really necessary?
  #require(scales, quietly=TRUE)
  if (missing(ignore_case)) ignore_case <- ignore.case
  ng <- if(ignore_case) ngrami(phrases, ...) else ngram(phrases, ...)
  p <- ggplot(data = ng, 
             aes_string(x = "Year", y = "Frequency", colour = "Phrase", fill="Phrase"))
  if (!(class(geom) == "character")) geom <- NULL
  if (!is.null(geom)) p <- p + do.call(stat_identity, c(geom = geom, geom_options))
  p <-  p + labs(x = NULL) + 
    scale_y_continuous(labels = percent)
  dots <- list(...)
  labs <- (!("aggregate" %in% names(dots)) || dots["aggregate"]==TRUE)
  if (labs & !google_theme) {
    p <- p +
      scale_colour_discrete("", labels = phrases) + 
      scale_fill_discrete("", labels = phrases)
  }
  else if (labs & google_theme) {
    # Google Ngram palette.
    palette = c("#264EC0", "#D22310", "#FC8608", "#168713", "#850086", "#1086B9", "#D22B63", "#559D05", "#A71B23", "#21436F", "#852D86", "#219B86")
    p = p +
      scale_colour_manual("", values = palette, labels = phrases) +
      scale_fill_manual("", values = palette, labels = phrases) +
      theme(panel.background = element_rect(fill = NA),
            axis.line = element_line(),
            axis.text = element_text(color = "black"),
            axis.ticks = element_blank()) +
      labs(y = NULL)
  } else {
    p <- p +
      scale_colour_discrete("") +
      scale_fill_discrete("")  
  }
  return(p)
}

