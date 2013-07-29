#' Get n-gram frequencies
#'
#' \code{ngram} downloads data from the Google Ngram Viewer website and
#' returns it in a dataframe.
#'
#' @param phrases vector of phrases, with a maximum of 12 items
#' @param corpus Google corpus to search (see Details for possible values)
#' @param year_start start year, default is 1500
#' @param year_end end year, default is 2008
#' @param tag apply a part-of-speech tag to the whole vector of phrases
#' @param smoothing smoothing paramater, default is 3
#' @details 
#'  Google generated two datasets drawn from digitised books in the Google
#'  Books collection. One was generated in July 2009, the second in July 2012.
#'  Google will update these datasets as book scanning continues.
#'  
#'  This function provides the annual frequency of words or phrases, known as
#'  n-grams, in a sub-collection or "corpus" taken from the Google Books collection.
#'  The search across the corpus is case-sensitive. For a case-insensitive search
#'  use \code{\link{ngrami}}. 
#'  
#' Below is a list of available corpora.
#' \tabular{ll}{
#' \bold{Corpus} \tab \bold{Corpus Name}\cr
#' eng_us_2012\tab American English 2012\cr
#' eng_us_2009\tab American English 2009\cr
#' eng_gb_2012\tab British English 2012\cr
#' eng_gb_2009\tab British English 2009\cr
#' chi_sim_2012\tab Chinese 2012\cr
#' chi_sim_2009\tab Chinese 2009\cr
#' eng_2012\tab English 2012\cr
#' eng_2009\tab English 2009\cr
#' eng_fiction_2012\tab English Fiction 2012\cr
#' eng_fiction_2009\tab English Fiction 2009\cr
#' eng_1m_2009\tab Google One Million\cr
#' fre_2012\tab French 2012\cr
#' fre_2009\tab French 2009\cr
#' ger_2012\tab German 2012\cr
#' ger_2009\tab German 2009\cr
#' heb_2012\tab Hebrew 2012\cr
#' heb_2009\tab Hebrew 2009\cr
#' spa_2012\tab Spanish 2012\cr
#' spa_2009\tab Spanish 2009\cr
#' rus_2012\tab Russian 2012\cr
#' rus_2009\tab Russian 2009\cr
#' ita_2012\tab Italian 2012\cr
#' }
#' 
#' The Google Million is a sub-collection of Google Books. All are in
#' English with dates ranging from 1500 to 2008.
#' No more than about 6,000 books were chosen from any one year, which means that
#' all of the scanned books from early years are present, and books from later
#' years are randomly sampled. The random samplings reflect the subject distributions
#' for the year (so there are more computer books in 2000 than 1980).
#' 
#' See \url{http://books.google.com/ngrams/info} for the full Ngram syntax.
#' @examples 
#' freq <- ngram(c("mouse", "rat"), year_start = 1950)
#' head(freq)
#' freq <- ngram(c("blue", "red"), tag = "ADJ")
#' head(freq)
#' freq <- ngram(c("President Roosevelt", "President Truman"), tag = "START", year_start = 1920)
#' head(freq)
#' @export

ngram <- function(phrases, corpus='eng_2012', year_start = 1500,
                  year_end = 2008, smoothing = 3, tag = NULL) {
  stopifnot(is.character(phrases))
  if (length(phrases) > 12){
    phrases <- phrases[1:12]
    warning("Maximum number of phrases exceeded: only using first 12.")
  }
  dfs <- lapply(corpus, function(corp) ngram_single(phrases, corpus=corp,
                                                    year_start=year_start,
                                                    year_end=year_end,
                                                    smoothing=smoothing,
                                                    tag=tag))
  result <- do.call("rbind", dfs)
  result$Corpus <- as.factor(result$Corpus)
  return(result)
}

ngram_single <- function(phrases, corpus, tag, ...){
  phrases <- phrases[1:ifelse(length(phrases) < 13, length(phrases), 12)]
  if (!is.null(tag)) {
    if (grepl("NOUN|VERB|ADJ|ADV|PRON|DET|ADP|NUM|CONJ|PRT", tag))
      phrases = paste0(phrases, "_", gsub("_", "", tag))      
    else if (grepl("ROOT|START|END", tag))
      phrases = paste(paste0("_", tag, "_"), phrases)      
  }
  corpus_n <- get_corpus(corpus)
  if (is.na(corpus_n)) {
    warning("Invalid corpus name. Defaulting to 'eng_2012'", call.=FALSE)
    corpus <- "eng_2012"
  }
  df <- ngram_fetch(phrases, corpus_n, ...)
  df$Corpus <- corpus
  return(df)
}

ngram_fetch <- function(phrases, corpus, year_start,  year_end, smoothing) {
  query <- as.list(environment())
  query$phrases <- NULL
  phrases <- phrases[phrases != ""]
  if (length(phrases)==0) stop("No valid phrases provided.")
  ng_url <- ngram_url(phrases, query)
  conn <- url(ng_url)
  html <- readLines(conn)
  close(conn)
  result <- ngram_parse(html)
  result <- reshape2::melt(result, id.vars="Year", variable.name="Phrase", value.name="Frequency")
  return(result)
}

ngram_url <- function(phrases, query=character()){
  url <- 'http://books.google.com/ngrams/graph'
  phrases <- paste(curlEscape(str_trim(phrases)), collapse='%2C')
  if (phrases=="") stop("No valid phrases provided.")
  url <- paste0(url, "?content=", phrases) 
  if (length(query) > 0) url <- modify_url(url, query=query)
  return(url)
}

ngram_parse <- function(html){
  if (any(grepl("No valid ngrams to plot!<br>", html))) stop("No valid ngrams.") 
    
  cols <- lapply(strsplit(grep("addColumn", html, value=TRUE), ","), getElement, 2)
  
  cols <- gsub(".*'(.*)'.*", "\\1", cols)
  # Clean up Unicode encoding. See discussion here:
  # http://stackoverflow.com/questions/17761858/converting-a-u-escaped-unicode-string-to-ascii
  cols <- sapply(cols, function(x) eval(parse(text=paste0("'", x, "'"))))

  html <- paste(html[-(1:grep("data.addRows\\(", html))], collapse='')
  html <- gsub("\\).*", "", html)
  
  data <- as.data.frame(t(sapply(fromJSON(html), unlist)))
  colnames(data) <- cols
  return(data)
}

get_corpus <- function(corpus){
  corpora <- c('eng_us_2012'=17, 'eng_us_2009'=5, 'eng_gb_2012'=18, 'eng_gb_2009'=6, 
           'chi_sim_2012'=23, 'chi_sim_2009'=11,'eng_2012'=15, 'eng_2009'=0,
           'eng_fiction_2012'=16, 'eng_fiction_2009'=4, 'eng_1m_2009'=1, 'fre_2012'=19, 'fre_2009'=7, 
           'ger_2012'=20, 'ger_2009'=8, 'heb_2012'=24, 'heb_2009'=9, 
           'spa_2012'=21, 'spa_2009'=10, 'rus_2012'=25, 'rus_2009'=12, 'ita_2012'=22)
  return(unname(corpora[corpus]))
}
