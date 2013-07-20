#' Get n-gram frequencies (case insensitive version)
#'
#' @param phrases vector of phrases
#' @param aggregate sum up each of the terms
#' @param ... remaining parameters passed to ngram
#' @export
     
ngrami <- function(phrases, aggregate=TRUE, ...){
  phrases <- sapply(phrases, function(x) paste0(toupper(substr(x, 1, 1)),
                                                tolower(substring(x, 2)))) 
  phrases <- c(phrases, tolower(phrases), toupper(phrases))
  phrases <- unique(phrases)
  result <- ngram(phrases, ...)
  if (aggregate){
    result$Phrase <- tolower(result$Phrase)
    result <- ddply(result, .(Year, Corpus, Phrase), summarise, Frequency = sum(Frequency))
    result$Phrase <- factor(result$Phrase)
  }
  return(result)
}