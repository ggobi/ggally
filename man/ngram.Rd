\name{ngram}
\alias{ngram}
\title{Get n-gram frequencies}
\usage{
  ngram(phrases, corpus = "eng_2012", year_start = 1500,
    year_end = 2008, smoothing = 3, tag = NULL)
}
\arguments{
  \item{phrases}{vector of phrases, with a maximum of 12
  items}

  \item{corpus}{Google corpus to search (see Details for
  possible values)}

  \item{year_start}{start year, default is 1500}

  \item{year_end}{end year, default is 2008}

  \item{tag}{apply a part-of-speech tag to the whole vector
  of phrases}

  \item{smoothing}{smoothing paramater, default is 3}
}
\description{
  \code{ngram} downloads data from the Google Ngram Viewer
  website and returns it in a dataframe.
}
\details{
  Google generated two datasets drawn from digitised books
  in the Google Books collection. One was generated in July
  2009, the second in July 2012.  Google will update these
  datasets as book scanning continues.

  This function provides the annual frequency of words or
  phrases, known as n-grams, in a sub-collection or
  "corpus" taken from the Google Books collection.  The
  search across the corpus is case-sensitive. For a
  case-insensitive search use \code{\link{ngrami}}.

  Below is a list of available corpora. \tabular{ll}{
  \bold{Corpus} \tab \bold{Corpus Name}\cr eng_us_2012\tab
  American English 2012\cr eng_us_2009\tab American English
  2009\cr eng_gb_2012\tab British English 2012\cr
  eng_gb_2009\tab British English 2009\cr chi_sim_2012\tab
  Chinese 2012\cr chi_sim_2009\tab Chinese 2009\cr
  eng_2012\tab English 2012\cr eng_2009\tab English 2009\cr
  eng_fiction_2012\tab English Fiction 2012\cr
  eng_fiction_2009\tab English Fiction 2009\cr
  eng_1m_2009\tab Google One Million\cr fre_2012\tab French
  2012\cr fre_2009\tab French 2009\cr ger_2012\tab German
  2012\cr ger_2009\tab German 2009\cr heb_2012\tab Hebrew
  2012\cr heb_2009\tab Hebrew 2009\cr spa_2012\tab Spanish
  2012\cr spa_2009\tab Spanish 2009\cr rus_2012\tab Russian
  2012\cr rus_2009\tab Russian 2009\cr ita_2012\tab Italian
  2012\cr }

  The Google Million is a sub-collection of Google Books.
  All are in English with dates ranging from 1500 to 2008.
  No more than about 6,000 books were chosen from any one
  year, which means that all of the scanned books from
  early years are present, and books from later years are
  randomly sampled. The random samplings reflect the
  subject distributions for the year (so there are more
  computer books in 2000 than 1980).

  See \url{http://books.google.com/ngrams/info} for the
  full Ngram syntax.
}
\examples{
freq <- ngram(c("mouse", "rat"), year_start = 1950)
head(freq)
freq <- ngram(c("blue", "red"), tag = "ADJ")
head(freq)
freq <- ngram(c("President Roosevelt", "President Truman"), tag = "START", year_start = 1920)
head(freq)
}

