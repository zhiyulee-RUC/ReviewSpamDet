\name{segmentCN}
\alias{segmentCN}
\title{Sengment a sentence.}
\usage{
  segmentCN(strwords,
    analyzer = get("Analyzer", envir = .RwordsegEnv),
    nature = FALSE, nosymbol = TRUE,
    returnType = c("vector", "tm"), isfast = FALSE,
    outfile = "", blocklines = 1000)
}
\arguments{
  \item{strwords}{A Chinese sentence in UTF-8 or the path
  of a text file.}

  \item{analyzer}{A JAVA object of analyzer.}

  \item{nature}{Whether to recognise the nature of the
  words.}

  \item{nosymbol}{Whether to keep symbols in the sentence.}

  \item{returnType}{Default is a string vector but we also
  can choose 'tm' to output a single string separated by
  space so that it can be used by \code{\link[tm]{Corpus}}
  directly.}

  \item{isfast}{Whether to run the fast analyzer.}

  \item{outfile}{The path of output if strwords is a file.}

  \item{blocklines}{The (maximal) number of lines to read
  at one time when strwords is a file.}
}
\value{
  a vector of words (list if input is vecter) which have
  been segmented or the path of output file.
}
\description{
  A function segment Chinese sentence into words.
}
\examples{
\dontrun{
segmentCN("hello world!")
}
}
\author{
  Jian Li <\email{rweibo@sina.com}>
}

