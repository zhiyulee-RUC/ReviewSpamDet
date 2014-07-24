\name{insertWords}
\alias{insertWords}
\title{Insert new words into dictionary.}
\usage{
  insertWords(strwords,
    analyzer = get("Analyzer", envir = .RwordsegEnv),
    strtype = rep("userDefine", length(strwords)),
    numfreq = rep(1000, length(strwords)), save = FALSE)
}
\arguments{
  \item{strwords}{Vector of words.}

  \item{analyzer}{A JAVA object of analyzer.}

  \item{strtype}{The type of the nature of word.}

  \item{numfreq}{The frequency of the word.}

  \item{save}{Whether to save to dictionary.}
}
\value{
  No results.
}
\description{
  Insert new words into dictionary.
}
\author{
  Jian Li <\email{rweibo@sina.com}>
}

