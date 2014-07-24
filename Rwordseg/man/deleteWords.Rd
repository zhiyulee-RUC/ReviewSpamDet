\name{deleteWords}
\alias{deleteWords}
\title{Remove words into dictionary.}
\usage{
  deleteWords(strwords,
    analyzer = get("Analyzer", envir = .RwordsegEnv),
    save = FALSE)
}
\arguments{
  \item{strwords}{Vector of words.}

  \item{analyzer}{A JAVA object of analyzer.}

  \item{save}{Whether to save to dictionary.}
}
\value{
  No results.
}
\description{
  Remove words into dictionary.
}
\author{
  Jian Li <\email{rweibo@sina.com}>
}

