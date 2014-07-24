\name{segment.options}
\alias{segment.options}
\title{Set options of segmentation.}
\usage{
  segment.options(...)
}
\arguments{
  \item{...}{any options can be defined, the same as
  \code{\link{options}}.}
}
\value{
  No results.
}
\description{
  Set options of segmentation.
}
\details{
  The are 3 built-in options can be used for segmentation.
  Each of them has logical value. \code{isNameRecognition}
  means whether to recognize person name automatically,
  default is FALSE. \code{isNumRecognition} means whether
  to recognize numbers automatically, default is TRUE.
  \code{isQuantifierRecognition} means whether to recognize
  quantifier automatically, default is TRUE.
}
\author{
  Jian Li <\email{rweibo@sina.com}>
}

