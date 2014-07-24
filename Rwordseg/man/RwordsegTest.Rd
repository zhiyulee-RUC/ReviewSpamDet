\name{RwordsegTest}
\alias{RwordsegTest}
\title{Run unit tests.}
\usage{
  RwordsegTest(TestPath = system.file(package = "Rwordseg", "unittests"),
    TestResult = NULL, ResultsType = c("html", "text"),
    testFileRegexp = "^runit\\..+\\.[rR]$")
}
\arguments{
  \item{TestPath}{Path of the folder which contains the
  test scripts.}

  \item{TestResult}{Name of the report file.}

  \item{ResultsType}{'html' or 'text'.}
}
\value{
  The results of function
  \code{\link{[RUnit]runTestSuite}}.
}
\description{
  Run unit tests.
}
\details{
  Run the unit tests by RUnit package, and generate a html
  or text report.
}
\examples{
\dontrun{
x <- RwordsegTest(TestResult = "Rwordseg_tests")
summary(x)
}
}
\author{
  Jian Li <\email{rweibo@sina.com}>
}
\keyword{debugging}

