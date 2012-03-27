\name{favstats}
\alias{favstats}
\alias{favstats,ANY,data.frame-method}
\alias{favstats,formula,ANY-method}
\alias{favstats,matrix,ANY-method}
\alias{favstats,numeric,ANY-method}
\title{Some favorite statistical summaries}
\arguments{
  \item{x}{numeric vector}

  \item{na.rm}{boolean indicating whether missing data
  should be ignored}
}
\value{
  A vector of statistical summaries
}
\description{
  Computes mean, standard deviation, quartiles, sample size
  and number of missing values for a data vector.
}
\examples{
favstats(1:10)
favstats(faithful$eruptions)
}
\keyword{stats}

