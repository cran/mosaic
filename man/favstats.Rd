\name{favstats}
\alias{favstats}
\alias{favstats,ANY,data.frame-method}
\alias{favstats,formula,ANY-method}
\alias{favstats,matrix,ANY-method}
\alias{favstats,numeric,ANY-method}
\alias{favstats-methods}
\title{Some favorite statistical summaries}
\usage{
  favstats(x, ..., na.rm = TRUE)

  \S4method{favstats}{matrix,ANY}(x, ..., na.rm = TRUE)

  \S4method{favstats}{numeric,ANY}(x, ..., na.rm = TRUE)

  \S4method{favstats}{formula,ANY}(x, data=parent.frame(),
    ..., na.rm = TRUE)

  \S4method{favstats}{ANY,data.frame}(x,
    data=parent.frame(), ..., na.rm = TRUE)
}
\arguments{
  \item{x}{numeric vector}

  \item{data}{a data frame}

  \item{na.rm}{boolean indicating whether missing data
  should be ignored}

  \item{...}{additional arguments (currently ignored)}
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

