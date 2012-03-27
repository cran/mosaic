\name{prop}
\alias{count}
\alias{perc}
\alias{prop}
\title{Compute proportions, percents, or counts for a single level}
\usage{
  prop(..., level = NULL, long.names = TRUE, sep = ":",
    format = "proportion")

  count(..., format = "count")

  perc(..., format = "percent")
}
\arguments{
  \item{\dots}{arguments passed through to
  \code{\link{tally}}}

  \item{level}{the level for which counts, proportions or
  percents are calculated}

  \item{long.names}{a logical indicating whether long names
  should be when there is a conditioning variable}

  \item{sep}{a character used to separate portions of long
  names}

  \item{format}{one of \code{proportion}, \code{percent},
  or \code{count}, possibly abbrevaited}
}
\description{
  Compute proportions, percents, or counts for a single
  level
}
\examples{
prop( ~sex, data=HELPrct)
prop( ~sex, data=HELPrct, level='male')
count( ~sex | substance, data=HELPrct)
prop( ~sex | substance, data=HELPrct)
perc( ~sex | substance, data=HELPrct)
}

