\name{tally}
\alias{tally}
\alias{tally,ANY-method}
\alias{tally,formula-method}
\title{Tabulate categorical data}
\arguments{
  \item{formula}{a formula describing the type of table
  desired}

  \item{data}{a data frame or environment in which
  evaluation occurs}

  \item{format}{a character string describing the desired
  format of the results.  One of \code{'default'},
  \code{'count'}, \code{'proportion'}, or \code{'percent'}.
  In case of \code{'default'}, counts are used unless there
  is a condition, in which case proportions are used
  instead.}

  \item{subset}{an expression evaluating to a logical
  vector used to select a subset of \code{data}}

  \item{quiet}{a logical indicating whether messages about
  order in which marginal distributions are calculated
  should be surpressed.  See \code{\link{addmargins}}.}

  \item{margins}{a logical indicating whether marginal
  distributions should be displayed.}

  \item{formula}{a formula}

  \item{data}{a data frame}

  \item{format}{one of \code{default}, \code{count},
  \code{proportion}, or \code{percent} describing the
  format the tallies should be returned in.}

  \item{margins}{a logical indicating whether margins
  tallies should be added.}

  \item{quiet}{a logical indicating whether tallying should
  be done quietly (vs. verbosely)}

  \item{subset}{an expression defining a subset of the data
  frame to be tallied.}
}
\description{
  Tabulate categorical data
}
\examples{
tally( ~ substance, HELPrct)
tally( ~ substance & sex , HELPrct)
tally( sex ~ substance, HELPrct)   # equivalent to tally( ~ sex | substance, ... )
tally( ~ substance | sex , HELPrct)
tally( ~ substance | sex , HELPrct, format='count')
tally( ~ substance & sex , HELPrct, format='percent')
}

