% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adapt_seq.R
\name{adapt_seq}
\alias{adapt_seq}
\title{Adaptively generate sequences in an interval}
\usage{
adapt_seq(
  from,
  to,
  length.out = 200,
  f = function(x, ...) {
     1
 },
  args = list(),
  quiet = FALSE
)
}
\arguments{
\item{from}{start of interval}

\item{to}{end of interval}

\item{length.out}{desired length of sequence}

\item{f}{a function}

\item{args}{arguments passed to \code{f}}

\item{quiet}{suppress warnings about NaNs, etc.}
}
\value{
a numerical vector
}
\description{
\code{adapt_seq} is similar to \code{seq} except that instead of
selecting points equally spaced along an interval, it selects points
such that the values of a function applied at those points are
(very) roughly equally spaced.  This can be useful for sampling
a function in such a way that it can be plotted more smoothly,
for example.
}
\examples{
adapt_seq(0, pi, 25, sin)

}
