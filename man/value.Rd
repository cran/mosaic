% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/value.R
\name{value}
\alias{value}
\alias{value.integrate}
\alias{value.default}
\title{Extract value from an object}
\usage{
value(object, ...)

\method{value}{integrate}(object, ...)

\method{value}{default}(object, ...)
}
\arguments{
\item{object}{an object from which a "value" is to be extracted.}

\item{...}{additional arguments (currently ignored).}
}
\description{
Functions like \code{\link[=integrate]{integrate()}} and \code{\link[=nlm]{nlm()}} return objects that contain more
information that simply the value of the integration or optimization.  \code{value()} extracts
the primary value from such objects.  Currently implemented situations include the output from
\code{\link[=integrate]{integrate()}},
\code{\link[=nlm]{nlm()}},
\code{\link[cubature:hcubature]{cubature::adaptIntegrate()}}, and
\code{\link[=uniroot]{uniroot()}}.
}
\examples{
integrate(sin, 0, 1) |> value()
nlm(cos, p = 0) |> value()
uniroot(cos, c(0, 2)) |> value()
}
