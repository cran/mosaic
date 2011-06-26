\name{pqrdata}
\alias{pdata}
\alias{qdata}
\alias{rdata}
\alias{ddata}
\title{ The Data Distribution
}
\description{
Density, distribution function, quantile function, and random generation
from data.
}

\usage{
rdata(n, vals, replace = TRUE, data=NULL, ...)
pdata(q, vals, lower.tail=TRUE,data=NULL, ...) 
qdata(p, vals, data=NULL, ...)
ddata(x, vals, log=FALSE, data=NULL, ...)
}

\arguments{
\item{x,q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of observations. If length(n) > 1, the length is taken to be the number required.}
\item{vals}{data values.}
\item{replace}{logical; if TRUE, sample with replacement.}
\item{log}{logical; if TRUE (default), probabilities p are given as log(p).}
\item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x] otherwise, P[X > x].}
\item{data}{Handles the \code{data=} style of referring to variables in data frames.}
\item{\dots}{additional arguments.}
}


\value{
\code{ddata} gives the (log) density, \code{pdata} gives the distribution function, 
\code{qdata} gives the quantile function, and \code{rdata} generates random deviates
treating \code{vals} as an enumeration of the population.
For \code{rdata}, the sampling can be either with or without replacement.
}

\examples{
data(iris)
rdata(10,iris$Species)
rdata(10, Species, data=iris)
ddata('setosa',iris$Species)
ddata('setosa', Species, data=iris)
pdata(3:6, iris$Sepal.Length)
pdata(3:6, Sepal.Length, data=iris)
qdata(.5, iris$Sepal.Length)
qdata(.5, Sepal.Length, data=iris)
}

\keyword{ distribution }
