\name{interval}
\alias{interval}
\alias{pval}
\alias{pval.htest}
\alias{stat}
\alias{stat.htest}
\alias{interval.htest}

\title{Confidence Interval}
\description{
Extract a confidence intervals, test statistics or p-values from an 
\code{htest} object.
}

\usage{
interval(x, ...)

pval(x, ...)

stat(x, ...)
}

\arguments{
  \item{x}{An object of class \code{htest}.}
  \item{\dots}{Additional arguments.}
}


\author{
Randall Pruim (\email{rpruim@calvin.edu})
}


\examples{
interval(t.test(rnorm(100)))
pval(t.test(rnorm(100)))
stat(t.test(rnorm(100)))
interval(var.test(rnorm(10,sd=1), rnorm(20, sd=2)))
pval(var.test(rnorm(10,sd=1), rnorm(20, sd=2)))

stat( t.test (age ~ shuffle(sex), HELP) )
# Compare to test statistic computed with permuted values of sex.
do(10) * stat( t.test (age ~ shuffle(sex), HELP) )
}

\keyword{ univar }
\keyword{ stats }
