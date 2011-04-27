\name{binom.test.formula}
\alias{binom.test}
\alias{binom.test.default}
\alias{binom.test.character}
\alias{binom.test.numeric}
\alias{binom.test.logical}
\alias{binom.test.factor}
\alias{binom.test.formula}
\title{
Exact Binomial Test
}
\description{
Performs an exact test of a simple null hypothesis about the probability of success in a 
Bernoulli experiment from summarized data or from raw data.
}
\usage{
binom.test(x, n, p=0.5, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95, ...)

\method{binom.test}{default}(x, n,  p = 0.5, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95, ...)

\method{binom.test}{numeric}(x, n, p = 0.5, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95, success, data.name, ...)

\method{binom.test}{character}(x, n, p = 0.5, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95, success, data.name, ...)

\method{binom.test}{factor}(x, n, p = 0.5, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95, success, data.name, ...)

\method{binom.test}{formula}(x, n , p = 0.5, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95,  success, data.name, ...)
}
\arguments{
  \item{x}{ count of successes, length 2 vector of success and failure counts, a formula,
  			or a character, numeric, or factor vector containing raw data.
}
  \item{n}{ sample size (successes + failures) or a data frame }
  \item{p}{ probability for null hypothesis }
  \item{alternative}{ type of alternative hypothesis }
  \item{conf.level}{ confidence level for confidence interval }
  \item{success}{ level of variable to be considered success.  All other levels are 
  	considered failure.}
  \item{data.name}{name for data.  If missing, this is inferred from variable names.}
  \item{\dots}{ additional arguments (often ignored) }
}
\details{
This is a wrapper around \code{\link{binom.test}} to simplify its use
when the raw data are available.
}
\value{
an \code{htest} object
}
\author{
Randall Pruim (\email{rpruim@calvin.edu})
}


\seealso{
\code{\link{binom.test}}, \code{\link{prop.test}}
}
\examples{
# Several ways to get a confidence interval for the proportion of Old Faithful
# eruptions lasting more than 3 minutes.
binom.test( faithful$eruptions > 3 )
binom.test(97,272)
binom.test(c(97,272-97))
faithful$long <- faithful$eruptions > 3
binom.test( faithful$long )
binom.test( ~long , faithful )
}
\keyword{ stats}
