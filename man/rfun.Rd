\name{rfun}
\alias{rfun}
\alias{rpoly2}
\title{Generate a natural-looking function}
\usage{
  rfun(vars = ~x & y, seed = NULL, n = 0)

  rpoly2(seed = NULL)
}
\arguments{
  \item{vars}{a formula; the LHS is empty and the RHS
  indicates the variables used for input to the function
  (separated by &)}

  \item{seed}{seed for random number generator, passed to
  \code{\link{set.seed}}.}

  \item{n}{the number of gaussians.  By default, this will
  be selected randomly.}
}
\value{
  a function with the appropriate number of inputs

  a function defined by a 2nd degree polynomial of 2
  variables with coefficients selected randomly according
  to a Unif(-1,1) distribution.
}
\description{
  Produce a random function that is sum of gaussians

  \code{rpoly2} generates a random 2nd degree poly of 2
  vars (as a function)
}
\details{
  \code{rfun} is an easy way to generate a natural-looking
  but random function with ups and downs much as you might
  draw on paper.  In two variables, a good way to produce a
  random landscape that is smooth. Things happen in the
  domain -5 to 5. The function is pretty flat outside of
  that. Use \code{seed} to create a fixed function that
  will be the same for everybody
}
\examples{
f <- rfun( ~ u & v)
plotFun(f(u,v)~u&v,u=range(-5,5),v=range(-5,5))
myfun <- rfun(~ u & v, seed=1959)
}
\author{
  Daniel Kaplan (\email{kaplan@macalester.edu})
}
\keyword{random}

