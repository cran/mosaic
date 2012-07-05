\name{findZeros}
\alias{findZeros}
\title{Find the zeros of a function}
\usage{
  findZeros(expr, ...,
    xlim = c(near - within, near + within), near = 0,
    within = Inf, nearest = 10, npts = 1000, iterate = 1)
}
\arguments{
  \item{expr}{A formula.  The right side names the variable
  with respect to which the zeros should be found. The left
  side is an expression, e.g. \code{sin(x) ~ x}. All free
  variables (all but the variable on the right side) named
  in the expression must be assigned a value via
  \code{\ldots}}

  \item{\dots}{Specific numerical values for the free
  variables in the expression.}

  \item{xlim}{The range of the dependent variable to search
  for zeros. \code{Inf} is a legitimate value, but is
  interpreted in the numerical sense as the non-Inf largest
  floating point number.  This can also be specified
  replacing \code{x} with the name of the variable.  See
  the examples.}

  \item{near}{a value near which zeros are desired}

  \item{within}{only look for zeros at least this close to
  near.  \code{near} and \code{within} provide an
  alternative to using \code{xlim} to specify the search
  space.}

  \item{nearest}{the number of nearest zeros to return.
  Fewer are returned if fewer are found.}

  \item{iterate}{maximum number of times to iterate the
  search. Subsequent searches take place with the range of
  previously found zeros.  Choosing a large number here is
  likely to kill performance without improving results, but
  a value of 1 (the default) or 2 works well when searching
  in \code{c(-Inf,Inf)} for a modest number of zeros near
  \code{near}.}

  \item{npts}{How many sub-intervals to divide the
  \code{xlim} into when looking for candidates for zeros.
  The default is usually good enough. If \code{Inf} is
  involved, the intervals are logarithmically spaced up to
  the largest finite floating point number. There is no
  guarantee that all the roots will be found.}
}
\value{
  A set of zero or more numerical values.  Plugging these
  into the expression on the left side of the formula
  should result in values near zero.
}
\description{
  Compute numerically the zeros of a function.
}
\details{
  Searches numerically using \code{uniroot}.
}
\examples{
findZeros( sin(t) ~ t, xlim=c(-10,10) )
# Can use tlim or t.lim instead of xlim if we prefer
findZeros( sin(t) ~ t, tlim=c(-10,10) )
findZeros( sin(theta) ~ theta, near=0, nearest=20)
findZeros( A*sin(2*pi*t/P) ~ t, xlim=c(0,100), P=50, A=2)
# Interval of a normal at half its maximum height.
findZeros( dnorm(x,mean=0,sd=10) - 0.5*dnorm(0,mean=0,sd=10) ~ x )
# A pathological example
# There are no "neareset" zeros for this function.  Each iteration finds new zeros.
f <- function(x) { if (x==0) 0 else sin(1/x) }
findZeros( f(x) ~ x, near=0 )
# Better to look nearer to 0
findZeros( f(x) ~ x, near=0, within=100 )
findZeros( f(x) ~ x, near=0, within=100, iterate=0 )
findZeros( f(x) ~ x, near=0, within=100, iterate=3 )
}
\author{
  Daniel Kaplan (\email{kaplan@macalester.edu})
}
\keyword{calculus}

