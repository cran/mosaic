\name{makeFunction}
\alias{makeFunction}
\alias{makeFunction,formula-method}
\title{Create a function from a formula}
\arguments{
  \item{object}{an object from which to create a function.
  This should generally be specified without naming.}

  \item{\dots}{additional arguments in the form \code{var =
  val} that set default values for the inputs to the
  function.}
}
\value{
  a function
}
\description{
  Provides an easy mechanism for creating simple
  "mathematical" functions via a formula interface.
}
\details{
  The definition of the function is given by the left side
  of a formula.  The right side lists at least one of the
  inputs to the function. The inptus to the function are
  all variables appearing on either the left or right sides
  of the formula.  Those appearing in the right side will
  occur in the order specified.  Those not appearing in the
  right side will appear in an unspecified order.
}
\examples{
f <- makeFunction( sin(x^2 * b) ~ x & y & a); f
g <- makeFunction( sin(x^2 * b) ~ x & y & a, a=2 ); g
h <- makeFunction( a * sin(x^2 * b) ~ b & y, a=2, y=3); h
}

