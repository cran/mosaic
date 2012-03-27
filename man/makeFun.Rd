\name{makeFun}
\alias{makeFun}
\alias{makeFun,formula-method}
\alias{makeFun,lm-method}
\title{Create a function from a formula}
\arguments{
  \item{object}{an object from which to create a function.
  This should generally be specified without naming.}

  \item{\dots}{additional arguments in the form \code{var =
  val} that set default values for the inputs to the
  function.}

  \item{strict.declaration}{if \code{TRUE} (the default),
  an error is thrown if default values are given for
  variables not appearing in the \code{object} formula.}
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
  inputs to the function. The inputs to the function are
  all variables appearing on either the left or right sides
  of the formula.  Those appearing in the right side will
  occur in the order specified.  Those not appearing in the
  right side will appear in an unspecified order.
}
\examples{
f <- makeFun( sin(x^2 * b) ~ x & y & a); f
g <- makeFun( sin(x^2 * b) ~ x & y & a, a=2 ); g
h <- makeFun( a * sin(x^2 * b) ~ b & y, a=2, y=3); h
model <- lm(wage ~ poly(exper,degree=2), data=CPS)
fit <- makeFun(model)
xyplot(wage ~ exper, data=CPS)
plotFun(fit(exper) ~ exper, add=TRUE)
}

