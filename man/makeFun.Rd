\name{makeFun}
\alias{makeFun}
\title{Utilities for creating mathematical functions from expressions}
\usage{
  makeFun(sexpr = NULL, ...)
}
\arguments{
  \item{sexpr}{the mathematical expression written as a
  formula}

  \item{...}{additional arguments setting default numerical
  values for arguments or parameters}
}
\value{
  a function
}
\description{
  \code{makeFun} turns a mathematical expression (written
  as a formula) into the corresponding R function.  It's
  purely a convenience that uses the same expression syntax
  as \code{plotFun}, \code{D} and \code{antiD}.
}
\examples{
f = makeFun(sin(x^2) ~ x)
g = makeFun(a*x + b ~ x,a=2,b=4)
g(x=10)
g(x=10,a=5,b=2)
h = makeFun(sin(x)*y ~ x&y)
}
\author{
  Daniel Kaplan (\email{kaplan@macalester.edu})
}

