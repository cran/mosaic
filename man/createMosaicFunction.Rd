\name{createMosaicFunction}
\alias{createMosaicFunction}
\title{Internal function for handling formula-based expressions}
\usage{
  createMosaicFunction(sexpr = NULL, ...)
}
\arguments{
  \item{sexpr}{a formula containing a mathematical
  expression.  The left-hand side has the expression, the
  right side contains the variables}

  \item{...}{additional assignments giving default values
  to symbolic parameters.}
}
\value{
  an object that contains a function as well as information
  about the function needed for plotting, etc.
}
\description{
  \code{createMosaicFunction} processes an expression and
  assignments to produce an object containing function and
  other information used by D, antiD, and plotFun
}
\details{
  This is not actually being used and is slated for
  deprecation in the near future.

  \code{createModaicFunction} underlies was going to be
  used to to undrly user functions such as \code{makeFun},
  \code{plotFun}, and \code{D}, but it looks like we have
  another solution for this coming.
}
\author{
  Daniel Kaplan (\email{kaplan@macalester.edu})
}

