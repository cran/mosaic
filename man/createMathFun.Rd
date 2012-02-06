\name{.createMathFun}
\alias{.createMathFun}
\title{Internal function for handling formula-based expressions}
\usage{
  .createMathFun(sexpr = NULL, ...)
}
\arguments{
  \item{sexpr}{a formula containing a mathematical
  expression.  The left-hand side has the expression, the
  right side contains the variables}

  \item{...}{additional assignments giving default values
  to symbolic parameters.}
}
\description{
  \code{.createMathFun} processes an expression and
  assignments to produce a function and other information
  used by D, antiD, and plotFun
}
\details{
  \code{.createMathFun} underlies user functions such as
  \code{makeFun}, \code{plotFun}, and \code{D}.  It returns
  a structure that contains a function as well as
  information about the function needed for plotting, etc.
}
\author{
  Daniel Kaplan (\email{kaplan@macalester.edu})
}

