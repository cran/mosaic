\docType{methods}
\name{linear-algebra}
\alias{dot}
\alias{linear-algebra}
\alias{mat}
\alias{project}
\alias{project,formula-method}
\alias{project,matrix-method}
\alias{project,numeric-method}
\alias{singvals}
\title{Functions for teaching linear algebra.}
\usage{
  project(x, u, ...)

  singvals(A, data = NULL)

  dot(u, v)
}
\arguments{
  \item{A}{a formula.  In \code{mat} and \code{singvals},
  only the right-hand side is used. In \code{project}, both
  sides are used, but the left-hand side should be a single
  quantity}

  \item{x}{a numeric vector, formula, or matrix}

  \item{u}{a numeric vector}

  \item{data}{a data frame from which to pull out numerical
  values for the variables in the formula}

  \item{\dots}{additional arguments (currently ignored)}

  \item{v}{a numeric vector}
}
\value{
  \code{mat} returns a matrix

  \code{singvals} gives singular values for each column in
  the model matrix

  \code{project} returns the projection of \code{x} onto
  \code{u} (or its length if \code{u} and \code{v} are
  numeric vectors and \code{type == "length"})

  \code{dot} returns the dot product of \code{u} and
  \code{v}
}
\description{
  These functions provide a formula based interface to the
  construction of matrices from data and for fitting.  You
  can use them both for numerical vectors and for functions
  of variables in data frames.
}
\details{
  \code{mat} returns a model matrix from a formula while
  \code{project} carries out the operation of least-squares
  fitting using a singular value method. This means that
  even when the matrix is singular, a solution, either
  exact or least-squares, will be found. To demonstrate
  singularity, use \code{singvals}. NOTE: unlike the
  standard formula expansion in \code{lm}, these linear
  algebra functions do NOT include an intercept by default.
  If you want an intercept, put \code{+1} as a term in your
  formula.  (See the examples.)
}
\examples{
a <- c(1,0,0); b <- c(1,2,3); c <- c(4,5,6); x <- rnorm(3)
dot(b,c)   # dot product
# projection onto the 1 vector gives the mean vector
mean(x)
project(x, 1)
project(x, 1, type='length')
mat(~a+b)
mat(~a+b+1)
kids = fetchData("KidsFeet.csv")
mat(~length+sex,data=kids)
project(a~b)
project(width~length+sex,data=kids)
project(log(width)~I(length^2)+sin(length)+sex,data=kids)
singvals(~length*sex*width,data=kids)
}
\author{
  Daniel Kaplan (\email{kaplan@macalester.edu})
}
\seealso{
  \code{\link{linearModel}}, which returns a function.
}

