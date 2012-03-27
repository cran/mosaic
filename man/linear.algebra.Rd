\docType{methods}
\name{linear.algebra}
\alias{dot}
\alias{linear.algebra}
\alias{mat}
\alias{project}
\alias{project,formula-method}
\alias{project,matrix-method}
\alias{project,numeric-method}
\alias{singvals}
\title{Functions for teaching linear algebra.}
\usage{
  project(x, u, ...)

  singvals(A, data = parent.frame())

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
  These functions are intended to support teaching basic
  linear algebra with a particular connection to
  statistics. \code{project} (preferably pronounced
  "PRO-ject" as in "projection") does either of two related
  things: (1) Given two vectors as arguments, it will
  project the first onto the second, returning the point in
  the subspace of the second that is as close as possible
  to the first vector.  (2) Given a formula as an argument,
  will work very much like \code{lm()}, constructing a
  model matrix from the right-hand side of the formula and
  projecting the vector on the left-hand side onto the
  subspace of that model matrix.  In (2), rather than
  returning the projected vector, \code{project()} returns
  the coefficients on each of the vectors in the model
  matrix. UNLIKE \code{lm()}, the intercept vector is NOT
  included by default.  If you want an intercept vector,
  include \code{+1} in your formula.

  \code{mat} returns a model matrix

  To demonstrate singularity, use \code{singvals}.
}
\examples{
a <- c(1,0,0); b <- c(1,2,3); c <- c(4,5,6); x <- rnorm(3)
dot(b,c)   # dot product
# projection onto the 1 vector gives the mean vector
mean(x)
project(x, 1)
# return the length of the vector, rather than the vector itself
project(x, 1, type='length')
# Formula interface
mat(~a+b)
mat(~a+b+1)
mat(~length+sex, data=KidsFeet)
project(a~b)
project(width~length+sex, data=KidsFeet)
project(log(width) ~ I(length^2)+sin(length)+sex, data=KidsFeet)
singvals(~length*sex*width, data=KidsFeet)
}
\seealso{
  \code{\link{linearModel}}, which returns a function.
}

