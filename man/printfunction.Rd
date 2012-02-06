\name{print.function}
\alias{print.function}
\title{Printing Calculus Functions}
\usage{
  \method{print}{function} (x, useSource = TRUE,
    default = FALSE, ...)
}
\arguments{
  \item{x}{the function to be printed}

  \item{default}{use the standard function printing
  methods}

  \item{useSource}{passed along to the standard function
  printing methods}

  \item{\dots}{additional arguments}
}
\value{
  nothing
}
\description{
  Printing Calculus Functions
}
\details{
  The mosaic calculus operators such as \code{D} and
  \code{antiD} and \code{linearModel} sometimes create
  functions with an obscure numerical method as the
  internal contents. This print method detects such
  functions (through their \code{kind} attribute) and
  provides a more friendly display
}
\examples{
F <- antiD(sin(x^2)~x)
F
print(F)
print(F, default=TRUE)
}
\author{
  Daniel Kaplan (\email{kaplan@macalester.edu})
}

