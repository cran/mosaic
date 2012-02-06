\name{nlsModel}
\alias{nlsModel}
\title{Fit a nonlinear least squares model}
\usage{
  nlsModel(formula, data, ..., options)
}
\arguments{
  \item{formula}{formula specifying the model}

  \item{data}{dataframe containing the data to be used}

  \item{...}{named initial values for parameters}

  \item{options}{not yet used}
}
\value{
  a function
}
\description{
  Allows you to specify a formula with parameters, along
  with starting guesses for the parameters.  Refines those
  guesses to find the least-squares fit.
}
\details{
  Fits a nonlinear least squares model to data.  In
  contrast to linear models, all the parameters (including
  linear ones) need to be named in the formula.  The
  function returned simply contains the formula together
  with pre-assigned arguments setting the parameter value.
  Variables used in the fitting (as opposed to parameters)
  are unassigned arguments to the returned function.
}
\note{
  This doesn't work with categorical explanatory variables.
}
\examples{
stan <- fetchData("stan-data.csv")
f <- nlsModel(temp ~ A+B*exp(-k*time), data=stan,A=50,B=50,k=1/20)
f(time=50)
}
\author{
  Daniel Kaplan (\email{kaplan@macalester.edu})
}
\seealso{
  \code{\link{linearModel}}, \code{\link{nls}}
}

