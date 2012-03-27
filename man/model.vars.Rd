\name{model.vars}
\alias{model.vars}
\title{extract predictor variables from a model}
\usage{
  model.vars(model)
}
\arguments{
  \item{model}{a model, typically of class \code{lm} or
  \code{glm}}
}
\value{
  a vector of variable names
}
\description{
  extract predictor variables from a model
}
\examples{
model <- lm( wage ~ poly(exper,degree=2), data=CPS )
model.vars(model)
}

