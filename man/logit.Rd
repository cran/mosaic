\name{logit}
\alias{logit}
\alias{ilogit}
\title{
Logit and inverse logit functions
}
\description{
Logit and inverse logit functions
}
\usage{
logit(x)
ilogit(x)
}
\arguments{
  \item{x}{ a numeric vector }
}
\value{
For \code{logit} the value is 
\deqn{log(x/(1 - x))}{log(x/(1 - x))}

For \code{ilogit} the value is 
\deqn{exp(x)/(1 + exp(x))}
}
\author{
Randall Pruim (\email{rpruim@calvin.edu})
}

\examples{
p <- seq(.1, .9, by=.10)
l <- logit(p); l
ilogit(l)
}
