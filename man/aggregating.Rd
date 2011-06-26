\name{aggregating}
\alias{mean}
\alias{mean.default}
\alias{mean.factor}
\alias{mean.formula}
\alias{median}
\alias{median.default}
\alias{median.factor}
\alias{median.formula}
\alias{sd}
\alias{sd.formula}
\alias{sd.default}
\alias{sd.factor}
\alias{var}
\alias{var.formula}
\alias{var.default}
\alias{var.factor}
\alias{IQR}
\alias{IQR.formula}
\alias{IQR.default}
\alias{IQR.factor}
\alias{prop}
\alias{prop.logical}
\alias{prop.factor}
\alias{prop.formula}
\alias{prop.default}
\alias{count}
\alias{count.logical}
\alias{count.factor}
\alias{count.formula}
\alias{count.default}

\title{
Aggregating summary statistics
}
\description{
These drop-in replacements and new summary statistics functions are 
formula-aware and allow the use of simple names within data frames.  
When given formulas, they call \code{\link{aggregate}} using the
formula.
}
\usage{
mean(x, ..., na.rm=TRUE)
\method{mean}{default}(x, ..., na.rm=TRUE)
\method{mean}{formula}(x, data, ...,  na.rm=TRUE)
\method{mean}{factor}(x,  ..., na.rm=TRUE)

median(x, ..., na.rm=TRUE)
\method{median}{default}(x, ..., na.rm=TRUE)
\method{median}{formula}(x, data, ...,  na.rm=TRUE)
\method{median}{factor}(x, ..., na.rm=TRUE)

sd(x, data=NULL, ...)
\method{sd}{formula}(x, data, na.rm=TRUE, ...)
\method{sd}{default}(x, na.rm=TRUE, ...)
\method{sd}{factor}(x, na.rm=TRUE, ...)

var(x, data=NULL, ...)
\method{var}{formula}(x, data, na.rm=TRUE, ...)
\method{var}{default}(x, na.rm=TRUE, ...)
\method{var}{factor}(x, na.rm=TRUE, ...)

IQR(x, data=NULL, ...)
\method{IQR}{formula}(x, data, na.rm=TRUE, ...)
\method{IQR}{default}(x, na.rm=TRUE, ...)
\method{IQR}{factor}(x, na.rm=TRUE, ...)

prop(x, data=NULL, ...)
\method{prop}{logical}(x, level=TRUE, na.rm=TRUE, ...)
\method{prop}{factor}(x, level=levels(x)[1], na.rm=TRUE, ...)
\method{prop}{formula}(x, data, na.rm=TRUE, ...)
\method{prop}{default}(x, na.rm=TRUE, ...)

count(x, data=NULL, ...)
\method{count}{logical}(x, level=TRUE, na.rm=TRUE, ...)
\method{count}{factor}(x, level=levels(x)[1], na.rm=TRUE, ...)
\method{count}{formula}(x, data, na.rm=TRUE, ...)
\method{count}{default}(x, na.rm=TRUE, ...)
}

\arguments{
  \item{x}{ an R object, possibly a formula }

  \item{data}{ a data frame for the formula methods}

  \item{na.rm}{ a logical indicating whether missing data should be removed 
  before calculation.  
  }
  \item{level}{ a level of a factor }
  \item{\dots}{additional arguments}
}
\details{
The default value for \code{na.rm} is reversed from the functions in \code{stats}.
}
\author{
Randall Pruim (\email{rpruim@calvin.edu})
}
\seealso{
\code{link{aggregate}}
}
\examples{
sd(age, data=HELP)
sd(~age, data=HELP)
sd(age ~ ., data=HELP)
sd(age ~ 1, data=HELP)
sd(age ~ NULL, data=HELP)
sd(HELP$age)
sd(age ~ sex, data=HELP)
sd( age ~ sex + treat, data=HELP)
sd(age ~ sex, data=HELP, overall=TRUE )
mosaic.par.set(aggregate.overall=TRUE)
sd(age ~ sex, data=HELP)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
