\name{mean}
\alias{max}
\alias{mean}
\alias{mean,ANY-method}
\alias{mean,data.frame-method}
\alias{mean,formula-method}
\alias{mean,numeric-method}
\alias{mean-methods}
\alias{median}
\alias{median,ANY-method}
\alias{median,data.frame-method}
\alias{median,formula-method}
\alias{median,numeric-method}
\alias{median-methods}
\alias{min}
\alias{sd}
\alias{sd,ANY-method}
\alias{sd,data.frame-method}
\alias{sd,formula-method}
\alias{sd,numeric-method}
\alias{sd-methods}
\alias{var}
\alias{var,ANY,ANY,ANY,ANY,ANY-method}
\alias{var,ANY,ANY,ANY,ANY,data.frame-method}
\alias{var,ANY,missing,ANY,ANY,data.frame-method}
\alias{var,data.frame,ANY,ANY,ANY,ANY-method}
\alias{var,formula,data.frame,ANY,ANY,missing-method}
\alias{var,formula,missing,ANY,ANY,data.frame-method}
\alias{var,formula,missing,ANY,ANY,missing-method}
\alias{var,matrix,ANY,ANY,ANY,ANY-method}
\alias{var,numeric,ANY,ANY,ANY,ANY-method}
\alias{var,numeric,numeric,ANY,ANY,ANY-method}
\alias{var-methods}
\title{Aggregating summary statistics}
\usage{
  mean(x, ..., na.rm=FALSE, trim=0)

  \S4method{mean}{ANY}(x, ..., na.rm=FALSE, trim=0)

  \S4method{mean}{numeric}(x, ..., na.rm=FALSE, trim=0)

  \S4method{mean}{data.frame}(x, ..., na.rm=TRUE, trim=0)

  \S4method{mean}{formula}(x, data=parent.frame(), ...,
    na.rm=TRUE, trim=0)

  median(x, ..., na.rm=FALSE)

  \S4method{median}{ANY}(x, ..., na.rm=FALSE)

  \S4method{median}{numeric}(x, ..., na.rm=FALSE)

  \S4method{median}{numeric}(x, ..., na.rm=FALSE)

  \S4method{median}{formula}(x, data=parent.frame(), ...,
    na.rm=TRUE)

  sd(x, ..., na.rm=FALSE)

  \S4method{sd}{ANY}(x, ..., na.rm=FALSE)

  \S4method{sd}{numeric}(x, ..., na.rm=FALSE)

  \S4method{sd}{data.frame}(x, ..., na.rm=TRUE)

  \S4method{sd}{formula}(x, data=parent.frame(), ...,
    na.rm=TRUE)

  var(x, y=NULL, na.rm=FALSE, use='everything', data=NULL)

  \S4method{var}{ANY,ANY,ANY,ANY,ANY}(x, y, na.rm=FALSE,
    use='everything', data=parent.frame())

  \S4method{var}{numeric,numeric,ANY,ANY,ANY}(x, y,
    na.rm=FALSE, use='everything', data=parent.frame())

  \S4method{var}{numeric,ANY,ANY,ANY,ANY}(x, y=NULL,
    na.rm=FALSE, use='everything', data=parent.frame())

  \S4method{var}{matrix,ANY,ANY,ANY,ANY}(x, y, na.rm=FALSE,
    use='everything', data=parent.frame())

  \S4method{var}{data.frame,ANY,ANY,ANY,ANY}(x, y,
    na.rm=TRUE, use='everything')

  \S4method{var}{formula,missing,ANY,ANY,missing}(x, y,
    na.rm=TRUE, use='everything', data=parent.frame())

  \S4method{var}{formula,missing,ANY,ANY,data.frame}(x, y,
    na.rm=TRUE, use='everything', data=parent.frame())

  \S4method{var}{formula,data.frame,ANY,ANY,missing}(x,
    y=parent.frame(), na.rm=FALSE, use='everything')

  \S4method{var}{ANY,missing,ANY,ANY,data.frame}(x,
    y = NULL, na.rm = FALSE, use = "everything",
    data = parent.frame())

  \S4method{var}{ANY,ANY,ANY,ANY,data.frame}(x, y = NULL,
    na.rm = FALSE, use = "everything",
    data = parent.frame())

  min(x, ..., na.rm = FALSE)

  max(x, ..., na.rm = FALSE)
}
\arguments{
  \item{x}{a vector}

  \item{na.rm}{logical indicating whether NAs should be
  removed before calculating}

  \item{\dots}{additional arguments}

  \item{trim}{a numeric indicating the proportion to be
  trimmed from each tail before calculating mean}

  \item{data}{a data frame}

  \item{y}{second vector allows for computation of
  covariances}

  \item{use}{passed along to \code{base::var}}
}
\description{
  These drop-in replacements and new summary statistics
  functions are formula-aware and allow the use of simple
  names within data frames. When given formulas, they call
  \code{\link{aggregate}} using the formula.
}
\details{
  These methods are wrappers around functions and methods
  in the \code{base} and \code{stats} packages and provide
  additional interfaces.

  The default value for \code{na.rm} is reversed from the
  functions in \code{base} and \code{stats}. Also,
  \code{na.rm}, \code{use}, and \code{trim} follow
  \code{\dots} so must be named using their full names.
}
\examples{
data(HELPrct)
mean(age, data=HELPrct)
mean(~age, data=HELPrct)
mean(age ~ 1, data=HELPrct)
mean(age ~ NULL, data=HELPrct)
mean(HELPrct$age)
mean(age ~ sex, data=HELPrct)
mean(age ~ sex & treat, data=HELPrct)
median(age, data=HELPrct)
median(~age, data=HELPrct)
median(age ~ 1, data=HELPrct)
median(age ~ NULL, data=HELPrct)
median(HELPrct$age)
median(age ~ sex, data=HELPrct)
median(age ~ sex & treat, data=HELPrct)
sd(age, data=HELPrct)
sd(~age, data=HELPrct)
sd(age ~ 1, data=HELPrct)
sd(age ~ NULL, data=HELPrct)
sd(HELPrct$age)
sd(age ~ sex, data=HELPrct)
sd(age ~ sex & treat, data=HELPrct)
var(age, data=HELPrct)
var(~age, data=HELPrct)
var(age ~ 1, data=HELPrct)
var(age ~ NULL, data=HELPrct)
var(HELPrct$age)
var(age ~ sex, data=HELPrct)
var(age ~ sex & treat, data=HELPrct)
min(age, data=HELPrct)
max(age, data=HELPrct)
max(~age, data=HELPrct)
max(age ~ 1, data=HELPrct)
max(age ~ NULL, data=HELPrct)
max(HELPrct$age)
max(age ~ sex, data=HELPrct)
max(age ~ sex & treat, data=HELPrct)
}
\seealso{
  \code{\link[stats]{aggregate}}, \code{\link[stats]{sd}},
  \code{\link[stats]{var}}, \code{\link[stats]{median}},
  \code{\link[base]{mean}}, \code{\link[base]{max}},
  \code{\link[base]{min}}, \code{\link[base]{sum}}
}
\keyword{methods}
\keyword{stats}

