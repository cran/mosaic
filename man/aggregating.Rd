\name{aggregating}
\docType{methods}
\alias{mean}
\alias{mean-methods}
\alias{mean,ANY-method}
\alias{mean,data.frame-method}
\alias{mean,formula-method}
\alias{mean,numeric-method}

\alias{median}
\alias{median-methods}
\alias{median,ANY-method}
\alias{median,data.frame-method}
\alias{median,formula-method}
\alias{median,numeric-method}

\alias{sd}
\alias{sd-methods}
\alias{sd,ANY-method}
\alias{sd,data.frame-method}
\alias{sd,formula-method}
\alias{sd,numeric-method}

\alias{var}
\alias{var-methods}
\alias{var,ANY,ANY,ANY,ANY,ANY-method}
\alias{var,data.frame,ANY,ANY,ANY,ANY-method}
\alias{var,matrix,ANY,ANY,ANY,ANY-method}
\alias{var,formula,data.frame,ANY,ANY,missing-method}
\alias{var,formula,missing,ANY,ANY,data.frame-method}
\alias{var,numeric,ANY,ANY,ANY,ANY-method}
\alias{var,numeric,numeric,ANY,ANY,ANY-method}

\alias{max}
\alias{max-methods}
\alias{max,ANY-method}
\alias{max,data.frame-method}
\alias{max,formula-method}
\alias{max,numeric-method}

\alias{min}
\alias{min-methods}
\alias{min,ANY-method}
\alias{min,data.frame-method}
\alias{min,formula-method}
\alias{min,numeric-method}

\alias{prop}
\alias{prop-methods}
\alias{prop,ANY-method}
\alias{prop,data.frame-method}
\alias{prop,formula-method}
\alias{prop,logical-method}
\alias{prop,factor-method}

\alias{count}
\alias{count-methods}
\alias{count,ANY-method}
\alias{count,data.frame-method}
\alias{count,formula-method}
\alias{count,logical-method}
\alias{count,factor-method}

\title{
Aggregating summary statistics
}
\description{
These drop-in replacements and new summary statistics functions are 
formula-aware and allow the use of simple names within data frames.  
When given formulas, they call \code{\link{aggregate}} using the
formula.
}



\section{Methods}{
\describe{

\item{\code{signature(x = "data.frame")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "formula")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "numeric")}}{
%%  ~~describe this method here~~
}
}}

\usage{
mean(x, ..., na.rm = TRUE, trim = 0)

median(x, ..., na.rm = TRUE)

sd(x, ..., na.rm = TRUE)

var(x, y = NULL, na.rm = TRUE, use, data = NULL) 

max(x, ..., na.rm = TRUE) 

min(x, ..., na.rm = TRUE) 

prop(x, ..., level=TRUE, na.rm = TRUE) 

count(x, ..., level=TRUE, na.rm = TRUE) 

}



\arguments{
  \item{x}{ an R object, possibly a formula }
  \item{y}{ an R object, typically a numeric vector, possibly a data frame }

  \item{data}{ a data frame for the formula methods}

  \item{na.rm}{ a logical indicating whether missing data should be removed 
  before calculation.    Defaults to \code{TRUE}.
  }
  \item{trim}{ proportion of timming (each tail) for a trimmed mean}
  \item{level}{ a level of a factor }
  \item{use}{see \code{\link[stats]{var}}}
  \item{\dots}{additional arguments}
}
\details{
These methods are wrappers around functions and methods in the \code{base} and \code{stats} 
packages and provide additional interfaces.

The default value for \code{na.rm} is reversed from the functions in \code{base} and \code{stats}.
Also, \code{na.rm}, \code{use}, and \code{trim} follow \code{\dots} so must be named using
their full names.
}
\author{
Randall Pruim (\email{rpruim@calvin.edu})
}
\seealso{
\code{link{aggregate}}
}
\examples{
mean(age, data=HELPrct)
mean(~age, data=HELPrct)
mean(age ~ ., data=HELPrct)
mean(age ~ 1, data=HELPrct)
mean(age ~ NULL, data=HELPrct)
mean(HELPrct$age)
mean(age ~ sex, data=HELPrct)
mean(age ~ sex + treat, data=HELPrct)

median(age, data=HELPrct)
median(~age, data=HELPrct)
median(age ~ ., data=HELPrct)
median(age ~ 1, data=HELPrct)
median(age ~ NULL, data=HELPrct)
median(HELPrct$age)
median(age ~ sex, data=HELPrct)
median(age ~ sex + treat, data=HELPrct)

max(age, data=HELPrct)
max(~age, data=HELPrct)
max(age ~ ., data=HELPrct)
max(age ~ 1, data=HELPrct)
max(age ~ NULL, data=HELPrct)
max(HELPrct$age)
max(age ~ sex, data=HELPrct)
max(age ~ sex + treat, data=HELPrct)

sd(age, data=HELPrct)
sd(~age, data=HELPrct)
sd(age ~ ., data=HELPrct)
sd(age ~ 1, data=HELPrct)
sd(age ~ NULL, data=HELPrct)
sd(HELPrct$age)
sd(age ~ sex, data=HELPrct)
sd(age ~ sex + treat, data=HELPrct)

var(age, data=HELPrct)
var(~age, data=HELPrct)
var(age ~ ., data=HELPrct)
var(age ~ 1, data=HELPrct)
var(age ~ NULL, data=HELPrct)
var(HELPrct$age)
var(age ~ sex, data=HELPrct)
var(age ~ sex + treat, data=HELPrct)

count(sex, data=HELPrct)
count(sex, data=HELPrct, level='male')
count(HELPrct$sex)

prop(sex, data=HELPrct)
prop(sex, data=HELPrct, level='male')
prop(HELPrct$sex)
}
\keyword{ methods }
