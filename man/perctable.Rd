\name{perctable}
\alias{perctable}
\alias{proptable}
\title{
Cross tabulation displayed as percents or proportions
}
\description{
\code{perctable} and \code{proptable} use the cross-classifying factors to build a 
contingency table of the percents or proportions at each combination of factor levels.
}
\usage{
perctable(...)

proptable(...)
}
\arguments{
  \item{...}{ 
  arguments passed directly to \code{\link{table}};  typically
  one or more objects which can be interpreted as factors (including character strings), 
  or a list (or data frame) whose components can be so interpreted. 
}
}
\details{
See \code{\link{table}}.
}
\value{
returns a contingency table, an object of class "table", an array of percentage or proportion
values. Note that unlike S the result is always an array, a 1D array if one factor is given.
}
\author{
Randall Pruim (\email{rpruim@calvin.edu})
}
\examples{
perctable(rbinom(1000,10,.5))
with(airquality,
   perctable(OzHi=Ozone > 80, Month, useNA="ifany"))
with(airquality,
   perctable(OzHi=Ozone > 80, Month, useNA="always"))
}
\keyword{ manipulate }
