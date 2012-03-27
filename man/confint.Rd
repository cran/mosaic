\name{confint.numeric}
\alias{confint.data.frame}
\alias{confint.numeric}
\title{Confidence interval methods for output of resampling}
\usage{
  \method{confint}{numeric} (object, parm, level = 0.95,
    ..., method = c("stderr", "quantile"), margin = FALSE)

  \method{confint}{data.frame} (object, parm, level = 0.95,
    ..., method = c("stderr", "quantile"), margin = FALSE)
}
\arguments{
  \item{object}{The data frame or numerical vector.}

  \item{parm}{not used -- for compatibility with other
  confint methods}

  \item{level}{confidence level (default 0.95)}

  \item{\dots}{additional arguments (currently ignored)}

  \item{method}{either "stderr" (default) or "quantile"}

  \item{margin}{if true, report intervals as a center and
  margin of error.}
}
\value{
  When applied to a data frame, returns a data frame giving
  the confidence interval for each variable in the data
  frame.  When applied to a numerical vector, returns a
  vector.
}
\description{
  Methods for \code{confint} to compute confidence
  intervals on numerical vectors and numerical components
  of data frames.
}
\examples{
s <- do(500)*mean( age ~ sex, data=resample(HELPrct) )
confint(s)
confint(s, method="quantile")
confint(s, margin=TRUE)
confint(s, margin=TRUE, level=0.99 )
s2 <- do(500)*mean( resample(1:10) )
confint(s2)
}

