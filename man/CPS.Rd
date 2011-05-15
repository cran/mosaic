\name{CPS}
\alias{CPS}
\docType{data}
\title{
Data from the Current Population Survey (CPS)
}
\description{
The Current Population Survey (CPS) is used to supplement census
information between census years. These data consist of a random
sample of persons from the CPS, with information on wages and
other characteristics of the workers, including sex, number of years
of education, years of work experience, occupational status, region of
residence and union membership.  
}
\usage{data(CPS)}
\format{
  A data frame with 534 observations on the following variables.
  \describe{
    \item{\code{wage}}{wage (US dollars per hour)}
    \item{\code{educ}}{number of years of education}
    \item{\code{race}}{a factor with levels \code{NW} (nonwhite) or \code{W} (white)}
    \item{\code{sex}}{a factor with levels \code{F} \code{M}}
    \item{\code{hispanic}}{a factor with levels \code{Hisp} \code{NH}}
    \item{\code{south}}{a factor with levels \code{NS} \code{S}}
    \item{\code{married}}{a factor with levels \code{Married} \code{Single}}
    \item{\code{exper}}{number of years of work experience (inferred from \code{sex} and
\code{educ})}
    \item{\code{union}}{a factor with levels \code{Not} \code{Union}}
    \item{\code{age}}{age in years}
    \item{\code{sector}}{a factor with levels \code{clerical} \code{const} \code{manag} \code{manuf} \code{other} \code{prof} \code{sales} \code{service}}
  }
}
\details{
Data are from 1985.
The data file is recoded from the original, which had
entirely numerical codes.  
}
\source{
Data are from \url{http://lib.stat.cmu.edu/datasets/CPS_85_Wages}.
}
\references{
Berndt, ER. \emph{The Practice of Econometrics} 1991. Addison-Wesley. 
}
\examples{
data(CPS)
}
\keyword{datasets}
