\name{HELP0}
\alias{HELP0}
\docType{data}
\title{Health Evaluation and Linkage to Primary Care}
\description{
The HELP study was a clinical trial for adult inpatients recruited from a
detoxification unit.  Patients with no primary care physician were randomized
to receive a multidisciplinary assessment and a brief motivational intervention
or usual care, with the goal of linking them to primary medical care.
}
\usage{
data(HELP0)
}
\format{
  Data frame with 453 observations on the following variables.
  
  \describe{
    \item{\code{age}}{subject age (in years)}
    \item{\code{anysubstatus}}{a numeric vector}
    \item{\code{cesd}}{Center for Epidemiologic Studies Depression measurements}
    \item{\code{d1}}{a numeric vector}
    \item{\code{daysanysub}}{a numeric vector}
    \item{\code{dayslink}}{a numeric vector}
    \item{\code{drugrisk}}{a numeric vector}
    \item{\code{e2b}}{a numeric vector}
    \item{\code{female}}{0 for male, 1 for female} 
    \item{\code{g1b}}{a factor with levels \code{no} \code{yes}}
    \item{\code{homeless}}{a factor with levels \code{housed} \code{homeless}}
    \item{\code{i1}}{a numeric vector}
    \item{\code{i2}}{a numeric vector}
    \item{\code{id}}{subject identifier}
    \item{\code{indtot}}{a numeric vector}
    \item{\code{linkstatus}}{a numeric vector} 
    \item{\code{mcs}}{a numeric vector}
    \item{\code{pcs}}{a numeric vector}
    \item{\code{pss_fr}}{a numeric vector}
    \item{\code{racegrp}}{a factor with levels \code{black} \code{hispanic} \code{other} \code{white}}
    \item{\code{satreat}}{a factor with levels \code{no} \code{yes}}
    \item{\code{sexrisk}}{a numeric vector}
    \item{\code{substance}}{a factor with levels \code{alcohol} \code{cocaine} \code{heroin}}
    \item{\code{treat}}{a factor with levels \code{no} \code{yes}}
  }
}

\details{
Eligible subjects were adults, who spoke Spanish or English, reported alcohol,
heroin or cocaine as their first or second drug of choice, resided in proximity
to the primary care clinic to which they would be referred or were homeless.
Patients with established primary care relationships they planned to continue,
significant dementia, specific plans to leave the Boston area that would
prevent research participation, failure to provide contact information for
tracking purposes, or pregnancy were excluded.

Subjects were interviewed at baseline during their detoxification
stay and follow-up interviews were undertaken every 6 months for 2
years.  A variety of continuous, count, discrete, and survival time
predictors and outcomes were collected at each of these five
occasions.
}
\source{
\url{http://www.math.smith.edu/help}
}
\references{
Samet JH, Larson MJ, Horton NJ, Doyle K, Winter M, and Saitz R.  Linking alcohol and 
drug-dependent adults to primary medical care: 
A randomized controlled trial of a multi-disciplinary health intervention in a detoxification unit.  {Addiction}, 2003; 98(4):509-516.
}
\examples{
data(HELP0)
}
\keyword{datasets}
