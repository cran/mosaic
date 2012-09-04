\docType{data}
\name{Depression}
\alias{Depression}
\title{Clinical Depression and Recurrence}
\format{A data frame with 109 observations on the following variables.

   \itemize{
    \item{\code{Hospt}} {the hospital of patient, represented by a code for each of the 5 hospitals (1, 2, 3, 5, or 6)}
    \item{\code{Treat}} {the treatment received by the patient (0=Lithium, 1=Imipramine, or 2=Placebo)}
    \item{\code{Outcome}} {whether or not a recurrence occurred during the treatment of patient (0 = No Recurrence, 1 = Recurrence)}
    \item{\code{Time}} {either the time in days till the first recurrence, or if a recurrence did not occur, the length (in days) of the patient's participation in the study}
    \item{\code{AcuteT}} {the time (in days) that the patient was depressed prior to the study}
    \item{\code{Age}} {the age of the patient (in years), when the patient entered the study}
    \item{\code{Gender}} {the gender of patient (\code{Female} or \code{Male})}
  }}
\usage{
  data(Depression)
}
\description{
  In the study conducted by the National Institutes of
  Health, 109 clinically depressed patients were separated
  into three groups, and each group was given one of two
  active drugs (imipramine or lithium) or no drug at all.
  For each patient, the dataset contains the treatment
  used, the outcome of the treatment, and several other
  interesting characteristics.
}
\examples{
favstats(Age ~ Gender, Depression)
tally(~ Outcome | Treat, Depression)
}
\references{
  Part of the Carnegie Mellon University Online Learning
  Initiative datasets.
}
\keyword{datasets}

