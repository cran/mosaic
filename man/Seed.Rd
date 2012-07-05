\docType{data}
\name{Seed}
\alias{Seed}
\title{Gosset's Experiment on Regular and Kiln-Dried Seeds}
\format{A data frame with 11 observations on the following variables.

   \itemize{
    \item{\code{regular.seed}}{the corn yield (in pounds per acre) planting regular seed}
    \item{\code{kiln.dried.seed}}{the corn yield (in pounds per acre) planting kiln-dried seed}
  }}
\usage{
  data(Seed)
}
\description{
  William S. Gosset was employed by the Guinness brewing
  company of Dublin. Sample sizes available for
  experimentation in brewing were necessarily small, and
  new techniques for handling the resulting data were
  needed. Gosset consulted Karl Pearson (1857-1936) of
  University College in London, who told him that the
  current state of knowledge was unsatisfactory. Gosset
  undertook a course of study under Pearson and the outcome
  of his study was perhaps the most famous paper in
  statistical literature, "The Probable Error of a Mean"
  (1908), which introduced the t distribution. Since Gosset
  was contractually bound by Guinness, he published under a
  pseudonym, "Student," hence the t distribution is often
  referred to as Student's t distribution.
}
\details{
  As an example to illustrate his analysis, Gosset reported
  in his paper on the results of seeding 11 different plots
  of land with two different types of seed: "regular" and
  "kiln-dried". There is reason to believe that drying
  seeds before planting will increase plant yield. Since
  different plots of soil may be naturally more fertile,
  this confounding variable was eliminated by using the
  matched pairs design and planting both types of seed in
  all 11 plots.

  The hypothesis is that kiln-dried seed yields more corn
  than regular seed.
}
\references{
  Part of the Carnegie Mellon University Online Learning
  Initiative datasets.
}
\keyword{datasets}

