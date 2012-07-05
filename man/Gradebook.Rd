\docType{data}
\name{Gradebook}
\alias{Gradebook}
\title{A Course's Grade Book}
\format{A data frame with 105 observations on the following variables.

   \itemize{
    \item{\code{Midterm1}} {student's score on the first midterm (0-100 scale)}
    \item{\code{Midterm2}} {student's score on the second midterm (0-100 scale)}
    \item{\code{Diff_Mid}} {the difference between the two midterm exam scores (Midterm1 - Midterm2)}
    \item{\code{Extra_Credit}} {did the student turn the extra credit assignment? 0 = No, 1 = Yes}
    \item{\code{Final}} {student's score on the final}
    \item{\code{Class}} {student's class (1 = First Year, 2 = Sophomore, 3 = Junior, 4 = Senior)}
  }}
\usage{
  data(Gradebook)
}
\description{
  At the end of the semester, an "Introduction to
  Statistics" instructor wanted to gain insight into his
  students' performance by analyzing gradebook data.  The
  instructor taught 3 large lecture sections offered at
  different times during the day.  Since each section,
  depending on the time it was taught, attracted different
  types of students (in terms of major, age,
  full-time/part-time, etc.) the instructor chose a random
  sample of 35 students from each section to insure proper
  representation.
}
\references{
  Part of the Carnegie Mellon University Online Learning
  Initiative datasets.
}
\keyword{datasets}

