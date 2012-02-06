\name{bargraph}
\alias{bargraph}
\title{Create bar graphs from raw data}
\usage{
  bargraph(x, data, groups, horizontal = FALSE, origin = 0,
    ...)
}
\arguments{
  \item{x}{a formula describing the plot}

  \item{data}{a data frame in which the formula \code{x} is
  evaluated}

  \item{groups}{a variable or expression used for grouping.
  See \code{\link[lattice]{barchart}}.}

  \item{horizontal}{a logical indicating whether bars
  should be horizontal}

  \item{\dots}{additional arguments passed through to
  \code{\link[lattice]{barchart}}}

  \item{origin}{beginning point for bars.  For the default
  behavior used by \code{\link[lattice]{barchart}} set
  \code{origin} to \code{NULL}, but 0 is a better default,
  and if 0 is not good, perhaps you should use a different
  kind of plot anyway.}
}
\value{
  a trellis object describing the plot
}
\description{
  \code{\link[lattice]{barchart}} from the \code{lattice}
  package makes bar graphs from pre-tabulated data.  Raw
  data can be tabulated using \code{\link{xtabs}}, but the
  syntax is unusual compared to the other lattice plotting
  functions.  \code{bargraph} provides an interface that is
  consistent with the other \code{lattice} functions.
}
\note{
  The current implementation may not work if one of the
  variables used in the plot is called \code{Freq}, since
  that variable is created by an underlying call to
  \code{\link{xtabs}}.
}
\examples{
bargraph( ~ substance, data=HELPrct)
bargraph( ~ substance, data=HELPrct, horizontal=TRUE)
bargraph( ~ substance | sex, groups=homeless, data=HELPrct)
}
\seealso{
  \code{link[lattice]{barchart}}
}

