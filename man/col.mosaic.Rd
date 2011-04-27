\name{col.mosaic}
\alias{col.mosaic}
\title{ Lattice Theme }
\description{ A theme for use with lattice graphics. }
\usage{
col.mosaic(bw = FALSE, lty=1:7)
}
\arguments{
  \item{bw}{whether color scheme should be "black and white"}
  \item{lty}{vector of line type codes}
}
\value{
Returns a list that can be supplied as the \code{theme} to 
\code{\link{trellis.par.set}()}.
}
\author{
Randall Pruim
}

\seealso{
\code{\link{trellis.par.set}}, 
\code{\link{show.settings}} 
}
\examples{
trellis.par.set(theme=col.mosaic(bw=TRUE))
show.settings()
trellis.par.set(theme=col.mosaic())
show.settings()
}
\keyword{ graphics }
