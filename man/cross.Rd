\name{cross}
\alias{cross}
\title{
Factor cross products
}
\description{
Construct a product of factors.
}
\usage{
cross(..., sep = ":", drop.unused.levels = FALSE)
}
\arguments{
  \item{\dots}{ factors to be crossed.
}
  \item{sep}{ separator between levels
}
  \item{drop.unused.levels}{
  	Should levels that do not appear in cross product be dropped?
}
}
\value{
A factor
}
\author{
Randall Pruim (\email{rpruim@calvn.edu})
}


\examples{
x <- letters[1:3]
y <- c(1,2,1,1,3,1,3)
cross(x, y)
cross(x, y, drop.unused.levels=TRUE)
}
\keyword{ manipulate }
