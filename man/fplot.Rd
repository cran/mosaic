\name{fplot}
\alias{fplot}
\alias{fplot.default}
\title{ Plot a mathematical function }

\description{ Plot a mathematical function }

\usage{
fplot(x, ...)

\method{fplot}{default} (x, xlim, ylim, n = 200, args = list(), type = "l", xlab, ylab, ...) 

}

\arguments{
	\item{x}{function-like object to be plotted.  For functions, first argument must be numeric and 
		output must be a numeric vector of the same length.
	}
	\item{xlim}{Limits on plotting window }
	\item{ylim}{Limits on plotting window }
	\item{n}{ approximate number of points to use when generating plot }
	\item{args}{ additional arguments to \code{f} } 
	\item{type}{type of plot (see \code{link{xyplot}}).  }
	\item{xlab}{x-axis label}
	\item{ylab}{y-axis label}
	\item{\dots}{
		additional arguments passed to \code{\link{xyplot}}
}
}
\details{
\code{fplot} uses a simple adaptive algorithm to sample more points
in regions where the function is changing rapidly.
}
\value{
a trellis plot produced by \code{\link{xyplot}}
}
\author{
Randall Pruim (\email{rpruim@calvin.edu})
}

\seealso{
\code{\link{curve}}, \code{\link{xyplot}}
}
\examples{
fplot(sin)
fplot(sin,xlim=c(0,4*pi))
f <- function(t) { sin(1/t)  }
fplot(f)
fplot(f,n=500)
}
\keyword{ graphics }
