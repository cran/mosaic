\name{plotFun}
\alias{plotFun}
\title{
Plotting mathematical expressions (EXPERIMENTAL)
}
\description{
Plots out mathematical functions of one or two variables.
}
\usage{
plotFun(expr, ..., add=FALSE, xlim=NULL, ylim=NULL, npts=NULL, ylab=NULL, xlab=NULL, zlab=NULL, main=NULL, lwd=1, col="black",filled=TRUE, nlevels=10, surface=FALSE, colorscheme=topo.colors,type="l",transparency=NULL)
}
\arguments{
  \item{expr}{A formula of the same sort used in \code{newD} and \code{newAntiD}.}
  \item{\dots}{Values for the variables and parameters contained in the expression.}
  \item{add}{If \code{add=TRUE}, add the new plot over the existing plot.}
  \item{xlim,ylim}{Limits for the x- and y-axes (or set using the independent variables)}
  \item{npts}{Approximately granularity of the plot.}
  \item{ylab,xlab,zlab,main}{Labels for the axes and the plot itself.}
  \item{lwd}{Line width for line plots}
  \item{col}{Color for line plots}
  \item{filled}{If \code{filled=TRUE}, then shade contour plots.}
  \item{nlevels}{Number of contour levels for contour plots.}
  \item{surface}{For functions of two variables, do a surface rather than a contour plot.}
  \item{colorscheme}{How to shade surface plots}
  \item{type}{Plotting type, e.g. "l", "p", as in \code{plot}}
  \item{transparency}{When overlaying a contour plot, the transparency of the overfill}
}
\details{
\code{plotFun} provides a way to plot out mathematical expressions.  All variables and parameters contained in the expression must be given specific numerical values.  The variables being plotted can be given a range of values.
}
\value{
A function that encapsulates the mathematical expression.
}
\author{
Daniel Kaplan (\email{kaplan@macalester.edu}) and 
}

\examples{
plotFun(A*sin(2*pi*t/P) ~ t, t=range(0,50), P=20, A=5)
f = plotFun(A*exp(-k*t)*sin(2*pi*t/P) ~ t & k, t=range(0,50), k=range(.01,.1), P=20, A=5)
% A slice of the above function
plotFun(f(t=7,A=3,P=5,k=k) ~ k, k=range(0.01,0.2))
}
\keyword{ calculus }
