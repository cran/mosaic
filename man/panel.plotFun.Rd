\name{panel.plotFun}
\alias{panel.plotFun}
\title{Panel function for plotting functions}
\usage{
  panel.plotFun(object, ..., npts = NULL, zlab = NULL,
    lwd = 1, col = "black", filled = TRUE, levels = NULL,
    nlevels = 10, surface = FALSE,
    colorscheme = topo.colors, type = "l",
    transparency = NULL)
}
\arguments{
  \item{object}{an object (e.g., a formula) describing a
  function}

  \item{\dots}{additional arguments passed on to other
  panel functions}

  \item{npts}{an integer giving the number of points (in
  each dimension) to sample the function}

  \item{zlab}{label for z axis (when in surface-plot mode)}

  \item{lwd}{line width for line graphs and contours}

  \item{col}{color for line graphs and contours}

  \item{filled}{fill with color between the contours
  (\code{TRUE} by default)}

  \item{levels}{levels at which to draw contours}

  \item{nlevels}{number of contours to draw (if
  \code{levels} not specified)}

  \item{surface}{a logical indicating whether to draw a
  surface plot rather than a contour plot}

  \item{colorscheme}{a function (\code{topo.colors} by
  default) for choosing colors for fill}

  \item{type}{type of plot (\code{"l"} by default)}

  \item{transparency}{number from 0 (transparent) to 1
  (opaque) for the fill colors}
}
\description{
  Panel function for plotting functions
}
\examples{
x <- runif(30,0,2*pi)
d <- data.frame( x = x,  y = sin(x) + rnorm(30,sd=.2) )
xyplot( y ~ x, data=d )
ladd(panel.plotFun( sin(x) ~ x ) )
}
\seealso{
  plotFun
}

