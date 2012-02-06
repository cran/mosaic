\name{plotFun}
\alias{plotFun}
\title{Plotting mathematical expressions}
\usage{
  plotFun(object, ..., add = FALSE, xlim = NULL,
    ylim = NULL, npts = NULL, ylab = NULL, xlab = NULL,
    zlab = NULL, main = NULL, lwd = 1, col = "black",
    filled = TRUE, levels = NULL, nlevels = 10,
    surface = FALSE, colorscheme = topo.colors, type = "l",
    transparency = NULL)
}
\arguments{
  \item{object}{a mathematical expression (see examples)}

  \item{...}{additional assignments to parameters and
  limits}

  \item{add}{if TRUE, then overlay an existing plot}

  \item{xlim}{limits for x axis (or use variable names, see
  examples)}

  \item{ylim}{limits for y axis (or use variable names, see
  examples)}

  \item{npts}{number of points for plotting.}

  \item{xlab}{label for x axis}

  \item{ylab}{label for y axis}

  \item{zlab}{label for z axis (when in surface-plot mode)}

  \item{main}{main label for plot}

  \item{lwd}{line width for line graphs and contours}

  \item{col}{color for line graphs and contours}

  \item{filled}{fill with color between the contours
  (\code{TRUE} by default)}

  \item{levels}{levels at which to draw contours}

  \item{nlevels}{number of contours to draw (if
  \code{levels} not specified)}

  \item{surface}{draw a surface plot rather than a contour
  plot}

  \item{colorscheme}{function (\code{topo.colors} by
  default) for choosing colors for fill}

  \item{type}{type of plot (\code{"l"} by default)}

  \item{transparency}{number from 0 (transparent) to 1
  (opaque) for the fill colors}
}
\value{
  an R function for the expression being plotted (not a
  graphics object)
}
\description{
  Plots mathematical expressions in one and two variables.
}
\details{
  makes plots of mathematical expressions using the formula
  syntax.  Will draw both line plots and contour/surface
  plots (for functions of two variables) In RStudio, the
  surface plot comes with sliders to set orientation. If
  the colors in filled surface plots are two blocky,
  increase \code{npts} beyond the default of 50.
  \code{npts=300} is as much as you're likely to every need
  See examples for overplotting a constraint function on an
  objective function.
}
\examples{
plotFun(a*sin(x^2)~x, xlim=range(-5,5), a=2)
f <- rfun( ~ u & v)
plotFun( f(u=u,v=v) ~ u & v, u.lim=range(-3,3), v.lim=range(-3,3) )
plotFun( u^2 + v < 3 ~ u & v, add=TRUE, npts=200)
}
\author{
  Daniel Kaplan (\email{kaplan@macalester.edu})
}

