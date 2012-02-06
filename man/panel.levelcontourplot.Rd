\name{panel.levelcontourplot}
\alias{panel.levelcontourplot}
\title{Lattice plot that draws a filled contour plot}
\usage{
  panel.levelcontourplot(x, y, z, subscripts, at, shrink,
    labels = FALSE,
    label.style = c("mixed", "flat", "align"),
    contour = FALSE, region = TRUE, col = add.line$col,
    lty = add.line$lty, lwd = add.line$lwd,
    border = "transparent", ..., col.regions = regions$col,
    color.scheme, filled = TRUE,
    alpha.regions = regions$alpha)
}
\arguments{
  \item{x}{x on a grid}

  \item{y}{y on a grid}

  \item{z}{zvalues for the x and y}

  \item{subscripts}{which points to plot}

  \item{at}{cuts for the contours}

  \item{shrink}{what does this do?}

  \item{labels}{draw the contour labels}

  \item{label.style}{where to put the labels}

  \item{contour}{logical draw the contours}

  \item{region}{logical color the regions}

  \item{col}{color for contours}

  \item{lty}{type for contours}

  \item{lwd}{width for contour}

  \item{border}{type of border}

  \item{\ldots}{additional arguments}

  \item{col.regions}{color set for regions}

  \item{color.scheme}{color generation function, e.g.
  \code{rainbow}}

  \item{filled}{whether to fill the contours with color}

  \item{alpha.regions}{transparency of regions}
}
\description{
  Used within plotFun
}

