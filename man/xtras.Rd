\name{xtras}
\alias{xchisq.test}
\alias{xhistogram}
\alias{xqqmath}
\alias{panel.xqqmath}
\alias{panel.xhistogram}
\title{
Augmented functions
}
\description{
These functions all behave similarly to the functions with the initial \code{x}
removed from their names.
}
\usage{
xchisq.test(...)

xhistogram(x, data=NULL, panel=panel.xhistogram, type="density", ...)

xqqmath(x, data=NULL, panel="panel.xqqmath", ...)

panel.xqqmath(x, qqmathline = !(fitline || idline), idline = FALSE, fitline = FALSE, 
    slope = NULL, intercept = NULL, overlines = FALSE, groups=NULL, ...,
    col.line = trellis.par.get("add.line")$col, pch = 16, lwd = 2, 
    lty = 2 ) 

panel.xhistogram(x, 
	dcol = trellis.par.get("plot.line")$col, dlwd = 2, 
    gcol = trellis.par.get("add.line")$col, glwd = 2, 
	fcol = trellis.par.get("superpose.polygon")$col,
	dmath = dnorm, 
	verbose = FALSE,
    dn = 100, args = NULL, labels = FALSE, density = FALSE, fit = NULL, 
    start = NULL, type = "density", v, h, groups=NULL, breaks, 
	stripes=c('vertical','horizontal','none'), alpha=1, ...) 
}
\arguments{
  \item{data}{a data frame.}

  \item{density }{boolean -- should a density curve be overlaid on histogram? }

  \item{dmath}{density function, defaults to \code{dnorm}}

  \item{dn}{an integer indicating the resolution to use for plotting the density curve}

  \item{args}{a list of arguments passed to \code{dmath}}

  \item{v, h}{numerical vector of positions for vertical and horizontal lines to add to plot}

  \item{breaks}{break points for histogram bins}

  \item{stripes}{type of stripes to use for grouping in histograms}

  \item{alpha}{opacity value for histogram rectangles}

  \item{panel}{a panel function}

  \item{x}{ A numeric vector or formula. }

  \item{overlines}{boolean. If TRUE, place lines above the data, otherwise
  lines are below (default).
  }

  \item{idline}{boolean. If TRUE, add the line with slope 1 and intercept 0
  to the plot.
  }

  \item{qqmathline}{boolean.  
  	If TRUE, add a line using \code{panel.qqmathline}
	}

  \item{fitline}{boolean.  If TRUE, add a line to the plot, using
  slope and intercept given by \code{slope} and \code{intercept}.
  }


  \item{slope}{ slope for \code{fitline}.  If NULL (the default)
  the mean of the data is used, which works well
  for normal-quantile plots but may not be what you want for fitting
  other distributions. }

  \item{intercept}{ intercept for \code{fitline}.  
  If NULL (the default), 
	the standard deviation of the data is used, which works well
	for normal-quantile plots but may not be what you want for fitting
	other distributions. }
\item{col.line}{like \code{col} but applied only to lines}

\item{lwd,lty}{line width and line type}

\item{dcol, dlwd}{like \code{col} and \code{lwd} but applied to density curves}

\item{gcol, glwd}{like \code{col} and \code{lwd} but applied to grid lines}

\item{fcol}{fill color for histogram rectangles}

\item{fit}{passed to \code{fitdistr} from \code{MASS} for maximum-likelihood fitting}

\item{verbose}{a logical indicating whether verbose output should be displayed}

\item{groups}{grouping variable as in other \code{lattice} plots}

\item{pch}{plot character as in other \code{lattice} plots}

\item{type}{type of histogram}

\item{labels}{a logical indicating whether labels be added to histogram}

\item{start}{starting point for use by \code{fitdistr}}

\item{\dots}{other arguments including
\code{q},
\code{p},
\code{which},
\code{main}
which behave as in other \code{\link{lattice}} graphics functions
}
}

\author{
Randall Pruim
}

\seealso{
\code{\link{histogram}}, 
\code{\link{chisq.test}}, 
\code{\link{pnorm}}, 
\code{\link{qnorm}}, 
\code{\link{qqmath}}, and
\code{\link{plot}}. 
}
\examples{
# Physicians' Health Study data
phs <- cbind(c(104,189),c(10933,10845)) 
rownames(phs) <- c("aspirin","placebo") 
colnames(phs) <- c("heart attack","no heart attack") 
phs 
xchisq.test(phs) 
x <- rnorm(200)
xhistogram(~ x, groups = x > 2, n=20, density=TRUE )
}
