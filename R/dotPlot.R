#' Dotplots
#' 
#' A high level function and panel function for producing
#' 	a variant of a histogram called a dotplot.
#'
#' @param x  a vector of values or a formula 
#'
#' @param nint  the number of intervals to use 
#'
#' @param panel  a panel function 
#'
#' @param breaks,equal.widths,groups,pch,col,lty,lwd,col.line,type,alpha  
#'     as in [histogram()] 
#'
#' @param cex  a ratio by which to increase or decrease the dot size
#'
#' @param \dots  additional arguments 
#' 
#' @return a trellis object
#'
#' 
#' @seealso [histogram()]
#' 
#' 
#' @examples
#' if (require(mosaicData)) {
#' dotPlot( ~ age, data = HELPrct)
#' dotPlot( ~ age, nint=42, data = HELPrct)
#' dotPlot( ~ height | voice.part, data = singer, nint = 17,
#'           endpoints = c(59.5, 76.5), layout = c(4,2), aspect = 1,
#'           xlab = "Height (inches)")
#' }
#' @keywords graphics 
#' @export

dotPlot <-
function (x, breaks, ..., panel = panel.dotPlot) 
{
  histogram(x, type = "count", panel = panel, breaks = breaks, ...)
}

#' @rdname dotPlot
#' @export

panel.dotPlot <-
  function (x, breaks, equal.widths = TRUE, groups = NULL, 
            nint = if (is.factor(x)) nlevels(x) else round(1.3* log2(length(x)) + 4),
            pch,
            col,
            lty = trellis.par.get("dot.line")$lty, 
            lwd = trellis.par.get("dot.line")$lwd, 
            col.line = trellis.par.get("dot.line")$col, 
            alpha = trellis.par.get("dot.symbol")$alpha, 
            cex=1, 
            type = "count", 
            ...) 
{
  if (is.function(breaks)) {
      breaks <- do.call(breaks, c(list(x=x, nint=nint), list(...)))
  }
      
  if (missing(pch)) {
    pch <-  if (is.null(groups)) trellis.par.get("dot.symbol")$pch 
              else trellis.par.get("superpose.symbol")$pch 
  }
  if (missing(col)) {
    col <- if (is.null(groups)) trellis.par.get("dot.symbol")$col 
            else trellis.par.get("superpose.symbol")$col 
  }
    
	if (is.null(nint)) nint <- if (is.factor(x)) nlevels(x) else round(1.3* log2(length(x)) + 4)
    dot.line <- trellis.par.get("dot.line")
    dot.symbol <- trellis.par.get("dot.symbol")
    sup.symbol <- trellis.par.get("superpose.symbol")
    x <- as.numeric(x)
	if ( !is.null(groups) ) {
		groups <- factor(groups)
		col <- rep(col, length.out=length(levels(groups)))
		pch <- rep(pch, length.out=length(levels(groups)))
		col <- col[as.numeric(groups)]
		pch <- pch[as.numeric(groups)]
		col <- col[order(x)]
		pch <- pch[order(x)]
	} else {
		pch <- rep(pch[1], length.out <- length(x))
		col <- rep(col[1], length.out <- length(x))
	}

    if (length(x) > 0) {
        if (missing(breaks)) {
            breaks <- if (equal.widths) 
                do.breaks(range(x, finite = TRUE), nint)
            else quantile(x, (0:nint)/nint, na.rm = TRUE)
        }
        h <- hist(x, breaks = breaks, plot = FALSE, warn.unused=FALSE, ...)
        cumcounts <- c(0,cumsum(h$counts))
        nb <- length(breaks)

        if (length(h$counts) != nb - 1) {
            stop("problem with hist computations")
        }
        if (nb > 1) {
            for (bin in 1:(nb - 1)) {
                if (h$counts[bin] <= 0) {
                  next
                }
                yvals <- 1:(h$counts[bin])
                xvals <- rep( h$mids[bin], h$counts[bin] )
                grid.points( 
                  size = cex * grid::unit(0.9 / current.panel.limits()$ylim[2], 
                                          "snpc"),  
                  pch = pch, 
                  gp = gpar(fill = col, alpha = alpha, col = col, 
                            lty = lty, lwd = lwd), 
				  		    x = xvals,
                  y = yvals - 0.5, 
						default.units = "native")
				pch <- pch[ -(1:h$counts[bin]) ]
				col <- col[ -(1:h$counts[bin]) ]
            }
        }
    }
}
