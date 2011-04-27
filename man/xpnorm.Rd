\name{xpnorm}
\alias{xpnorm}
\alias{xqnorm}
\title{
Augmented versions of pnorm and qnorm
}
\description{
These functions all behave similarly to the functions with the initial \code{x}
removed from their names but add more verbose output and graphics.
}
\usage{
xpnorm(q, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, invisible = FALSE, 
    digits = 4, lower.tail = TRUE, log.p = FALSE, xlim, ylim, 
    vlwd = 2, vcol = trellis.par.get("add.line")$col, rot = 45, 
    ...) 

xqnorm(p, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, digits = 4, 
    lower.tail = TRUE, log.p = FALSE, xlim, ylim, invisible = FALSE, 
    vlwd = 2, vcol = trellis.par.get("add.line")$col, rot = 45, 
    ...) 

}
\arguments{
\item{p}{probability}
\item{q}{quantile}
\item{mean, sd}{parameters of normal distribution.}
\item{plot}{logical.  If TRUE, show an illustrative plot.}
\item{verbose}{logical.  If TRUE, display verbose output.}
\item{invisible}{logical.  If TRUE, return value invisibly.}
\item{digits}{number of digits to display in output.}
\item{lower.tail}{logical.  If FALSE, use upper tail probabilities.}
\item{log.p}{logical.  If TRUE, uses the log of probabilities.}
\item{xlim, ylim}{limits for plotting.}
\item{vlwd, vcol}{line width and color for vertical lines}
\item{rot}{angle of rotation for text labels}
\item{...}{additional arguments.}
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
xpnorm(650, 500, 100)
xqnorm(.75, 500, 100)
if (require(manipulate)) {
  manipulate( xpnorm(score, 500, 100, verbose=verbose),
    score = slider(200,800),
	verbose = checkbox(TRUE, label="Verbose Output")
  )
} 

}
