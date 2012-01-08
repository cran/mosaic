
# utility for various graphical representations of distributions.

distPlot <- function( dist, params=list(), kind=c('density','cdf','qq','histogram'), 
					 xlab="", ylab="", breaks, type, resolution=5000,... ) {
	kind = match.arg(kind)
	ddist = paste('d', dist, sep='')
	qdist = paste('q', dist, sep='')
	pdist = paste('p', dist, sep='')

	values = do.call(qdist, c(p=list(ppoints(resolution)), params)) 
	fewerValues = unique(values)
	discrete = length(fewerValues) < length(values) 
	if ( missing(breaks) && discrete ){
		step = min(diff(fewerValues))
		breaks = seq( min(fewerValues) -.5 * step , max(fewerValues) + .5*step, step)
	}
	if (kind=='cdf') {
		if (discrete) {
			step = min(diff(fewerValues))
			cdfx <- seq( min(fewerValues) -1.5 * step , max(fewerValues) + 1.5*step, length.out=resolution)
			cdfy <- approxfun( fewerValues, do.call(pdist, c(list(q=fewerValues),params)), method='constant', 
							  f=1, yleft=0, yright=1 ) (cdfx)
		} else {
			cdfx <- values
			cdfy <- do.call( pdist, c(list(q=values), params) ) 
		}
	}
	if (missing(type)) {
		if (discrete) {
			type = switch(kind,
						  density = c('p','h'),
						  cdf = 'p',
						  histogram = 'density',
						  qq = 'l')  
		} else {
			type = switch(kind,
						  density = 'l',
						  cdf = 'l',
						  histogram = 'density',
						  qq = 'l')
		}
	}

	switch(kind, 
		   density = lattice::xyplot( do.call( ddist, c(list(x=fewerValues), params) ) ~ fewerValues, 
							type=type, xlab=xlab, ylab=ylab, ...),
		   cdf = lattice::xyplot( cdfy ~ cdfx, type=type, xlab=xlab, ylab=ylab, ...),
		   qq = lattice::qqmath( ~ values, type=type, xlab=xlab, ylab=ylab, ...),
		   histogram = lattice::histogram( ~ values, type=type, xlab=xlab, breaks=breaks, ...)
		   )
}

