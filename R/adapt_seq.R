
.adapt_seq <-function(from, to, 
	length.out=100, 
	f=function(x,...){ 1 }, 
	args=list(),
	frac=.25
	) 
{
	n <- round(log(length.out))
	s <- seq(from, to, length.out=n)
	while (length(s) < length.out) {
		# subdivide all intervals
		ds <- diff(s)
		mid.s <- s[-1] - .5 * ds
		s <- sort( c(s, mid.s) )

		# subdivide again if function changing rapidly
		ds <- diff(s)
		mid.s <- s[-1] - .5 * ds
		y <- do.call(f, args=c(list(s), args))
		dy <- abs(diff(y))
		o <- rev(order(dy))
		m <- sum( dy > quantile(dy,.75, na.rm=TRUE), na.rm=TRUE )
		new.s <- mid.s[ o[ 1:m ] ]
		s <- sort( c(s, new.s) )
	}
	return(s)
}
