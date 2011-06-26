.oldD <- function(f, n=1L, h=1e-4) {
  	n <- as.integer(n)
  	if ( ! is.integer(n) || n < 0 ) {
		stop( 'n must be a non-negative integer')
	}
	if (n == 0) {
		return(f)
	}
	if (n==1) {
  		return( function(x, ... ) {
			# setting h
			temp = x+h
			h = x - temp
			# finite difference value: rise over run
			return( (f(x+h,...) - f(x, ...) )/h )
			}
		)
	}
	return ( .oldD(.oldD(f,n-1,h/2)) )
}

