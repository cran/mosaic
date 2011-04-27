##################
# p,q,r for data sets
# Creates a parallelism between the functions for distributions and the
# functions for data
# Written June 14, 2009
.check_for_quant_input <- function(x) {
	if (is.data.frame(x) ) {
		stop("Give a variable, not a data frame.")
	}

	if (is.factor(x) | is.character(x) ) {
		stop("Input must be a numerical variable, not categorical.")
	}
	return(TRUE)
}

qdata <- function(p, vals, ... ) {
	.check_for_quant_input(vals)
	if (any(p > 1) | any(p < 0) ) {
		stop("Prob outside of range 0 to 1.  Do you perhaps want pdata?")
	}
	quantile(vals, probs=p, na.rm=TRUE, ... )
}

pdata = function(q, vals, lower.tail=TRUE, ... ) {
  .check_for_quant_input(vals)
#  L = length(q)
#  res = rep(0,L)
  n <- sum( ! is.na(vals) )
  probs <- sapply( q, function(q) { sum( vals <= q , na.rm=TRUE ) } ) / n
  if (lower.tail) { 
  	return(probs)
  } else {
	return( 1 - probs )
  }
}

rdata = function(n, vals, replace=TRUE, ... ) {
  sample( vals, n, replace=replace, ...)
}

ddata = function(x, vals, log=FALSE, ...) {
	n <- sum( ! is.na(vals) )
	print(n)
	probs <- sapply(x, function(x) { sum( vals == x, na.rm=TRUE ) / n } )
	if (log) { probs <- log(probs) }
	return (probs)
}

