
enterxtab <- function() {
	cat("Row variable name: ")
	rowvar <- scan(what=character(0), n=1)
	cat("Row variable levels: ")
	rnames <- scan(what=character(0))
	cat("Column variable name: ")
	colvar <- scan(what=character(0), n=1)
	cat("Column variable levels: ")
	cnames <- scan(what=character(0))

	result <- matrix(NA, nr=length(rnames), nc=length(cnames))
	for ( r in 1:length(rnames) ) {
		cat (paste("Row ", r, ": ", sep="") )
		result[r,] <- scan(what=integer(0), n=length(cnames))
	}

	dn <- list(rnames, cnames)
    names(dn) <- c(rowvar, colvar)
    attr(result, "dimnames") <- dn
    class(result) <- c("xtabs", "table")
    return(result)
}
