# Contributed by Randall Pruim (rpruim@calvin.edu)

interval <- function(x, ...){UseMethod("interval", x)}

interval.htest <- function (x, verbose=FALSE, ...){
  int <- x$conf.int
  lev <- attr(int, "conf.level")
  if (verbose ) {
	  cat( "\n" )
	  cat('Method: ')
	  cat(x$method)
	  cat( "\n" )
	  cat( "\n" )
	  print(x$estimate) 
	  cat( "\n" )
	  cat( paste(lev * 100, "% confidence interval: \n", sep = "") )
	  cat( as.vector(int) )
	  cat( "\n" )
	  cat( "\n" )
  	  invisible(int)
  }
  interv <- as.vector(int) 
  names(interv) <- c('lower','upper')
  int <- c(x$estimate, interv )
  return(int)
}
