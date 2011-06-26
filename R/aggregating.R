.mosaic_aggregate <- function(x, data, FUN, overall=mosaic.par.get("aggregate.overall"), ...) {
	if (length(x) == 2 ) {
		return( data.frame( FUN (eval( x[[2]], data) ) ) )
	} else {
		return( as.data.frame( 
			Hmisc::summary.formula( x, data, fun=FUN, overall=overall, method='cross',...) ) )
	}
	result <- summary(x, data, fun=FUN, overall=overall, method=method, ...)
	result <- as.data.frame(oldUnclass(result))
	return(result)
}

# implement test for formula with NULL, ., 1 or 0 on the right hand side
.test.formula.simple.RHS = function(x){
     "formula" == class(x)  &&
         (length(x)==2 || is.null(x[[3]]) ||
          (length(x[[3]])==1 &&
          ((is.numeric(x[[3]]) && (x[[3]]==0 || x[[3]]==1)) ||  (all.names(x[[3]]) %in% c(".")))))
}

# basic simple stat functions, e.g., mean, IQR, sd, var, median
.stat.fun.maker = function(fun,methodname){
  function(x, data=NULL, ...) {
    if( is.name( substitute(x) ) && !is.null(data))
      fun(eval( substitute(x), data, enclos=parent.frame()), ...)
    else { if( .test.formula.simple.RHS(x) ) {
         # It's a formula with no left-hand side or a simple right-hand side, e.g. NULL, 
         fun( eval( x[[2]], data, enclos=parent.frame()), ...)
      }
      else UseMethod(methodname)
    }
  }
}

# This version is needed when functions take ... as their first argument: min and max
.stat.fun.maker2 = function(fun,methodname){
  function(..., data=NULL, na.rm=TRUE) {
    x = list(substitute(...))[[1]]
    if( is.name( x ) && !is.null(data)) {
      fun(eval( x, data, enclos=parent.frame()))
    }
    else {
      # This won't work for simple formulas
      if(.test.formula.simple.RHS(x) ) { #"formula" == class(x)  && length(x)==2 ) {
         # It's a formula with no left-hand side
         fun( eval( x[[2]], data, enclos=parent.frame()))
      }
      else UseMethod(methodname)
    }
  }
}

mean <- function(x, ..., na.rm=TRUE) {
	UseMethod("mean")
}

median <- function(x, ..., na.rm=TRUE) {
	UseMethod("median")
}

#mean   <- .stat.fun.maker( mean, "mean" )
#median <- .stat.fun.maker( stats::median, "median" )
sd     <- .stat.fun.maker( stats::sd, "sd" )
var    <- .stat.fun.maker( stats::var, "var" )
IQR    <- .stat.fun.maker( IQR, "IQR" )
prop   <- .stat.fun.maker( prop, "prop" )
count  <- .stat.fun.maker( count, "count" )
#min    <- .stat.fun.maker2( min, "min" )
#max    <- .stat.fun.maker2( max, "max" )

.stat.fun.formula.maker <- function(FUN,resname) {
  function( x, data=parent.frame(), na.rm=TRUE, ... ) {
	result <- .mosaic_aggregate( x, data, FUN=FUN, na.rm=na.rm, ... )
	class(result) <- c('aggregated.stat', class(result))
	attr(result, 'stat.name') <- resname
	return(result)
  }
}

mean.formula    <- function(x, data, ..., na.rm=TRUE){
	result <- .mosaic_aggregate( x, data, FUN=mean, na.rm=na.rm, ... )
	class(result) <- c('aggregated.stat', class(result))
	attr(result, 'stat.name') <- resname
	return(result)
}

median.formula    <- function(x, data, ..., na.rm=TRUE) {
	result <- .mosaic_aggregate( x, data, FUN=mean, na.rm=na.rm, ... )
	class(result) <- c('aggregated.stat', class(result))
	attr(result, 'stat.name') <- resname
	return(result)
}

sd.formula      <- .stat.fun.formula.maker( stats::sd,     "sd" )
var.formula     <- .stat.fun.formula.maker( stats::var,    "var" )
IQR.formula     <- .stat.fun.formula.maker( stats::IQR,    "IQR" )
count.formula   <- .stat.fun.formula.maker( count.default, "count" )
prop.formula    <- .stat.fun.formula.maker( prop.default,  "prop" )
#min.formula     <- .stat.fun.formula.maker( base::min,     "min" )
#max.formula     <- .stat.fun.formula.maker( base::max,     "max" )

mean.default   <- function( x, ..., na.rm=TRUE) c(mean = base::mean.default(x, na.rm=na.rm, ...))
sd.default     <- function( x, na.rm=TRUE, ... ) stats::sd(x, na.rm=na.rm)
var.default    <- function( x, na.rm=TRUE, ... ) stats::var(x, na.rm=na.rm)
median.default <- function( x, ..., na.rm=TRUE)  stats::median.default(x, na.rm=na.rm)
IQR.default    <- function( x, na.rm=TRUE, ... ) stats::IQR( x, na.rm=na.rm, ...)
count.default  <- function( x, na.rm=TRUE, ... ) count.factor( as.factor(x), na.rm=na.rm, ...)
prop.default   <- function( x, na.rm=TRUE, ... ) prop.factor( as.factor(x), ...)
#min.default    <- function( ..., na.rm=TRUE ) base::min( ..., na.rm=na.rm)
#max.default    <- function( ..., na.rm=TRUE ) base::max( ..., na.rm=na.rm)

.stat.fun.factor.bogus.maker = function(statname) {
  function( x, na.rm=TRUE, ...) {
    stop( paste("Can't take",statname,"of a factor (categorical).  Use, e.g., count( ) or prop( ).") )
  }
}

.stat.fun.factor.bogus.maker2 = function(statname) {
  function( x, ..., na.rm=TRUE) {
    stop( paste("Can't take",statname,"of a factor (categorical).  Use, e.g., count( ) or prop( ).") )
  }
}
mean.factor   <- .stat.fun.factor.bogus.maker2("mean")
median.factor <- .stat.fun.factor.bogus.maker2("median")
sd.factor     <- .stat.fun.factor.bogus.maker("sd")
var.factor    <- .stat.fun.factor.bogus.maker("var")
IQR.factor    <- .stat.fun.factor.bogus.maker("IQR")
#min.factor    <- .stat.fun.factor.bogus.maker("min")
#max.factor    <- .stat.fun.factor.bogus.maker("max")

count.logical <- function(x, level=TRUE, na.rm=TRUE, ...) 
	count.factor( as.factor(x), level=level, na.rm=na.rm ) 

count.factor <- function(x, level=levels(x)[1], na.rm=TRUE, ...) {
	if (! level %in% levels(x) ) {
		level = levels(x) [as.numeric(level)]
	}
	result <- sum( x == level, na.rm=na.rm ) 
	names(result) <- paste('count', level, sep=".")
	return(result)
}

prop.logical <- function(x, level=TRUE, na.rm=TRUE, ...) 
	prop.factor( as.factor(x), level=level, na.rm=na.rm )

prop.factor <- function(x, level=levels(x)[1], na.rm=TRUE, ...) {
	xx <- substitute(x)
	x.string <- tail(as.character(xx),1)
	if (! level %in% levels(x) ) {
		level = levels(x) [as.numeric(level)]
	}
	result <- mean( x == level, na.rm=na.rm ) 
	names(result) <- paste('prop', level, sep=".")
	return(result)
}

