
.mosaic_aggregate <- function(x, data, FUN, overall=mosaic.par.get("aggregate.overall"), ...) {
	if (length(x) == 2 ) {
		return( data.frame( FUN (eval( x[[2]], data, enclose=parent.frame()) ) ) )
	} else {
		return( as.data.frame( 
			Hmisc::summary.formula( x, data, fun=FUN, overall=overall, method='cross',...) ) )
	}
	result <- summary(x, data, fun=FUN, overall=overall, method=method, ...)
	result <- as.data.frame(oldUnclass(result))
	return(result)
}

# returns TRUE for a formula, FALSE otherwise, even if evaluation throws an error
.is.formula <- function(x)  
	tryCatch( inherits(x, 'formula'), error = function(e) {FALSE} )

# check for formula with no left-hand side or a simple right-hand side, e.g. NULL, ., 1, or 0
.is.simple.formula <-  function(x){
     inherits(x, "formula") &&
         (length(x)==2 || is.null(x[[3]]) ||
          (length(x[[3]])==1 &&
          ((is.numeric(x[[3]]) && (x[[3]]==0 || x[[3]]==1)) ||  (all.names(x[[3]]) %in% c(".")))))
}

.simple.part <- function(x) {
	if (! .is.simple.formula(x) ) {
		return(NULL) 
	} else {
		return(x[[2]])
	}
}


.flatten <- function(x) {
    .x <- c()
  for (item in x) .x <- c(.x,item)
  return(.x)
}


##########################################################################################

setGeneric( 
	"mean", 
	function(x, ..., na.rm=TRUE, trim=0)  {
		dots <- list(...)
		if ( ! .is.formula(x) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
			data <- dots[[1]]
			return( base::mean( eval(substitute(x), data), na.rm=na.rm, trim=trim) )
		}
		standardGeneric('mean')
	}
)

setMethod(
	'mean',
	'ANY',
	function(x, ..., na.rm=TRUE, trim=0) 
		c( mean=base::mean( .flatten(c(x,list(...))), na.rm=na.rm, trim=trim ) )
	
)

setMethod(
	'mean',
	'numeric',
	function(x, ..., na.rm=TRUE, trim=0) 
		c( mean=base::mean( c(x,.flatten(list(...))), na.rm=na.rm, trim=trim ) )
	
)

setMethod( 
	"mean", 
	signature=c("data.frame"),
	function(x, ..., na.rm=TRUE, trim=0) 
		base::mean(x=x, ..., na.rm=na.rm, trim=trim)
	
)

setMethod( 
	"mean", 
	signature=c("formula"),
	function(x, data=parent.frame(), ..., na.rm=TRUE, trim=0) {
		if( .is.simple.formula(x) ) {
			return( mean( eval( .simple.part(x), data, enclos=parent.frame()), 
							   ..., na.rm=na.rm, trim=trim ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=base::mean, ..., na.rm=na.rm, trim=trim ) )
		} 
	}
)

##########################################################################################


setGeneric( 
	"median", 
	function(x, ..., na.rm=TRUE)  {
		dots <- list(...)
		if ( ! .is.formula(x) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
			data <- dots[[1]]
			return(stats::median(eval( substitute(x), data),  na.rm=na.rm))
		}
		standardGeneric('median')
	}
)

setMethod(
	'median',
	'ANY',
	function(x, ..., na.rm=TRUE) 
		c( median=stats::median( .flatten(c(x,list(...))), na.rm=na.rm ) )
	
)

setMethod(
	'median',
	'numeric',
	function(x, ..., na.rm=TRUE) 
		c( median=stats::median( c(x,.flatten(list(...))), na.rm=na.rm) )
	
)

setMethod( 
	"median", 
	signature=c("data.frame"),
	function(x, ..., na.rm=TRUE) sapply( x, stats::median, na.rm=na.rm)
)

setMethod( 
	"median", 
	signature=c("formula"),
	function(x, data=parent.frame(), ..., na.rm=TRUE) {
		if( .is.simple.formula(x) ) {
			return( median( eval( .simple.part(x), data, enclos=parent.frame()), 
							   ..., na.rm=na.rm ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=stats::median, na.rm=na.rm) )
		} 
	}
)

##########################################################################################

setGeneric( 
	"sd", 
	function(x, ..., na.rm=TRUE)  {
		dots <- list(...)
		if ( ! .is.formula(x) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
			data <- dots[[1]]
			return(stats::sd(eval( substitute(x), data),  na.rm=na.rm))
		}
		standardGeneric('sd')
	}
)

setMethod(
	'sd',
	'ANY',
	function(x, ..., na.rm=TRUE) 
		c( sd=stats::sd( .flatten(c(x,list(...))), na.rm=na.rm) )
)

setMethod(
	'sd',
	'numeric',
	function(x, ..., na.rm=TRUE) 
		c( sd=stats::sd( c(x,.flatten(list(...))), na.rm=na.rm) )
	
)

setMethod( 
	"sd", 
	signature=c("data.frame"),
	function(x, ..., na.rm=TRUE) sapply( x, stats::sd, na.rm=na.rm)
)

setMethod( 
	"sd", 
	signature=c("formula"),
	function(x, data=parent.frame(), ..., na.rm=TRUE) {
		if( .is.simple.formula(x) ) {
			return( sd( eval( .simple.part(x), data, enclos=parent.frame()), ..., na.rm=na.rm ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=stats::sd, na.rm=na.rm) )
		} 
	}
)
##########################################################################################
# this is currently broken in that NAME does not get substituted as desired in standardGeneric()

.make.Maxlike.generic <- function( NAME=".Max", FUN = stats::max ) {

	setGeneric( 
		NAME, 
		function(x, ..., na.rm=TRUE)  {
			dots <- list(...)
			if ( ! .is.formula(x) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
				data <- dots[[1]]
				return(FUN(eval( substitute(x), data),  na.rm=na.rm))
			}
			standardGeneric(NAME)
		}
	)

	setMethod(
		NAME,
		'numeric',
		function(x, ..., na.rm=TRUE) {
			FUN(x, ..., na.rm=na.rm) 
		}
	)


	setMethod( 
		NAME, 
		signature=c("data.frame"),
		function(x, ..., na.rm=TRUE) {
			sapply( x, FUN, na.rm=na.rm)
		}
	)


	setMethod( 
		NAME, 
		signature=c("formula"),
		function(x, ..., na.rm=TRUE) {
			dots <- list(...)
			data  <- dots[[1]]

			if( .is.simple.formula(x) ) {
				return( FUN( eval( .simple.part(x), data, enclos=parent.frame()), na.rm=na.rm ) )
			} else {
				return( .mosaic_aggregate( x, data, FUN=FUN, na.rm=na.rm) )
			} 
		}
	)
}

#.make.Maxlike.generic( 'Max', base::max )
#.make.Maxlike.generic( 'Min', base::min )

###############################################################
setGeneric( 
	'.Max', 
	function(x, ..., na.rm=TRUE)  {
		dots <- list(...)
		if ( ! .is.formula(x) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
			data <- dots[[1]]
			return(base::max(eval( substitute(x), data),  na.rm=na.rm))
		}
		standardGeneric('.Max')
	}
)

setMethod(
	'.Max',
	'ANY',
	function(x, ..., na.rm=TRUE) 
		c( max=base::max( x,..., na.rm=na.rm) )
)

setMethod(
	'.Max',
	'numeric',
	function(x, ..., na.rm=TRUE) {
		base::max(x, ..., na.rm=na.rm) 
	}
)


setMethod( 
	'.Max', 
	signature=c("data.frame"),
	function(x, ..., na.rm=TRUE) {
		sapply( x, base::max, na.rm=na.rm)
	}
)


setMethod( 
	'.Max', 
	signature=c("formula"),
	function(x, ..., na.rm=TRUE) {
		dots <- list(...)
		data  <- dots[[1]]

		if( .is.simple.formula(x) ) {
			return( base::max( eval( .simple.part(x), data, enclos=parent.frame()), na.rm=na.rm ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=base::max, na.rm=na.rm) )
		} 
	}
)
###############################################################
setGeneric( 
	'.Min', 
	function(x, ..., na.rm=TRUE)  {
		dots <- list(...)
		if ( ! .is.formula(x) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
			data <- dots[[1]]
			return(base::min(eval( substitute(x), data),  na.rm=na.rm))
		}
		standardGeneric('.Min')
	}
)

setMethod(
	'.Min',
	'ANY',
	function(x, ..., na.rm=TRUE) 
		c( max=base::min(x ,..., na.rm=na.rm) )
)

setMethod(
	'.Min',
	'numeric',
	function(x, ..., na.rm=TRUE) {
		base::min(x, ..., na.rm=na.rm) 
	}
)


setMethod( 
	'.Min', 
	signature=c("data.frame"),
	function(x, ..., na.rm=TRUE) {
		sapply( x, base::min, na.rm=na.rm)
	}
)


setMethod( 
	'.Min', 
	signature=c("formula"),
	function(x, ..., na.rm=TRUE) {
		dots <- list(...)
		data  <- dots[[1]]

		if( .is.simple.formula(x) ) {
			return( base::min( eval( .simple.part(x), data, enclos=parent.frame()), na.rm=na.rm ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=base::min, na.rm=na.rm) )
		} 
	}
)

##########################################################################################



setGeneric( 
	"var", 
	function(x, y=NULL, na.rm=TRUE, use, data=NULL)  {
		if ( is.name(substitute(x)) && ! is.null(y) && is.name(substitute(y)) && is.data.frame( data ) ) {
			return( stats::var(eval( substitute(x), data), eval(substitute(y, data), na.rm=na.rm, use=use)) )
		}
		if ( is.name(substitute(x)) && is.data.frame( y ) ) {
			return(stats::var(eval( substitute(x), y),  na.rm=na.rm))
		}
		if ( is.name(substitute(x)) && is.null(y) && is.data.frame( data ) ) {
			return( stats::var( eval( substitute(x), data), na.rm=na.rm, use=use) )
		}
		standardGeneric('var')
	}
)


setMethod(
	'var',
	c('ANY','ANY'),
	function(x, y, na.rm=TRUE, use=use, data=data) 
		stats::var( x, y, na.rm=na.rm, use=use) 
)

setMethod(
	'var',
	c('numeric','numeric'),
	function(x, y, na.rm=TRUE, use=use, data=data) 
		c( var=stats::var( x, y, na.rm=na.rm, use=use) )
)

setMethod(
	'var',
	c('numeric'),
	function(x, y, na.rm=TRUE, use=use, data=data) 
		c( var=stats::var( x, y, na.rm=na.rm, use=use) )
)

setMethod(
	'var',
	c('matrix'),
	function(x, y, na.rm=TRUE, use=use, data=data) 
		stats::var( x, y, na.rm=na.rm, use=use) 
)

setMethod( 
	"var", 
	signature=c("data.frame"),
	function(x, y, na.rm=TRUE, use=use) stats::var(x, y, na.rm=na.rm, use=use)
)

setMethod( 
	"var", 
	signature=c(x="formula", y="missing", na.rm='ANY', use='ANY', data="data.frame"),
	function(x, na.rm=TRUE, use, data=parent.frame()) {
		if( .is.simple.formula(x) ) {
			return( stats::var( eval( .simple.part(x), data ),  na.rm=na.rm, use=use ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=stats::var, na.rm=na.rm, use=use) )
		} 
	}
)

setMethod( 
	"var", 
	signature=c(x="formula", y="data.frame", na.rm='ANY', use='ANY', data="missing"),
	function(x, y=parent.frame(),  na.rm=TRUE, use) {
		data <- y
		if( .is.simple.formula(x) ) {
			return( stats::var( eval( .simple.part(x), data),  na.rm=na.rm, use=use ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=stats::var, na.rm=na.rm, use=use) )
		} 
	}
)
##########################################################################################

min <- .Min
max <- .Max

##########################################################################################
setGeneric('count',
	function(x, ..., level=TRUE, na.rm=TRUE) {
		dots <- list(...)
			if ( ! .is.formula(x) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
				data <- dots[[1]]
				return( callGeneric(eval( substitute(x), data), level=level, na.rm=na.rm) ) 
			}
		standardGeneric('count')
	}
)

setMethod(
	'count',
	'ANY',
	function(x, ..., level=level, na.rm=TRUE) 
		c( count=callGeneric( as.factor( .flatten(c(x,list(...))) ), level=level, na.rm=na.rm) )
)

setMethod('count',
	signature = c('logical'),
	function(x, ..., level=TRUE, na.rm=TRUE) 
		callGeneric( as.factor(.flatten(c(x, list(...)))), level=level, na.rm=na.rm ) 
)

setMethod('count',
	signature = 'factor',
	function(x, ..., level=TRUE, na.rm=TRUE) {
		if (! level %in% levels(x) ) {
			level = levels(x) [as.numeric(level)]
		}
		result <- sum( x == level, na.rm=na.rm ) 
		names(result) <- paste('count', level, sep=".")
		return(result)
	}
)

setMethod( 
	"count", 
	signature=c("data.frame"),
	function(x, ..., level=TRUE, na.rm=TRUE) 
		sapply(x, sum, level=level, na.rm=na.rm)
)

setMethod( 
	"count", 
	signature=c("formula"),
	function(x, data=parent.frame(), ..., level=level, na.rm=TRUE) {
		if( .is.simple.formula(x) ) {
			x <-  eval(.simple.part(x), data) 
			if (! level %in% levels(x) ) {
				level = levels(x) [as.numeric(level)]
			}
			result <- sum( x == level, na.rm=na.rm ) 
			names(result) <- paste('count', level, sep=".")
			return(result)
		} else {
			stop('Invalid formula type.  Perhaps you should try xtabs().')
			return( .mosaic_aggregate( x, data, FUN=count, ..., level=level, na.rm=na.rm ) )
		} 
	}
)

##########################################################################

setGeneric('prop',
	function(x, ..., level=TRUE, na.rm=TRUE) {
		dots <- list(...)
		if ( length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
			data <- dots[[1]]
			return(prop(eval( substitute(x), data), level=level, na.rm=na.rm))
		}
		standardGeneric('prop')
	}
)

setMethod(
	'prop',
	'ANY',
	function(x, ..., level=level, na.rm=TRUE) 
		c( prop=callGeneric( as.factor( .flatten(c(x,list(...))) ), level=level, na.rm=na.rm) )
)

setMethod('prop',
	signature = c('logical'),
	function(x, ..., level=TRUE, na.rm=TRUE) 
		callGeneric( as.factor( .flatten(c(x,list(...))) ), level=level, na.rm=na.rm ) 
)

setMethod('prop',
	signature = 'factor',
	function(x, ..., level=TRUE, na.rm=TRUE) {
		if (! level %in% levels(x) ) {
			level = levels(x) [as.numeric(level)]
		}
		result <- base::mean( x == level, na.rm=na.rm ) 
		names(result) <- paste('prop', level, sep=".")
		return(result)
	}
)

setMethod( 
	"prop", 
	signature=c("data.frame"),
	function(x, ..., level=TRUE, na.rm=TRUE) 
		sapply(x, prop, level=level, na.rm=na.rm)
)

setMethod( 
	"prop", 
	signature=c("formula"),
	function(x, data=parent.frame(), ..., level=level, na.rm=TRUE) {
		if( .is.simple.formula(x) ) {
			return( prop( eval( .simple.part(x), data, enclos=parent.frame()), 
							   ..., level=level, na.rm=na.rm ) )
		} else {
			stop('Invalid formula type.  Perhaps you should try xtabs().')
			return( .mosaic_aggregate( x, data, FUN=prop, ..., level=level, na.rm=na.rm ) )
		} 
	}
)

