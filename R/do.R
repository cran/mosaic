utils::globalVariables(c('.row'))
#' @importFrom mosaicCore vector2df nice_names
require(parallel)
require(profvis)
require(compiler)
require(rlang)
parallel::detectCores()
NA
  

#' Set seed in parallel compatible way
#'
#' When the parallel package is used, setting the RNG seed for reproducibility
#' involves more than simply calling [set.seed()]. `set.rseed` takes
#' care of the additional overhead.
#'
#' @param seed seed for the random number generator
#' @details
#' If the `parallel` package is not on the search path, then [set.seed()] is called.
#' If `parallel` is on the search path, then the RNG kind is set to `"L'Ecuyer-CMRG"`,
#' the seed is set and `mc.reset.stream` is called.
#' 
#' @examples
#' # These should give identical results, even if the `parallel' package is loaded.
#' set.rseed(123); do(3) * resample(1:10, 2)
#' set.rseed(123); do(3) * resample(1:10, 2)
#' @export

set.rseed <- function(seed) {
  if ("package:parallel" %in% search()) {
    set.seed(seed, kind = "L'Ecuyer-CMRG")
    parallel::mc.reset.stream()
  } else {
    set.seed(seed) 
  }
}


#' Do Things Repeatedly
#' 
#' `do()` provides a natural syntax for repetition tuned to assist 
#' with replication and resampling methods.
#'
#' @rdname do
#' @param n  number of times to repeat 
#' 
#' @param object an object
#'
#' @param cull  function for culling output of objects being repeated.  If NULL,
#'   a default culling function is used.  The default culling function is 
#'   currently aware of objects of types
#'   `lme`,
#'   `lm`,
#'   `htest`,
#'   `table`,
#'   `cointoss`, and 
#'   `matrix`.
#'   
#' @param mode  target mode for value returned
#' 
#' @param algorithm a number used to select the algorithm used.  Currently numbers below 1 
#'   use an older algorithm and numbers >=1 use a newer algorithm which is faster in some 
#'   situations.
#' @param parallel a logical indicating whether parallel computation should be attempted
#'   using the \pkg{parallel} package (if it is installed and loaded).
#' 
#' @param e1 an object (in cases documented here, the result of running `do`)
#' @param e2 an object (in cases documented here, an expression to be repeated)
#' @param ... additional arguments
#' 
#' @note `do` is a thin wrapper around `Do` to avoid collision with
#'   [dplyr::do()] from the \pkg{dplyr} package.
#' @return `do` returns an object of class `repeater` which is only useful in
#'   the context of the operator `*`.  See the examples.
#' @author Daniel Kaplan (\email{kaplan@@macalaster.edu})
#'   and Randall Pruim (\email{rpruim@@calvin.edu})
#'
#' @section Naming:
#' The names used in the object returned from `do()` are inferred from the
#' objects created in each replication.  Roughly, this the strategy employed.
#' 
#' * If the objects have names, those names are inherited, if possible.
#' * If the objects do not have names, but `do()` is used with a simple 
#' function call, the name of that function is used. 
#' Example: `do(3) * mean(~height, data = Galton)` produces a data frame with
#' a variable named `mean`.
#' * In cases where names are not easily inferred and a single result is produced,
#' it is named `result`.
#' 
#' To get different names, one can rename the objects as they are created, or 
#' rename the result returned from `do()`.  Example of the former:
#' `do(3) * c(mean_height = mean(~height, data = resample(Galton)))`.
#'  
#' @seealso [replicate()], [set.rseed()]
#' 
#' @examples
#' do(3) * rnorm(1)
#' do(3) * "hello"
#' do(3) * 1:4
#' do(3) * mean(rnorm(25))
#' do(3) * lm(shuffle(height) ~ sex + mother, Galton)
#' do(3) * anova(lm(shuffle(height) ~ sex + mother, Galton))
#' do(3) * c(sample.mean = mean(rnorm(25)))
#' # change the names on the fly
#' do(3) * mean(~height, data = resample(Galton))
#' do(3) * c(mean_height = mean(~height, data = resample(Galton)))
#' set.rseed(1234)
#' do(3) * tally( ~sex|treat, data=resample(HELPrct))
#' set.rseed(1234)  # re-using seed gives same results again
#' do(3) * tally( ~sex|treat, data=resample(HELPrct))
#' @keywords iteration 
#' @export

do <- function(object, ...) {
  UseMethod("do")
}

#' @rdname do
#' @export
do.numeric <- function(object, ...) {
  Do(n=object, ...)
}

#' @rdname do
#' @export
do.default <- function(object, ...) {
  dplyr::do(object, ...)
}

#' @rdname do
#' @export
Do <- function(n=1L, cull=NULL, mode='default', algorithm=1.0, parallel=TRUE) {
	new( 'repeater', n=n, cull=cull, mode=mode, algorithm=algorithm, parallel=parallel)
}

#' @rdname mosaic-internal
#' @keywords internal
#' @details
#' `.make.data.frame` converts things to a data frame
#' @param x object to be converted
#' @return a data frame

.make.data.frame <- function( x ) {
	if (is.data.frame(x)) return(x)
	if (is.vector(x)) {
		nn <- names(x)
		result <- as.data.frame( matrix(x, nrow=1) )
		if (! is.null(nn) ) names(result) <- nn
		return(result)
	}
	return(as.data.frame(x))
	}



null2na <- function(x) if (is.null(x)) NA else x

#' Repeater objects
#'
#' Repeater objects can be used with the `*` operator to repeat
#' things multiple time using a different syntax and different output
#' format from that used by, for example, [replicate()].
#'
#' @rdname repeater-class
#' @name repeater-class
#' @seealso [do()]
#' @section Slots:
#' \describe{
#'   \item{`n`:}{Object of class `"numeric"` indicating how many times to repeat something.}
#'   \item{`cull`:}{Object of class `"function"` that culls the output from each repetition.}
#'   \item{`mode`:}{Object of class `"character"` indicating the output mode 
#'   ('default', 'data.frame', 'matrix', 'vector', or 'list').  For most purposes 'default' (the default)
#'   should suffice.}
#'   \item{`algorithm`:}{an algorithm number.}
#'   \item{`parallel`:}{a logical indicating whether to attempt parallel execution.}
#' }
#' @exportClass repeater

setClass('repeater', 
	representation = representation(n='numeric', cull='ANY', mode='character', 
                                  algorithm='numeric', parallel='logical'),
	prototype = prototype(n=1, cull=NULL, mode="default", algorithm=1, parallel=TRUE)
)


# old version
if(FALSE) {
.merge_data_frames <- function(a, b) {
  a <- .make.data.frame(a)
  b <- .make.data.frame(b)
  if (nrow(b) < 1) return (a) 
  if (nrow(a) < 1) return (b) 

	a$mosaic_merge_id <- paste('A',1:nrow(a))
	b$mosaic_merge_id <- paste('B',1:nrow(b))
	result <- merge(a,b,all=TRUE)
	w <- which(names(result) == 'mosaic_merge_id')
	result <- result[, -w]
	return(result)
}
}


#' @rdname mosaic-internal
#' @keywords internal
#' @details `.merge_data_frames` is a wrapper around merge
#'
#' @param a a data frame
#' @param b a data frame
#'
#' @return a data frame 

.merge_data_frames = function(a,b) {
  a <- .make.data.frame(a)
  b <- .make.data.frame(b)
  if (nrow(b) < 1) return (a) 
  if (nrow(a) < 1) return (b) 
  missing.from.b = setdiff(names(a),names(b))
  missing.from.a = setdiff(names(b),names(a))
  for (var in missing.from.b) b[[var]] = NA
  for (var in missing.from.a) a[[var]] = NA
  dplyr::bind_rows(a,b)
}


#' @rdname mosaic-internal
#' @keywords internal
#' @details 
#' `.squash_names` squashes names of a data frame into a single string
#'
#' @param object an object
#' @param sep a character
#'
#' @return a character vector

.squash_names <- function(object,sep=":") {
	if ( ncol(object) < 1 ) {return(rep("",nrow(object)))}

	result <- object[,1]
	if ( ncol(object) < 2 ) {return(as.character(result))}

	for (c in 2:ncol(object)) {
		result <- paste(result, as.character(object[,c]), sep=sep)
	}
	return(result)
		
}


#' @rdname do
#' @param x an object created by `do`.
#' @export
print.repeater <- function(x, ...) 
    {
  		message(paste('This repeats a command',x@n,'times. Use with *.'))
  		return(invisible(x))
    }

.list2tidy.data.frame <- function (l) {
  
  # see if we really just have a vector
  ul <- unlist( l )
  if ( length(ul) == length(l) ) {
    result <- data.frame(..result.. = as.vector(ul))
    row.names(result) <- NULL
    if( !is.null(names(l[[1]])) ) names(result) <- names(l[[1]])
    return(result)
  }
  
  # if each element is a data frame, combine them with bind_rows
  if ( all( sapply( l, is.data.frame ) ) ) {
    return(
      lapply(l, function(x) {mutate(x, .row= 1:n())}) |> 
        dplyr::bind_rows() |> 
        mutate(.index = c(1, 1 + cumsum( diff(.row) != 1 ))) 
    )
  }
  
  # If rbind() works, do it
  tryCatch(
    return ( as.data.frame( do.call( rbind, l) ) ),
    error=function(e) {} 
  )
  
  if (all (sapply(l, length) ) == length(l[[1]]) ) {
    result <-  as.data.frame( matrix( ul, nrow=length(l) ) )
    names(result) <- names(l[[1]])
    return(result)
  }
  
  # nothing worked.  Just return the list as is.
  return( l )
}

#' Cull objects used with do()
#' 
#' The [do()] function facilitates easy replication for
#' randomization tests and bootstrapping (among other things).  Part of what
#' makes this particularly useful is the ability to cull from the objects
#' produced those elements that are useful for subsequent analysis. 
#' `cull_for_do` does this culling.  It is generic, and users
#' can add new methods to either change behavior or to handle additional
#' classes of objects.
#' 
#' @param object an object to be culled
#' @param ... additional arguments (currently ignored)
#' 
#' @details When `do(n) * expression` is evaluated, `expression`
#' is evaluated `n` times to produce a list of `n` result objects.
#' `cull_for_do` is then applied to each element of this list to 
#' extract from it the information that should be stored.  For example,
#' when applied to a object of class `"lm"`,
#' the default `cull_for_do` extracts the coefficients, coefficient
#' of determinism, an the estimate for the variance, etc.
#' 
#' @export 
#' @examples
#' cull_for_do(lm(length ~ width, data = KidsFeet))
#' do(1) * lm(length ~ width, data = KidsFeet)


cull_for_do <- function(object, ...) {
  UseMethod("cull_for_do")
}


#' @export 
cull_for_do.default <- function(object, ...) {
  object
}

#' @export
cull_for_do.fitdistr <- function(object, ...) {
  est <- object$estimate
  names(est) <- paste0(names(est), ".est")
  se <- object$sd
  names(se) <- paste0(names(se), ".se")
  c(est, se)
}

#' @export 
cull_for_do.aov <- function(object, ...) {
  cull_for_do(anova(object))
}
  
#' @export 
cull_for_do.anova <- function(object, ...) {  
    res <- as.data.frame(object)
    res <- cbind (data.frame(source=row.names(res)), res)
    names(res)[names(res) == "Df"] <- "df"
    names(res)[names(res) == "Sum Sq"] <- "SS"
    names(res)[names(res) == "Mean Sq"] <- "MS"
    names(res)[names(res) == "F value"] <- "F"
    names(res)[names(res) == "Pr(>F)"] <- "pval"
    names(res)[names(res) == "Sum of Sq"] <- "diff.SS"
    names(res)[names(res) == "Res.Df"] <- "res.df"
    return(res)
    return( data.frame(
      SSTotal= sum(object$`Sum Sq`),
      SSModel= object$`Sum Sq`[1],
      SSError= object$`Sum Sq`[2],
      MSTotal= sum(object$`Sum Sq`),
      MSModel= object$`Mean Sq`[1],
      MSError= object$`Mean Sq`[2],
      F=object$`F value`[1],
      dfModel=object$Df[1],
      dfError=object$Df[2],
      dfTotal=sum(object$Df)
    ) )
}

#' @export 
cull_for_do.table <- function(object, ...) {
  result <- data.frame(object)
  res <- result[[ncol(result)]]
  nms <- as.character(result[[1]])
  if (ncol(result) > 2) {
    for (k in 2:(ncol(result)-1)) {
      nms <- paste(nms, result[[k]],sep=".")
    }
  }
  names(res) <- nms
  return(res)
}

#' @export 
cull_for_do.aggregated.stat <- function(object, ...) {
  result <- object
  res <- as.vector(result[, "S"])  # ncol(result)]
  names(res) <- 
    paste( attr(object, 'stat.name'), 
           .squash_names(object[,1:(ncol(object)-3),drop=FALSE]), sep=".")
  return(res)
} 

#' @export 
cull_for_do.lme <- function(object, ...) {
  result <- object
  names(result) <- mosaicCore::nice_names(names(result))
  return( object$coef$fixed )
}

#' @export 
cull_for_do.lm <- function(object, ...) {
  sobject <- summary(object)
  Fstat <- sobject$fstatistic[1]
  DFE <- sobject$fstatistic["dendf"]
  DFM <- sobject$fstatistic["numdf"]
  if (!is.null(Fstat)) {
    names(Fstat) <- "F"
    result <-  c(coef(object), sigma=sobject$sigma, 
                 r.squared = sobject$r.squared, 
                 Fstat,
                 DFM,
                 DFE)
  } else {
    result <-  c(coef(object), sigma=sobject$sigma, 
                 r.squared = sobject$r.squared
    )
  }
  mosaicCore::vector2df(result, nice_names = TRUE)
}

#  @export 
# cull_for_do.groupwiseModel <- function(object, ...) {
#   sobject <- summary(object)
#   Fstat <- sobject$fstatistic[1]
#   DFE <- sobject$fstatistic["dendf"]
#   DFM <- sobject$fstatistic["numdf"]
#   if (!is.null(Fstat)) {
#     names(Fstat) <- "F"
#     result <-  c(coef(object), sigma=sobject$sigma, 
#                  r.squared = sobject$r.squared, 
#                  Fstat,
#                  DFM,
#                  DFE)
#   } else {
#     result <-  c(coef(object), sigma=sobject$sigma, 
#                  r.squared = sobject$r.squared
#     )
#   }
#   names(result) <- nice_names(names(result))
#   return(result)
# }
#   
  
#' @export 
cull_for_do.htest <- function(object, ...) {
  if (is.null(object$conf.int)) {
    result <-  data.frame( 
      statistic = null2na(object$statistic), 
      parameter = null2na(object$parameter),
      p.value = null2na(object$p.value),
      method = null2na(object$method),
      alternative = null2na(object$alternative),
      data = null2na(object$data.name)
    )
  } else {
    result <-  data.frame( 
      statistic = null2na(object$statistic), 
      parameter = null2na(object$parameter),
      p.value = null2na(object$p.value),
      conf.level = attr(object$conf.int,"conf.level"),
      lower = object$conf.int[1],
      upper = object$conf.int[2],
      method = null2na(object$method),
      alternative = null2na(object$alternative),
      data = null2na(object$data.name)
    )
  }
  if ( !is.null(names(object$statistic)) ) 
    names(result)[1] <-  names(object$statistic)
  if ( !is.null(names(object$parameter)) ) 
    names(result)[2] <- names(object$parameter)
  return(result)
}

#   if (inherits(object, 'table') ) {
#     nm <- names(object)
#     result <-  as.vector(object)
#     names(result) <- nm
#     return(result)
#   }

#' @export 
cull_for_do.cointoss <- function(object, ...) {
  return( c(n=attr(object,'n'), 
            heads=sum(attr(object,'sequence')=='H'),
            tails=sum(attr(object,'sequence')=='T'),
            prop=sum(attr(object,'sequence')=="H") / attr(object,'n')
  ) )
}

#' @export 
cull_for_do.matrix <- function(object, ...) {
  if (ncol(object) == 1) {
    nn <- rownames(object)
    object <- as.vector(object)
    if (is.null(nn)) {
      names(object) <- paste('v',1:length(object),sep="")
    } else {
      names(object) <- nn
    }
    return(object)
  }
  if (nrow(object) > 1) {
    res <- as.data.frame(object)
    res[[".row"]] <- row.names(object)
    return(res)
  }
  # if we get here, we have a 1-row or empty matrix
  row.names(object) <- NULL
  object
}

#' @rdname do
#' @aliases *,repeater,ANY-method
#' @export

setMethod(
  "*",
  signature(e1 = "repeater", e2="ANY"),
  function (e1, e2) 
  {
    e2_lazy <- rlang::enquo(e2)
    #		e2unevaluated = substitute(e2)
    #		if ( ! is.function(e2) ) {
    #      frame <- parent.frame()
    #			e2 = function(){eval(e2unevaluated, envir=frame) }   
    #		}
    n = e1@n
    
    cull = e1@cull
    if (is.null(cull)) {
      cull <- cull_for_do
    }
    
    out.mode <- if (!is.null(e1@mode)) e1@mode else 'default'
    
    resultsList <- if( e1@parallel && "package:parallel" %in% search() ) {
      if (getOption("mosaic:parallelMessage", TRUE)) {
        message("Using parallel package.\n",
                "  * Set seed with set.rseed().\n", 
                "  * Disable this message with options(`mosaic:parallelMessage` = FALSE)\n")
      }
      parallel::mclapply( integer(n), function(...) { cull(rlang::eval_tidy(e2_lazy)) } )
    } else {
      lapply( integer(n), function(...) { cull(rlang::eval_tidy(e2_lazy)) } )
    }
    
    if (out.mode=='default') {  # is there any reason to be fancier?
      out.mode = 'data.frame'
    }
    
    result <- switch(out.mode, 
                     "list" = resultsList,
                     "data.frame" = .list2tidy.data.frame( resultsList ),
                     "matrix" = as.matrix( do.call( rbind, resultsList) ),
                     "vector" = unlist(resultsList)  
    ) 
    class(result) <- c(paste('do', class(result)[1], sep="."), class(result))
    if (inherits( result, "data.frame")) {
      # we get mutliple parts here if expression involves, for example, ::
      # just grab last part. (paste()ing would be out of order
      alt_name <- tryCatch(
        tail(as.character(rhs(e2_lazy)[[1]]), 1),
        error = function(e) "result"
      )
      names(result) <- mosaicCore::nice_names(names(result))
      names(result)[names(result) == "..result.."] <- 
        if(mosaicCore::nice_names(alt_name) == alt_name) alt_name else "result"
    }
    attr(result, "lazy") <- e2_lazy
    if (out.mode == "data.frame") attr(result, "culler") <- cull
    return(result)
  })
