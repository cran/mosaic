utils::globalVariables(c('.'))
require(rlang)
#' @importFrom mosaicCore mosaic_formula_q mosaic_formula

# evaluate a lazy object and return the unevaluated expression if the expression
# doesn't evaluate to an existing object.

safe_eval <- function(x) {
  tryCatch(rlang::eval_tidy(x), 
           error = function(e) x$expr)
}


.fetchFromDots <- function( dots, name, class = 'data.frame', n = 1, default = NULL ) {
  result <- dots[[name]]
  if (is.null(result)) {
    if (length(result) < n) return(default)
    result <- dots[[n]]
    if (! inherits(result, 'class') ) result <- default
  }
  return(result)
}

#' Check if formula
#' 
#' @param x an object
#' @return TRUE for a formula, FALSE otherwise, even if evaluation throws an error
#'
#' @rdname mosaic-internal
#' @keywords internal

.is.formula <- function(x)  
  tryCatch( inherits(x, 'formula'), error = function(e) {FALSE} )

#' Check for simple formula
#'
#' @param x a formula
#'
#' @return TRUE if formula has no left-hand side or a simple right-hand side 
#' (e.g., `NULL`, ., 1,  or 0)
#'
#' @rdname mosaic-internal
#' @keywords internal
.is.simple.formula <-  function(x){
  inherits(x, "formula") &&
    (length(x) == 2 || is.null(x[[3]]) ||
       (length(x[[3]]) == 1 &&
          ((is.numeric(x[[3]]) && (x[[3]] == 0 || x[[3]] == 1)) ||  
             (all.names(x[[3]]) %in% c(".")))))
}

# This could use a better name and a better description



#' Extract simple part from formula
#'
#' @param x a formula
#'
#' @return simple part of formula or NULL if formula is not simple
#'
#' @rdname mosaic-internal
#' @keywords internal

.simple.part <- function(x) {
  if (! .is.simple.formula(x) ) {
    return(NULL) 
  } else {
    return(x[[2]])
  }
}

.flatten <- function(x) {
  result <- c()
  for (item in x) result <- c(result, item)
  return(result)
}


#' Aggregate for mosaic
#'
#' Compute function on subsets of a variable in a data frame.
#'
#' @rdname aggregatingAux
#' @return  a vector
#' @param formula a formula.  Left side provides variable to be summarized.  Right side and condition
#'                            describe subsets.  If the left side is empty, right side and condition are
#'                            shifted over as a convenience.
#' @param data a data frame.  
#' Note that the default is `data = parent.frame()`.  This makes it convenient to
#' use this function interactively by treating the working environment as if it were 
#' a data frame.  But this may not be appropriate for programming uses.  
#' When programming, it is best to use an explicit `data` argument
#' -- ideally supplying a data frame that contains the variables mentioned
#' in `formula`.
#' @param FUN a function to apply to each subset 
#' @param groups grouping variable that will be folded into the formula (if there is room for it).  
#' This offers some additional flexibility in how formulas can be specified.
#' @param subset a logical indicating a subset of `data` to be processed.
#' @param drop a logical indicating whether unused levels should be dropped.
#' @param \dots additional arguments passed to `FUN`
#' @param .format format used for aggregation. `"default"` and `"flat"` are equivalent.  
# #' @param format format used for aggregation. \code{"default"} and \code{"flat"} are equivalent.  
# #'   Ignored  if \code{.format} is not \code{NULL}.
#' @param .overall currently unused
#' @param .name a name used for the resulting object
# #' @param name a name used for the resulting object.  Ignored if \code{.format} is not \code{NULL}.
#' @param .envir an environment in which to evaluate expressions 
# #' @param envir an environment in which to evaluate expressions. 
# #'   Ignored if \code{.envir} is not \code{NULL}.
#' @param .multiple a logical indicating whether FUN returns multiple values
# #' @param multiple a logical indicating whether FUN returns multiple values
#'   Ignored if `.multiple` is not `NULL`.
#'
#' @examples
#' if (require(mosaicData)) {
#' maggregate( cesd ~ sex, HELPrct, FUN = mean )
#' # using groups instead
#' maggregate( ~ cesd, groups = sex, HELPrct, FUN = sd )
#' # the next four all do the same thing
#' maggregate( cesd ~ sex + homeless, HELPrct, FUN = mean )
#' maggregate( cesd ~ sex | homeless, HELPrct, FUN = sd )
#' maggregate( ~ cesd | sex , groups= homeless, HELPrct, FUN = sd )
#' maggregate( cesd ~ sex, groups = homeless, HELPrct, FUN = sd )
#' # this is unusual, but also works.
#' maggregate( cesd ~ NULL , groups = sex, HELPrct, FUN = sd )
#' }
#'
#' @export
maggregate <- 
  function(
    formula, 
    data = parent.frame(), 
    FUN, 
    groups = NULL, 
    subset, 
    drop = FALSE, 
    ...,
    .format = c('default', 'table', 'flat'), 
    .overall = mosaic.par.get("aggregate.overall"), 
    # .multiple = NULL,
    .multiple = FALSE, 
    # .name = NULL,
    .name = deparse(substitute(FUN)), 
    # .envir = NULL,
    .envir = parent.frame () 
    ) {
 
  # if (inherits(formula, c("environment", "data.frame")) && 
  #     inherits(data, "formula")) {
  #   temp <- formula
  #   formula <- data
  #   data <- temp
  # }
  #   
  if (! inherits(data, c("environment", "data.frame")) ) {
    if (inherits(data, c("tbl")))
      stop ("Your tbl is not a data.frame.  Perhaps you need dplyr functions here.",
            call. = FALSE)
    else 
      stop("data must be an environment or data.frame.", call. = FALSE)
  }
  formula <- 
    mosaicCore::mosaic_formula_q(formula, groups = !!rlang::enexpr(groups), envir = .envir)
  
  if (length(formula) == 2) { 
    return(FUN( eval(formula[[2]], data, .envir), ...))
  }
  
  dots <- list(...)
  groupName <- ".group"  # gets changed to something better later when possible.
  
  .format <- match.arg(.format, c('default', 'table', 'flat'))
  evalF <- mosaicCore::evalFormula(formula, data = data)
  
  if (!missing(subset)) {
    subset <- eval(substitute(subset), data, environment(formula))
    if (!is.null(evalF$left))           evalF$left <- evalF$left[subset, ]
    if (!is.null(evalF$right))         evalF$right <- evalF$right[subset, ]
    if (!is.null(evalF$condition)) evalF$condition <- evalF$condition[subset, ]
  }
  # this should now be standardized by the call to mosaic_formula_q() above.
  #  if ( is.null( evalF$left ) ) {
  #    evalF$left <- evalF$right
  #    evalF$right <- evalF$condition
  #    evalF$condition <- NULL
  #  }
  
  if ( is.null(evalF$left) || ncol(evalF$left) < 1 )  {
    if (ncol(evalF$right) > 1) warning("Too many variables in rhs; ignoring all but first.")
    if (.format == "table") {
      if (.multiple) stop ("table view unavailable for this function.")
      ldata <- evalF$right[ , 1, drop = FALSE]
      gdata <- group_by(data)
      res <- as.data.frame(
        dplyr::do(gdata, foo = FUN( as.data.frame(.)[, 1], ...) ) )
      names(res)[ncol(res)] <- gsub(".*::", "", .name)
      return(res)
      
      return(evalF$right[ , 1, drop = FALSE] |> 
               group_by() |>
               dplyr::do( do.call(FUN, list(evalF$right[, 1], ...)) ) |>
               as.data.frame()
      )
      #      return(plyr::ddply(evalF$right[ , 1, drop = FALSE], names(NULL),
      #                   function(x) do.call(FUN, list(evalF$right[, 1], ...)) 
      #      )[, -1])  # remove the .id column since it is uninteresting here.
    }
    return( do.call(FUN, alist(evalF$right[, 1], ...) ) )
  } else {
    if (ncol(evalF$left) > 1) warning("Too many variables in lhs; ignoring all but first.")
    if (.format == 'table') {
      if (.multiple) stop ("table view unavailable for this function.")
      ldata <- mosaicCore::joinFrames(evalF$left[ , 1, drop = FALSE], evalF$right, evalF$condition) 
      ldata$.var <- ldata[, 1]
      gdata <- do.call( group_by, c(list(ldata),  
                                    lapply(union(names(evalF$right), names(evalF$condition)),
                                           as.name )) )
      res <- as.data.frame(
        dplyr::do(gdata, foo = FUN( as.data.frame(.)[, 1], ...) ) )
      names(res)[ncol(res)] <- gsub(".*::", "", .name)
      #      res <-  plyr::ddply( 
      #        mosaicCore::joinFrames(evalF$left[ , 1, drop = FALSE], evalF$right, evalF$condition), 
      #        union(names(evalF$right), names(evalF$condition)),
      #        function(x) do.call(FUN, list(x[, 1], ...))
      #      )
      
    } else {
      res <- lapply( split( evalF$left[, 1], 
                            mosaicCore::joinFrames(evalF$right, evalF$condition), 
                            drop = drop),
                     function(x) { do.call(FUN, alist(x, ...) ) }
      )
      groupName <- paste(c(names(evalF$right), names(evalF$condition)), collapse = ".")
      
      if (! .multiple ) res <- unlist(res)
      
      if (! is.null(evalF$condition) ) {
        if (ncol(evalF$left) > 1) message("Too many variables in lhs; ignoring all but first.")
        res2 <- lapply( split( evalF$left[, 1], evalF$condition, drop = drop),
                        function(x) { do.call(FUN, alist(x, ...) ) }
        )
        groupName <- paste(names(evalF$condition), collapse = ".")
        if (!.multiple) {
          res <- c( res, unlist(res2) )
        } else {
          res <- c(res, res2)
        }
      }
      if (.multiple) {
        result <- res
        res <- result[[1]]
        for (item in result[-1]) {
          res <- as.data.frame(rbind(res, item))
        }
        if ( nrow(res) == length(names(result)) ) {
          res[groupName] <- names(result)
        } else {
          res[groupName] <- rep(names(result), each = nrow(res) / length(names(result)) )
        }
        res <- res[, c(ncol(res), 1:(ncol(res) -1))]
      }
    }
  }
  w <- grep("V[[:digit:]]+",  names(res))
  if (length(w) == 1) {
    names(res)[w] <- gsub(".*:{2,3}", "", .name)
  } else {
    names(res)[w] <- paste0( gsub(".*:{2,3}", "", .name), 1:length(w) )
  }
  row.names(res) <- NULL
  return( res )
}

# for handling functions of two inputs
# under construction still

maggregate2 <- function(formula, data = parent.frame(), FUN, subset, 
                        overall = mosaic.par.get("aggregate.overall"), 
                        .format = c('default', 'table', 'flat'), drop = FALSE, 
                        .multiple = FALSE, 
                        groups = NULL, 
                        .name = deparse(substitute(FUN)), 
                        ...) {
  dots <- list(...)
  formula <- 
    mosaicCore::mosaic_formula_q(formula, groups = !!rlang::enexpr(groups), as.environment(data))
  
  .format <- match.arg(.format)
  
  evalF <- mosaicCore::evalFormula(formula, data = data)
  
  if (!missing(subset)) {
    subset <- eval(substitute(subset), data, environment(formula))
    if (!is.null(evalF$left))           evalF$left <- evalF$left[subset, ]
    if (!is.null(evalF$right))         evalF$right <- evalF$right[subset, ]
    if (!is.null(evalF$condition)) evalF$condition <- evalF$condition[subset, ]
  }
  # this should now be standardized by the call to mosaic_formula_q() above.
  #  if ( is.null( evalF$left ) ) {
  #    evalF$left <- evalF$right
  #    evalF$right <- evalF$condition
  #    evalF$condition <- NULL
  #  }
  
  if ( is.null(evalF$left) || ncol(evalF$left) < 1 )  
    stop("formula must have lhs.")
  
  if (ncol(evalF$left) > 1) stop("Too many variables in lhs.")
  
  if (.format == 'table') {
    if (.multiple) stop ("table view unavailable.")
    ldata <- mosaicCore::joinFrames(evalF$left[ , 1, drop = FALSE], evalF$right, evalF$condition) 
    ldata$.var <- ldata[, 1]
    gdata <- do.call( group_by, c(list(ldata),  as.name(names(evalF$condition)) ) )
    res <- as.data.frame( dplyr::do(gdata, foo = FUN( as.data.frame(.)[, 1], as.data.frame(.)[, 2], ...) ) )
    names(res)[ncol(res)] <- gsub(".*::", "", .name)
  } else {
    res <- lapply( split( evalF$left[, 1], 
                          mosaicCore::joinFrames(evalF$right, evalF$condition), 
                          drop = drop),
                   function(x) { do.call(FUN, alist(x, ...) ) }
    )
    groupName <- paste(c(names(evalF$right), names(evalF$condition)), collapse = ".")
    
    if (! .multiple ) res <- unlist(res)
    
    if (! is.null(evalF$condition) ) {
      if (ncol(evalF$left) > 1) message("Too many variables in lhs; ignoring all but first.")
      res2 <- lapply( split( evalF$left[, 1], evalF$condition, drop = drop),
                      function(x) { do.call(FUN, alist(x, ...) ) }
      )
      groupName <- paste(names(evalF$condition), collapse = ".")
      if (!.multiple) {
        res <- c( res , unlist(res2) )
      } else {
        res <- c(res, res2)
      }
    }
    if (.multiple) {
      result <- res
      res <- result[[1]]
      for (item in result[-1]) {
        res <- as.data.frame(rbind(res, item))
      }
      if ( nrow(res) == length(names(result)) ) {
        res[groupName] <- names(result)
      } else {
        res[groupName] <- rep(names(result), each = nrow(res) / length(names(result)) )
      }
      res <- res[, c(ncol(res), 1:(ncol(res) -1))]
    }
  }
  
  w <- grep("V[[:digit:]]+",  names(res))
  if (length(w) == 1) {
    names(res)[w] <- gsub(".*:{2,3}", "", .name)
  } else {
    names(res)[w] <- paste0( gsub(".*:{2,3}", "", .name), 1:length(w) )
  }
  row.names(res) <- NULL
  return( res )
}


