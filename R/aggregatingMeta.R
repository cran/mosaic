globalVariables(c("FUNCTION_TBD", "NA.RM", "OUTPUT.MULTIPLE"))
require(rlang)

#' 1-ary Aggregating functions
#' 
#' `aggregatinFuntion1` creates statistical summaries of one numerical vector that are formula aware.
#' 
#' @import rlang
#' @rdname aggregatingFunction1
#' @aliases aggregatingFunction1 
#' @param fun a function that takes a numeric vector and computes a summary statistic,
#' returning a numeric vector.
#' @param output.multiple a boolean indicating whether `fun` returns multiple values
#' @param envir an environment in which evaluation takes place.
#' @param na.rm the default value for na.rm in the resulting function.
#' @param style one of `"formula1st"`, `"formula2nd"` or `"flexible"`.  In the first
#' two cases, the first argument must be a formula or evaluate to an object.  In the latter case, 
#' bare names will be converted into formulas.
#' @return a function that generalizes `fun` to handle a formula/data frame interface.
#' 
#' @details The logic of the resulting function is this: 1) If the first argument is a formula,
#' use that formula and `data` to create the necessary call(s) to `fun`; (2) Else simply
#' pass everything to `fun` for evaluation.
#' 
#' @section Note: 
#' Earlier versions of this function supported a "bare name + data frame" interface.  This
#' functionality has been removed since it was (a) ambiguous in some cases, (b) unnecessary, and (c) 
#' difficult to maintain.
#' 
#' @examples
#' if (require(mosaicData)) {
#'   foo <- aggregatingFunction1(base::mean)
#'   foo( ~ length, data = KidsFeet)
#'   base::mean(KidsFeet$length)
#'   foo(length ~ sex, data = KidsFeet)
#' } 
#' @export
# 

# Testing a possible replacement for aggregatingFunction1
# Might be masked below by the old one -- be sure to check.

aggregatingFunction1 <- 
  function(fun, 
           output.multiple = FALSE, 
           envir = parent.frame(), 
           na.rm = getOption("na.rm", FALSE),
           style = c("formula1st", "formula", "flexible")
  ) {
    fun <- deparse(substitute(fun))
    style = match.arg(style)
    templates <- list(
      flexible = # this should probably be removed in the near future.
        function(
          x, ..., 
          data = NULL,
          groups = NULL, 
          na.rm = NA.RM)  ## getOption("na.rm", FALSE)) 
        {
          pframe <- parent.frame()
          subst_x <- substitute(x)
          lazy_formula <- 
            tryCatch(
              rlang::enquo(x),
              error = function(e) {
                if (grepl("Promise has already been forced", e$message) ||
                    # next line can be deleted when lazyeval updates on CRAN
                    grepl("e of NULL environment", e$message) 
                ) 
                  NULL
                else 
                  stop(e)
              }
            )
          ## See issue #531 for problem with using lazy_formula.
          if (is.null(lazy_formula) || ! inherits(lazy_formula$expr, "formula")) {
            lazy_formula <- structure(list(expr = subst_x, env = pframe), class = "lazy")
          }
          if (is.null(data)) {
            result <-
              tryCatch(FUNCTION_TBD(x, ..., na.rm = na.rm), 
                       error = function(e) {e} , 
                       warning = function(w) {w}) 
            if (! inherits(result, c("error", "warning")))  return(result) 
            data <- parent.frame()
          }
          formula <- formularise(lazy_formula, parent.frame(2)) 
          formula <- 
            mosaicCore::mosaic_formula_q(formula, groups = !!rlang::enexpr(groups), max.slots = 3) 
          maggregate(formula, data = data, FUN = FUNCTION_TBD, na.rm = na.rm, ..., 
                     .multiple = output.multiple)
        },
      formula = 
        function(
          x, ..., 
          data = NULL,
          groups = NULL, 
          na.rm = NA.RM)  ## getOption("na.rm", FALSE)) 
        {
          if (is.null(data)) {
            result <-
              tryCatch(FUNCTION_TBD(x, ..., na.rm = na.rm), error = function(e) {e} , warning = function(w) {w} ) 
            if (! inherits(result, c("error", "warning")))  return(result) 
            data <- parent.frame()
          }
          if (! inherits(x, "formula")) stop("`x' must be a formula")
          formula <- 
            mosaicCore::mosaic_formula_q(x, groups = !!rlang::enexpr(groups), max.slots = 3) 
          maggregate(formula, data = data, FUN = FUNCTION_TBD, ..., .multiple = output.multiple)
        },
      
      formula1st = 
        function(
          x, ..., 
          data = NULL,
          groups = NULL, 
          na.rm = NA.RM)  ## getOption("na.rm", FALSE)) 
        {
          if (rlang::is_formula(x)) {
            if (is.null(data)) data <- environment(x)
            formula <- 
              mosaicCore::mosaic_formula_q(x, groups = !!rlang::enexpr(groups), max.slots = 3) 
            
            return(maggregate(formula, data = data, FUN = FUNCTION_TBD, ..., 
                              na.rm = na.rm,
                              .multiple = OUTPUT.MULTIPLE))
          }
          FUNCTION_TBD(x, ..., na.rm = na.rm)
        }
    )
    
    fun_text <- deparse(templates[[style]])
    fun_text <- gsub("FUNCTION_TBD", fun, fun_text) 
    fun_text <- gsub("OUTPUT.MULTIPLE", output.multiple, fun_text)
    if (missing(na.rm)) {
      fun_text <- gsub("NA.RM", deparse(substitute(getOption("na.rm", FALSE))), fun_text)
    } else {
      fun_text <- gsub("NA.RM", deparse(substitute(na.rm)), fun_text)
    }
    res <- eval(parse(text = fun_text))
    environment(res) <- parent.frame()
    res
  } 


# #' @export
# aggregatingFunction1or2 <- function(fun, input.multiple = FALSE, output.multiple = FALSE, 
#                                      envir = parent.frame(), na.rm = getOption("na.rm",FALSE)) {
#   result <- function(x, ..., data, groups = NULL) {
#     orig.call <- match.call()
#     fun.call <- orig.call 
#     fun.call[[1]] <- substitute(..fun..)
#     fun.call[[2]] <- substitute(x)
#     
#     # if data is not given, we will try to evaluate fun()
#     missingData <- FALSE  
#     if (missing(data)) {
#       missingData <- TRUE
#       data = parent.frame()
#       
#       result <- tryCatch(eval(fun.call, envir = parent.frame()) , 
#                           error = function(e) {e} ,
#                           warning = function(w) {w}) 
#       if ( ! inherits(result, "warning") && ! inherits(result,"error")) {
#         return(result) 
#       }
#     }
#     # either data was specified or fun() generated an error.
#     # so we will generate a new call.
#     maggregate.call <- orig.call  
#     x_name <- substitute(x)
#     if (! .is.formula(x)) {
#       if ( !missingData) {
#         fun.call[['data']] <- NULL
#       }
#       if (input.multiple) {
#         result <- tryCatch(eval(fun.call, envir = data, enclos = parent.frame()),
#                             error = function(e) {e},
#                             warning = function(w) {w}) 
#         if ( ! inherits(result, "warning") && ! inherits(result,"error")) {
#           return(result) 
#         }
#       }
#       
#       x <- eval(substitute( 
#         mosaicCore::mosaic_formula_q(.x, groups = quote(groups)), 
#         list(.x = substitute(x) , .g = substitute(groups))
#       ) )
#       
#       if ("groups" %in% names(maggregate.call)) maggregate.call[['groups']] <- NULL
#       
#     }
#     # now x is a formula
#     
#     maggregate.call[[1]] <- quote(maggregate)
#     maggregate.call$formula <- x
#     maggregate.call$data <- data 
#     maggregate.call$x <- NULL
#     maggregate.call$FUN <- substitute(..fun..)  # keep substitute here or no?
#     maggregate.call$.multiple <- output.multiple
#     maggregate.call$na.rm <- substitute(na.rm)
#     #    print(maggregate.call)
#     return(eval(maggregate.call, envir = envir))
#   }
#   formals(result) <- c(formals(result), ..fun.. = substitute(fun), na.rm = substitute(na.rm))
#   return(result)
# }

#' 1- or 2-ary aggregating functions
#' 
#' `aggregatingFunction1or2()` creates statistical summaries for functions like
#' [var()] that can have either 1 or 2 numeric vector inputs.
#' 
#' @param fun a function that takes 1 or 2 numeric vectors and computes a summary statistic,
#' returning a numeric vector of length 1.
#' @param na.rm the default value for na.rm in the resulting function.
#' @param output.multiple a boolean indicating whether `fun` returns multiple values

#' 
#' @details This was designed primarily to support `var` which can be used to compute
#' either the variance of one variable or the covariance of two variables.
#' The logic of the resulting function is this: 1) If the first two arguments are both formulas,
#' then those formulas are evaluated (with `data`) to compute the covariance; 
#' (2) If the first argument is a formula, and the second is `NULL`, 
#' then the formula and `data` are used to create the necessary call(s) to `fun`; 
#' (3) Else everything is simply passed to `fun` for evaluation.
#' 
#' @section Note: 
#' Earlier versions of this function supported a "bare name + data frame" interface.  This
#' functionality has been removed since it was (a) ambiguous in some cases, (b) unnecessary, 
#' and (c) difficult to maintain.
#' @export
aggregatingFunction1or2 <- 
  function(fun, output.multiple = FALSE, 
           na.rm = getOption("na.rm", FALSE)) {
    template <- 
      function(x, y = NULL, na.rm = NA.RM, ..., data = NULL) { 
        if (rlang::is_formula(x)) {
          if (rlang::is_formula(y)) {
            x <- rlang::eval_tidy(rlang::f_rhs(x), data, environment(x))
            y <- rlang::eval_tidy(rlang::f_rhs(y), data, environment(y))
          } else {
            formula <- mosaicCore::mosaic_formula_q(x, max.slots = 2)
            if (is.null(data)) data <- environment(formula)
            return(maggregate(formula, data = data, FUN = FUNCTION_TBD, 
                              .multiple = OUTPUT.MULTIPLE, ...))
          }
        }
        FUNCTION_TBD(x, y, na.rm = na.rm, ...)
      }
    
    fun_name <- deparse(substitute(fun))
    fun_text <- deparse(template)
    fun_text <- gsub("FUNCTION_TBD", fun_name, fun_text) 
    fun_text <- gsub("OUTPUT.MULTIPLE", deparse(substitute(output.multiple)), fun_text) 
    if (missing(na.rm)) {
      fun_text <- gsub("NA.RM", deparse(substitute(getOption("na.rm", FALSE))), fun_text)
    } else {
      fun_text <- gsub("NA.RM", deparse(substitute(na.rm)), fun_text)
    }
    res <- eval(parse(text = fun_text))
    environment(res) <- parent.frame()
    res   
  }


#' 2-ary aggregating functions
#' 
#' `aggregatinFuntion2` creates statistical summaries of two numerical vectors that 
#' are formula aware.

#' @rdname aggregatingFunction2
#' @aliases aggregatingFunction2 
#' @param fun a function that takes two numeric vectors and computes a summary statistic,
#' returning a numeric vector of length 1.
#' @return a function that generalizes `fun` to handle a formula/data frame interface.
#' @details 
#' This was designed to support functions like `cov()` which can be used to compute
#' numerical summaries from two numeric vectors.
#' The logic of the resulting function is this: 1) If the first two arguments are both formulas,
#' then those formulas are evaluated (with `data`) to compute the covariance; 
#' (2) If the first argument is a formula, and the second is `NULL`, 
#' then the left and ride sides of the formula and `data` are used to create the 
#' vectors passed to `fun`;
#' (3) Else everything is simply passed to `fun` for evaluation.
#' 
#' @section Note: 
#' Earlier versions of this function supported a "bare name + data frame" interface.  This
#' functionality has been removed since it was (a) ambiguous in some cases, (b) unnecessary, 
#' and (c) difficult to maintain.
#' 
#' @examples
#' if(require(mosaicData)) {
#'   foo <- aggregatingFunction2(stats::cor)
#'   foo(length ~ width, data = KidsFeet)
#'   stats::cor(KidsFeet$length, KidsFeet$width)
#' }
#' @export

aggregatingFunction2 <- function(fun) {
  
  template <- 
    function(x, y = NULL, ..., data = NULL) { 
      
      tryCatch(y, error = function(e) {
        if (!is.null(data)) {
          stop("When `data' is specified, `y' should not be used.",
               call. = FALSE)
        }
      })
      
      if (!(is.null(y) || inherits(y, "formula")) && !is.null(data)) {
        stop("When `data' is specified, `y' should be a formula or NULL.", 
             call. = FALSE)
      }
      if (rlang::is_formula(x)) {
        if (rlang::is_formula(y)) {
          x <- rlang::eval_tidy(rlang::f_rhs(x), data, environment(x))
          y <- rlang::eval_tidy(rlang::f_rhs(y), data, environment(y))
        } else {
          formula <- mosaicCore::mosaic_formula_q(x, max.slots = 3)
          x <- rlang::eval_tidy(rlang::f_rhs(formula), data, environment(formula))
          y <- rlang::eval_tidy(rlang::f_lhs(formula), data, environment(formula))
        }
        FUNCTION_TBD(x, y, ...)
      }
      FUNCTION_TBD(x, y, ...)
    }
  
  fun_name <- deparse(substitute(fun))
  fun_text <- deparse(template)
  fun_text <- gsub("FUNCTION_TBD", fun_name, fun_text) 
  
  res <- eval(parse(text = fun_text))
  environment(res) <- parent.frame()
  res
}

# old version -- keep until we establish that new version is working properly.

# aggregatingFunction2 <- function(fun) {
#   result <- function(x, y = NULL, ..., data = parent.frame()) { # , ..fun.. = fun) {
#     orig.call <- match.call()
#     mosaic.call <- orig.call 
#     mosaic.call[[1]] <- fun
#     
#     if ( #"data" %in% names(orig.call) && 
#       ! .is.formula(eval(orig.call$x, parent.frame())))  {  
#       if (!'data' %in% names(formals(fun)) && ! "..." %in% names(formals(fun))) {
#         if("data" %in% names(mosaic.call)) mosaic.call[["data"]] <- NULL  # in case original function didn't have ...
#       }
#       return (eval(mosaic.call , data, enclos = parent.frame()))
#     }
#     
#     # message("Using mosaic super powers!")
#     formula <- eval(orig.call$x, parent.frame())
#     mosaic.call[['data']] <- NULL
#     # if (is.null(mosaic.call[['data']])) mosaic.call[['data']] <- quote(parent.frame())
#     mosaic.call$x <- eval(lhs(formula), envir = data, enclos = parent.frame())
#     mosaic.call$y <- eval(rhs(formula), envir = data, enclos = parent.frame())
#     if (! "..." %in% names(formals(orig.call))) {
#       for (n in setdiff(names(mosaic.call), names(formals(fun)))) {
#         if (n != "") mosaic.call[[n]] <- NULL
#       }
#     }
#     if (! is.null(condition(formula))) {
#       stop("Formula must be of the form y ~ x.")
#     }
#     return(eval(mosaic.call))
#   }
#   assign("fun", fun, environment(result))
#   return(result)
# }


#' Aggregating functions
#' 
#' The `mosaic` package makes several summary statistic functions (like `mean` and `sd`)
#' formula aware.
#' 
#' @rdname aggregating
#' @aliases sum min max mean median sd var cov cor favstats
#' @param x a numeric vector or a formula
#' @param y a numeric vector or a formula
#' @param groups a grouping variable, typically a name of a variable in `data`
#' @param data a data frame in which to evaluate formulas (or bare names).
#' Note that the default is `data = parent.frame()`.  This makes it convenient to
#' use this function interactively by treating the working environment as if it were 
#' a data frame.  But this may not be appropriate for programming uses.  
#' When programming, it is best to use an explicit `data` argument
#' -- ideally supplying a data frame that contains the variables mentioned.
#' @param \dots additional arguments
### @param ..fun.. the underlying function.  It would be very unusual to change 
### this from its default value.
#' @param na.rm a logical indicating whether `NA`s should be removed before computing
#' @details Many of these functions mask core R functions to provide an additional formula 
#' interface.  Old behavior should be unchanged.  But if the first argument is a formula,
#' that formula, together with `data` are used to generate the numeric vector(s) 
#' to be summarized.  Formulas of the shape `x ~ a` or `~ x | a` can be used to
#' produce summaries of `x` for each subset defined by `a`.  Two-way aggregation
#' can be achieved using formulas of the form `x ~ a + b` or ` x ~ a | b`.  See
#' the examples.
#' 
# ' When an object exists both in \code{data} and in the environment of the formula, \code{data}
# ' takes precedence, but disambiguation is possible using \code{.data$x} or \code{.env$x}.  See
# ' the examples.
#' 
#' @section Note:
#' Earlier versions of these functions supported a "bare name + data frame" interface.  This
#' functionality has been removed since it was (a) ambiguous in some cases, (b) unnecessary,
#' and (c) difficult to maintain.
#'
#' @export
mean_ <- aggregatingFunction1(base::mean)

#' @rdname aggregating
#' @export
mean <- function(x, ...) {
  the_call <- match.call()
  the_call[[1]] <- as.name("mean_")
  
  if (inherits(x, c("Matrix", "sparseMatrix", "sparseVector")))
    return(Matrix::mean(x, ...))
  
  return(eval(the_call, parent.frame()))
}

#' @rdname aggregating
#' @export
median <- aggregatingFunction1(stats::median)
#' @rdname aggregating
#' @export

range <- aggregatingFunction1(base::range)
#' @rdname aggregating
#' @export
sd <- aggregatingFunction1(stats::sd)
#' @rdname aggregating
#' @export
max <- aggregatingFunction1(base::max)
#' @rdname aggregating
#' @export
min <- aggregatingFunction1(base::min)
#' @rdname aggregating
#' @export
sum <- aggregatingFunction1(base::sum)
#' @rdname aggregating
#' @export
IQR <- aggregatingFunction1(stats::IQR)
#' @rdname aggregating
#' @export
fivenum <- aggregatingFunction1(stats::fivenum)
#' @rdname aggregating
#' @export
iqr <- aggregatingFunction1(stats::IQR)
#' @rdname aggregating
#' @export
prod <- aggregatingFunction1(base::prod)
#' @rdname aggregating
#' @export
sum <- aggregatingFunction1(base::sum)
#' @rdname aggregating
#' @export
favstats <- aggregatingFunction1(fav_stats, output.multiple = TRUE, na.rm = TRUE)
#' @rdname aggregating
#' @export
quantile <- aggregatingFunction1(stats::quantile, output.multiple = TRUE, 
                                 na.rm = getOption("na.rm", FALSE))
#' @rdname aggregating
#' @export
var <- aggregatingFunction1or2(stats::var)
#' @rdname aggregating
#' @export
cor <- aggregatingFunction2(stats::cor)
#' @rdname aggregating
#' 
#' @examples
#' mean(HELPrct$age)
#' mean( ~ age, data = HELPrct)
#' mean( ~ drugrisk, na.rm = TRUE, data = HELPrct)
#' mean(age ~ shuffle(sex), data = HELPrct)
#' mean(age ~ shuffle(sex), data = HELPrct, .format = "table")
#' # wrap in data.frame() to auto-convert awkward variable names
#' data.frame(mean(age ~ shuffle(sex), data = HELPrct, .format = "table"))
#' mean(age ~ sex + substance, data = HELPrct)
#' mean( ~ age | sex + substance, data = HELPrct)
#' mean( ~ sqrt(age), data = HELPrct)
#' sum( ~ age, data = HELPrct)
#' sd(HELPrct$age)
#' sd( ~ age, data = HELPrct)
#' sd(age ~ sex + substance, data = HELPrct)
#' var(HELPrct$age)
#' var( ~ age, data = HELPrct)
#' var(age ~ sex + substance, data = HELPrct)
#' IQR(width ~ sex, data = KidsFeet)
#' iqr(width ~ sex, data = KidsFeet)
#' favstats(width ~ sex, data = KidsFeet)
#' 
#' cor(length ~ width, data = KidsFeet)
#' cov(length ~ width, data = KidsFeet)
#' tally(is.na(mcs) ~ is.na(pcs), data = HELPmiss)
#' cov(mcs ~ pcs, data = HELPmiss)             # NA because of missing data
#' cov(mcs ~ pcs, data = HELPmiss, use = "complete")  # ignore missing data
#' # alternative approach using filter explicitly
#' cov(mcs ~ pcs, data = HELPmiss |> filter(!is.na(mcs) & !is.na(pcs)))    
#' @export

cov <- aggregatingFunction2(stats::cov)

#' All pairs mean and sum of absolute differences
#' 
#' All pairs mean and sum of absolute differences
#' 
#' @inheritParams MAD

#' @return the mean or sum of the absolute differences between each pair
#' of values in `c(x,...)`.
#' @seealso [mad()]
#' @rdname MAD_
#' @export
MAD_ <- function(x, ..., na.rm = getOption("na.omit", FALSE)) {
  SAD_(x, ..., na.rm = na.rm) / (length(x) + length(list(...)))
}

#' @rdname MAD_
#' @param ... additional arguments appended to `x`
#' 
#' @export
SAD_ <- function(x, ..., na.rm = getOption("na.omit", FALSE)) {
  x <- c(x, unlist(list(...)))
  x <- na.omit(x)
  M <- outer(x, x, "-")
  base::sum(upper.tri(M) * abs(M))
}

#' All pairs mean and sum of absolute differences
#' 
#' The functions compute the sum or mean of all pairwise absolute 
#' differences.  This differs from [stats::mad()], which computes
#' the median absolute difference of each value from the median of 
#' all the values.  See the `ISIwithR` package (and the textbook it
#' accompanies) for examples using these functions in the context of 
#' simulation-based inference.
#' 
#' @param x a numeric vector or a formula.  
#' @param ... additional arguments passed through to `MAD_` 
#'   or `SAD_`.  If `x` is a formula, `...` should
#'   include an argument named `data` if the intent is to 
#'   interpret the formula in a data frame.
#' @param na.rm a logical indicating whether NAs should be removed before
#'   calculating.
#' @param groups a grouping variable, typically a name of a variable in `data`
#' @param data a data frame in which to evaluate formulas (or bare names).
#'   Note that the default is `data = parent.frame()`.  This makes it convenient to
#'   use this function interactively by treating the working environment as if it were 
#'   a data frame.  But this may not be appropriate for programming uses.  
#'   When programming, it is best to use an explicit `data` argument
#'   -- ideally supplying a data frame that contains the variables mentioned.
#' @return the mean or sum of the absolute differences between each pair
#'   of values in `c(x,...)`.
#' @seealso [mad()], [MAD_()]
#' @rdname MAD
#' @export
#' @examples
#' SAD(1:3)
#' MAD(1:3)
#' MAD(~eruptions, data = faithful)

MAD <- aggregatingFunction1(MAD_)

#' @rdname MAD
#' @export
SAD <- aggregatingFunction1(SAD_)

