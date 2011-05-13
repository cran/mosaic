prop.test <- function(
	x, n, p=NULL, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95,...) 
{
	UseMethod('prop.test')
}

prop.test.default <- function(
	x, n, p=NULL, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95,...) 
{
	stats::prop.test( x=x, n=n , p = p,
	alternative = alternative,
	conf.level = conf.level,...) 
}

prop.test.formula <- function(
	x, n, p=NULL, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95, success, data.name, data, ...) 
{
    formula <- x
	missing.n <- missing(n)
	missing.data <- missing(data)
    dots <- list(...)
#    groups <- eval(substitute(groups), data, environment(formula))
#    subset <- eval(substitute(subset), data, environment(formula))
	if (missing.n && !missing.data) {
    	form <- lattice::latticeParseFormula(formula, data, #subset = subset, #groups = groups,  
					subscripts = TRUE, drop = TRUE)
		if (missing(data.name)) {
			data.name <- paste( deparse(substitute(data)), "$", form$right.name, sep="" )
		}
	} else {
    	form <- lattice::latticeParseFormula(formula, n, #subset = subset, #groups = groups,  
					subscripts = TRUE, drop = TRUE)
		if (missing(data.name)) {
			data.name <- paste( deparse(substitute(n)), "$", form$right.name, sep="" )
		}
		data <- n
	}
	# now data.name should be set and data should hold the data

#    groups <- eval(substitute(groups), data, environment(formula))
#    subset <- eval(substitute(subset), data, environment(formula))
    groups <- form$groups
    subscr <- form$subscr
    cond <- form$condition
    x <- form$right
    if (length(cond) == 0) {
        cond <- list(gl(1, length(x)))
    }

	prop.test(x, p=p, alternative=alternative, 
		conf.level=conf.level, success=success, data.name=data.name, ...)
}

prop.test.numeric <- function(
	x,  n, p=NULL, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95, success, data.name, ...) 
{
	if ( length(x) == 1 ) {
		result <-  stats::prop.test(x=x, n=n, p=p, alternative=alternative,
			conf.level=conf.level) 
		result$data.name <- paste( deparse(substitute(x)), "and", deparse(substitute(n)) )
		return(result)
	}

	if ( length(x) == 2 ) {
		result <-  stats::prop.test(x=x[1], n=sum(x), p=p, alternative=alternative,
			conf.level=conf.level) 
		result$data.name <- deparse(substitute(x))
		return(result)
	}

	if (missing(data.name)) { 
		data.name <- deparse(substitute(x)) 
	}

	prop.test(x=factor(x), p=p, alternative=alternative, 
		conf.level=conf.level, 
		success=success, 
		data.name=data.name, ...)
}

prop.test.character <- function(
	x,  n, p=NULL, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95, success, data.name, ...) 
{
	if (missing(data.name)) { 
		data.name <- deparse(substitute(x)) 
	}
	prop.test(x=factor(x), p=p, alternative=alternative, 
		conf.level=conf.level, 
		success=success, 
		data.name=data.name, ...)
}

prop.test.logical <- function(
	x,  n, p=NULL, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95, success, data.name, ...) 
{
	if (missing(data.name)) { 
		data.name <- deparse(substitute(x)) 
	}
	prop.test(x=factor(x, levels=c('TRUE','FALSE')), p=p, alternative=alternative, 
		conf.level=conf.level, 
		success=success, 
		data.name=data.name, ...)
}


prop.test.factor <- function(
	x,  n, p=NULL, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95, success, data.name, ...) 
{
	if (missing(data.name)) { 
		data.name <- deparse(substitute(x)) 
	}
	if (missing(success)) {
		success <- levels(x)[1]
	}
	x <- x [!is.na(x)]
	count <- sum(x==success)
	n <- length(x)
	result <- stats::prop.test( x=count, n=n , p = p,
		alternative = alternative,
		conf.level = conf.level, ...) 
	result$data.name <- data.name
	return(result)
}
