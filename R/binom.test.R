binom.test <- function(
	x, n, p = 0.5, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95,...) 
{
	UseMethod('binom.test')
}

binom.test.default <- function(
	x, n, p = 0.5, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95,...) 
{
	stats::binom.test( x=x, n=n , p = p,
	alternative = alternative,
	conf.level = conf.level,...) 
}

binom.test.formula <- function(
	x, n, p = 0.5, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95, success, data.name, ...) 
{
    formula <- x

	data <- n
    dots <- list(...)
#    groups <- eval(substitute(groups), data, environment(formula))
#    subset <- eval(substitute(subset), data, environment(formula))
    form <- lattice::latticeParseFormula(formula, data, #subset = subset, 
        #groups = groups,  
		subscripts = TRUE, drop = TRUE)
    groups <- form$groups
        subscr <- form$subscr
    cond <- form$condition
    x <- form$right
    if (length(cond) == 0) {
        cond <- list(gl(1, length(x)))
    }

	if (missing(data.name)) {
		data.name <- paste( deparse(substitute(n)), "$", form$right.name, sep="" )
	}

	binom.test(x, p=p, alternative=alternative, 
		conf.level=conf.level, success=success, data.name=data.name, ...)
}

binom.test.numeric <- function(
	x,  n, p = 0.5, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95, success, data.name, ...) 
{
	if ( length(x) == 1 ) {
		result <-  stats::binom.test(x=x, n=n, p=p, alternative=alternative,
			conf.level=conf.level) 
		result$data.name <- paste( deparse(substitute(x)), "and", deparse(substitute(n)) )
		return(result)
	}

	if ( length(x) == 2 ) {
		result <-  stats::binom.test(x=x[1], n=sum(x), p=p, alternative=alternative,
			conf.level=conf.level) 
		result$data.name <- deparse(substitute(x))
		return(result)
	}

	if (missing(data.name)) { 
		data.name <- deparse(substitute(x)) 
	}

	binom.test(x=factor(x), p=p, alternative=alternative, 
		conf.level=conf.level, 
		success=success, 
		data.name=data.name, ...)
}

binom.test.character <- function(
	x,  n, p = 0.5, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95, success, data.name, ...) 
{
	if (missing(data.name)) { 
		data.name <- deparse(substitute(x)) 
	}
	binom.test(x=factor(x), p=p, alternative=alternative, 
		conf.level=conf.level, 
		success=success, 
		data.name=data.name, ...)
}

binom.test.logical <- function(
	x,  n, p = 0.5, 
	alternative = c("two.sided", "less", "greater"), 
	conf.level = 0.95, success, data.name, ...) 
{
	if (missing(data.name)) { 
		data.name <- deparse(substitute(x)) 
	}
	binom.test(x=factor(x), p=p, alternative=alternative, 
		conf.level=conf.level, 
		success=success, 
		data.name=data.name, ...)
}


binom.test.factor <- function(
	x,  n, p = 0.5, 
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
	result <- stats::binom.test( x=count, n=n , p = p,
		alternative = alternative,
		conf.level = conf.level, ...) 
	result$data.name <- data.name
	return(result)
}
