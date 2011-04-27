##########################################
# Random number generation in modeling: for use in model formulas
rand = function(df=1, rdist=rnorm, args=list(), nr, seed=NULL ){
	if(missing(nr)) {
		nr <- length(get( ls( envir=parent.frame())[1], envir=parent.frame()))
	}
	if (!is.null(seed)){
		set.seed(seed)
	}

	result <-  matrix( do.call( rdist, args=c(list(n=df*nr), args) ), nr=nr ) 
#	colnames(result) <- paste('rand',1:df,sep="")
	return(result)
}
