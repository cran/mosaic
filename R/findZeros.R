findZeros = function(expr, ..., xlim=NULL, npts=1000) {
    vals = list(...)
    ..f.. = .createMathFun( sexpr=substitute(expr), ...)
    vars = formals(..f..$fun)
    # If there is a plot showing, and no independent 
    # variable was specified, get the axis names from the plot
    if( is.null(xlim) ){
      ..currentAxisNames = mosaic.par.get("currentAxisNames")
      if( is.null(..currentAxisNames) ) ..currentAxisNames=c("", "")
      ..currentAxisLimitX= mosaic.par.get("currentAxisLimitX")
      if( is.null(..currentAxisLimitX) ) ..currentAxisLimitX=c(0, 1)
      
      if (is.null(..f..$names) | length(..f..$vals[[..f..$names[1]]]) == 0 ) {
        if( ..currentAxisNames[1] == "" )
          stop("No plotting variable defined")
        else ..f..$names = ..currentAxisNames[1]
        if (length(..f..$vals)==0 ) xlim = ..currentAxisLimitX
        else xlim=c(-Inf,Inf)
      }
    }
    ndims = length(..f..$names)
    if( ndims != 1 ) stop("Only works for one unknown.")
    pfun = function(.x){
      vals[[..f..$names]] = .x
      eval( ..f..$sexpr, envir=vals, enclos=parent.frame())
    }
    xlim2 = xlim
    if( is.null(xlim) & ..f..$names %in% names(vals) ) xlim2 = vals[[..f..$names]]
    
    mx = max(xlim2)
    mn = min(xlim2)
    if( length(xlim2) < 2 | mx==mn ) 
       stop("Must provide a finite range to search over.")
    # Deal with very large numbers for the interval, e.g. Inf
    verybig = .Machine$double.xmax
    plainbig = 10000 # linear spacing below this.
    mx = max(min(verybig,mx),-verybig)
    mn = min(max(-verybig,mn),verybig)
    rightseq = NULL
    leftseq = NULL
    middleseq = NULL
    if( mx > plainbig ) { 
      rightseq = exp( seq( log(max(plainbig,mn)),log(mx),length=npts) )
    }
    middleseq = seq( max(-plainbig,mn), min(plainbig,mx), length=npts)
    if( mn < -plainbig ){
      leftseq = -exp( seq( log(-mn), log(-min(-plainbig,mx)), length=npts))
    }
    searchx = unique(c(leftseq,middleseq, rightseq))
    # searchx = seq(min(xlim2), max(xlim2), length=npts)
    y = pfun(searchx)
    ys = sign(y)
    testinds = which(abs(diff(ys)) != 0)
    if (length(testinds) == 0 ) return(NULL)
    zeros = rep(NA, length(testinds) )
    for (k in 1:length(testinds) ) {
      where = testinds[k]
      zeros[k] = uniroot(function(qqzz){pfun(qqzz)}, 
        lower=searchx[where], upper=searchx[where+1])$root
    }
    return(zeros)
}
