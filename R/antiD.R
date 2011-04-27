antiD <-
function(f, input0=0, val0=0, init = c(input0,val0), ...){
  # find the antiderivative of a function
  # make sure init.x is a reasonable starting point
  x0=input0
  init.x = init[1]
  init.val = init[2]
  v = f(init.x)
  if( abs(v)==Inf | is.nan(v) )
    stop(paste("Function being integrated is Inf or NaN at x0 =", x0))
  # the function to return
  function(x) {
    # either for just one number or a vector
    if( length(x) == 1 ) {
      val0 = integrate(f,init.x, x,...)$value
      return(init.val + val0)
    }
    # it's a vector.
    val0 = integrate(f,init.x,x[1],...)$value
    # a running sum of the integrals between the intervals.
    res = rep(0, length(x))
    for (k in 2:length(res) ){
      res[k] = integrate(f, x[k-1],x[k], ...)$value
       #     res[k] = integrate(f, x[1],x[k], ...)$value
    }
    res = init.val+ val0+cumsum(res)
#        res = init.val+ val0+(res)
    return(res)
  }
}

