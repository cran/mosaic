# ============== Local Data Storage ========
.fetchData.storage.helper = function( ){
   local.library = list()
   search.path = c("http://www.mosaic-web.org/datasets/",
   "http://www.macalester.edu/~kaplan/ISM/datasets/",
   "http://dl.dropbox.com/u/5098197/Math155/Data/")
   
   fun = function(library=FALSE,searchpath=FALSE,val=NULL,name=NULL,action) {
     if( library ) {
       if( action=="add"){ local.library[[name]] = val; return(c())}
       if( action=="get"){ return( local.library[[name]] ) }
       if( action=="names"){ return( names(local.library) ) }
     }
     if( searchpath ){
       if( action=="add") {search.path = c(val, search.path); return(search.path)}
       if( action=="delete") {
         if( is.null(val) ) search.path=c()
         else search.path = search.path[ val != search.path ]
         return(search.path)
       }
       if( action=="get") return(search.path)
     }
     stop("Can use only for the fetchData library and search path.")
   }
   return(fun)
}
# ===============
.fetchData.storage =.fetchData.storage.helper() # run the function to create .fetchData.storage

# ============
fetchData = function(name=NULL,
  add.to.path=FALSE, drop.from.path=FALSE,
  add.to.library=FALSE, directory=NULL, var=NULL){
  # ==== load a data set to the local library
  if (add.to.library) {
      if( !is.null(var)) {
        .fetchData.storage(name=name,val=var,library=TRUE,action="add")
      }
      else {
        goo = fetchData(name)  # get it from the web site
        if (exists("goo") ) .fetchData.storage(name=name,val=goo,library=TRUE,action="add") 
        else warning("Can't find file ", name)
      }
      return(NULL)
  }
  # ==== Interface to the search path
  if (add.to.path) {
    if (!is.null(name) )
      .fetchData.storage(searchpath=TRUE,name=name,action="add")
    return(.fetchData.storage(searchpath=TRUE,action="get"))
  }
  if (drop.from.path) { # leaving out name means that path will be emptied.
      return(.fetchData.storage(searchpath=TRUE,name=name,action="delete"))
  }
  # ==== Look for file in a local directory.
  if( is.null(name) )
    return( read.csv( file.choose() ))
  # ==== Look on web, in library, in packages.         
  if (name %in% .fetchData.storage(library=TRUE,action="names") )
    return( .fetchData.storage(library=TRUE,action="get",name=name) )
  else {
    res = NULL
    # look for it in the web sites
    web.sites = c(directory,.fetchData.storage(searchpath=TRUE,action="get"))
    for (k in web.sites) {
      res = try( suppressWarnings(read.csv( paste(k,name,sep="") )), silent=TRUE)
      if( !(is.null(res) | class(res)=="try-error") ) return(res)
    }
    # If not found on the web search path or in the local library, try packages.
    # This is just for convenience.  Strip .csv from the end of the string if it's there
    foo = gsub(".csv$|.CSV$","",name)
    suppressWarnings( data(list=c(foo)) )
    if (exists(foo)) return(get(foo)) 
    if (is.null(res) | class(res) == "try-error" )
      stop("Can't locate file ",name )
  }
}
