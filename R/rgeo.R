deg2rad <- function(x) {
	x/180 * base::pi
}

rad2deg <- function(x) {
	x / base::pi * 180
}

xyz2latlon <- function(x,y,z) {

	# rescale to unit sphere
	R <- sqrt( x^2 + y^2 + z^2)
	x <- x/R; y <- y/R; z <- z/R;

	r <- sqrt( x^2 + y^2)
	lat <- rad2deg( asin(z) )


	long <- (r>0) * rad2deg( acos(x/r) ) 
	long <- ( 1 - 2 * (y < 0) ) * long
	long [ is.na(long) ] <- 0
	return( cbind(lat=lat, lon=long) )
}

latlon2xyz <- function(latitude,longitude) {
	z <- sin(deg2rad(latitude))
	r <- sqrt(1 - z^2)
	x <- rad2deg(cos( longitude ))
	y <- rad2deg(sin( longitude ))
	return(cbind( x=x, y=y, z=z ))
}

rgeo <- function( n=1, latlim=c(-90,90), lonlim=c(-180,180), verbose=FALSE ) {

	zlim <- sin(sort(deg2rad(latlim)))
	z <- runif( n,  zlim[1], zlim[2] )

	#beta <- runif( n, min(deg2rad(lonlim)), max(deg2rad(lonlim)) )
	blim <- deg2rad(sort(lonlim))
	beta <- runif( n, blim[1], blim[2] )

	r <- sqrt(1-z^2)
	x <- r * cos(beta)
	y <- r * sin(beta)

	# now convert this to latitude and longitude 
	latlon <- xyz2latlon(x,y,z)
	if (verbose) {
		return(data.frame(lat=latlon[,1], lon=latlon[,2], x=x, y=y, z=z))
	}
	return(data.frame(lat=latlon[,1], lon=latlon[,2]))
}

rgeo2 <- function( n=1, latlim=c(-90,90), lonlim=c(-180,180), verbose=FALSE ) {

	# oversample pts in a cube

	m <- 10 + 3*n
	x <- runif(m,-1,1)
	y <- runif(m,-1,1)
	z <- runif(m,-1,1)

	# select pts inside unit sphere and project them onto surface

	r <- sqrt( x^2 + y^2 + z^2 )
	ids <- which( r < 1 ) [1:n]
	x <- x[ids]
	y <- y[ids]
	z <- z[ids]
	r <- r[ids]
	x <- x/r
	y <- y/r
	z <- z/r

	# now convert this to latitude and longitude 
	latlon <- xyz2latlon(x,y,z)
	if (verbose) {
		return(data.frame(lat=latlon[,1], lon=latlon[,2], x=x, y=y, z=z))
	}
	return(data.frame(lat=latlon[,1], lon=latlon[,2]))
}

googleMap <- function(latitude, longitude, position=NULL,
	zoom=12, 
	maptype=c('roadmap','satellite','terrain','hybrid'),
	mark=FALSE,
	radius=0,
	browse=TRUE,
	...
	)
{
	urls <- .googleMapURL( 
				latitude=latitude, longitude=longitude,
				position=position, zoom=zoom, 
				maptype = maptype, mark=mark, radius=radius
			)

	if (browse) {
		invisible( sapply( urls, function(x) { browseURL(x,...) } ) )
	} else {
		invisible(urls)
	}
}

	
.googleMapURL <- function(latitude, longitude, position=NULL,
	zoom=11, 
	maptype=c('roadmap','satellite','terrain','hybrid'),
	mark=FALSE,
	radius=0
	) 
{
	filename <- "googlemap3.html"

	if (FALSE) { # can't get browseURL to accept a file with parameters added on
		package <- "mosaic"
		paths <- .find.package(package, verbose = TRUE)
		paths <- paths[file_test("-d", file.path(paths, "google"))]
		paths <- file.path(paths, "google")
		paths <- paths[file_test("-f", file.path(paths, filename))]
		url <- file.path(paths, filename)
		url <- paste("file://",url,sep="")
	}
	url <- paste('http://mosaic-web.org/',filename,sep="")
	if (is.null(position)) {
		position <- data.frame(lat=latitude,lon=longitude)
	}
	latitude  <- position[,1]
	longitude <- position[,2]
	maptype <- match.arg(maptype)
	center <- paste(latitude,",",longitude,sep="")
	markString <- ""
	if (mark == TRUE) { markString <- paste('&mlat=',round(latitude,6),'&mlon=',round(longitude,6) ,sep="") } 

	invisible(paste(
		url,
		'?lat=', round(latitude,6),
		'&lon=', round(longitude,6),
		markString,
		'&zoom=', zoom,
		'&radius=', paste(as.character(radius),collapse=","),
		sep=""))
}

.googleMapURL2 <- function(latitude, longitude, position=NULL,
	zoom=12, 
	width=600, height=400, 
	maptype=c('roadmap','satellite','terrain','hybrid'),
	mark=FALSE
	) 
{
	latitude  <- position[,1]
	longitude <- position[,2]
	url <- "http://maps.google.com/maps/api/staticmap?"
	maptype <- match.arg(maptype)
	center <- paste(latitude,",",longitude,sep="")
	size <- paste(width,'x',height,sep="")
	markString <- ""
	if (mark == TRUE) { markString <- paste('&markers=size:tiny|', center,sep="") } 

	invisible(paste(
		url,
		'center=', center,
		markString,
		'&zoom=', zoom,
		'&size=', size,
		'&sensor=false', 
		'&maptype=', maptype,
		sep=""))
}
