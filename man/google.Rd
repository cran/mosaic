\name{googleMap}
\alias{googleMap}
\title{
Access to Google Maps
}
\description{
Front end allowing access to google maps.
}
\usage{
googleMap(latitude, longitude, position = NULL, zoom = 12, 
maptype = c("roadmap", "satellite", "terrain", "hybrid"), 
mark = FALSE, radius = 0, browse = TRUE, ...)
}
\arguments{
  \item{latitude}{ a numeric value between -90 and 90 giving the latitude of the center of the map.
}
  \item{longitude}{a numeric value between -180 and 180 giving the longitude of the center of the map.
}
  \item{position}{a data frame or matrix with at least 2 columns.  The first two columns are interpreted
  as  \code{latitude} and \code{longitude} values.
}
  \item{zoom}{an integer value between 1 and 21 indicating the amount zoom.
}
  \item{maptype}{ type of map (not yet implemented).
}
  \item{mark}{a boolean indicating whether the center of the map should be marked with a flag.
}
  \item{radius}{ a numeric vector of radii (in miles) to indicate with concentric circles around the map 
  location.
}
  \item{browse}{a logical.  If \code{TRUE} open URL in browser, else return the URL.
}
  \item{\dots}{ additional arguments
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Primarily used for the side effect of opening a map in a browser, the URLs used are returned (invisibly).
}
\author{
Randall Pruim (\email{rpruim@calvin.edu}
}


\seealso{
\code{\link{rgeo}}
}
\examples{
googleMap( 42.931708, -85.58886, zoom=13, radius=c(0.01,0.1,1,3,12))
googleMap( pos=rgeo(), radius=1:3 )
}
\keyword{ graphics }
