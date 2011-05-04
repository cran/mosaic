\name{mosaicData-package}
\alias{mosaicData-package}
\alias{mosaicData}
\docType{package}
\title{Data sets for Project MOSAIC}
\description{Data sets for Project MOSAIC}


\details{
  \tabular{ll}{
	Package: \tab mosaicData\cr
	Type: \tab Package\cr
	Version: \tab 0.0-0\cr
	Date: \tab 2011-1-12\cr
	License: \tab GPL\cr
	LazyLoad: \tab yes\cr
	LazyData: \tab yes\cr
}

\tabular{ll}{
Overview\tab \code{file.show(system.file("README", package = "mosaicData"))}\cr
News\tab \code{RShowDoc("NEWS", package = "mosaicData")}\cr
Acknowledgements\tab \code{RShowDoc("THANKS", package = "mosaicData")}\cr
This File\tab \code{?mosaicData}\cr
}

}

\author{
Randall Pruim (\url{rpruim@calvin.edu})
}

\keyword{ package }

%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}

\examples{
data(CPS)
str(CPS)
}
