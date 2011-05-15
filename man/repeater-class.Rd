\name{repeater-class}
\Rdversion{1.1}
\docType{class}
\alias{repeater-class}
\alias{*,repeater,ANY-method}
\alias{print,repeater-method}

\title{Class \code{"repeater"}}
\description{
Repeater objects store information about a desired number
of replications of other code.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("repeater", ...)},
but \code{do(...)} is the recommended call for creating these objects
as it makes the resulting syntax much nicer.
}
\section{Slots}{
  \describe{
    \item{\code{n}:}{number of replications }
    \item{\code{cull}:}{function for culling output of each replication }
    \item{\code{mode}:}{desired mode of object returned }
  }
}
\section{Methods}{
  \describe{
    \item{*}{\code{signature(e1 = "repeater", e2 = "ANY")}: ... }
    \item{print}{\code{signature(x = "repeater")}: ... }
	 }
}
\author{
Randall Pruim (\email{rpruim@calvin.edu}), 
Daniel Kaplan (\email{kaplan@macalester.edu})
}

\seealso{
\code{\link{do}}
}
\examples{
showClass("repeater")
}
\keyword{classes}
