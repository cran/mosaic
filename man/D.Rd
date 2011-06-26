\name{.oldD}
\alias{.oldD}
\alias{.oldAntiD}
\title{
Old Versions of Derivative and Anti-derivative operators 
}
\description{
Operators for computing numerical derivatives and anti-derivatives as 
functions.  Use the new \code{D} and \code{antiD} instead.
}
\usage{
.oldD(f, n=1, h = 1e-04)
.oldAntiD(f, input0 = 0, val0 = 0, init = c(input0, val0), ...) 
}
\arguments{
  \item{f}{a function that returns a numerical vector 
  of the same length as its first (numerical) argument.
  All values are required to be finite.
}
  \item{h}{ horizontal distance between points used for secant slope
  calculation in \code{.oldD()}.
  }
  \item{n}{order of derivative}
  \item{input0}{initial input to \code{f} unless \code{init} is used instead }
  \item{val0}{initial value of \code{f} unless \code{init} is used instead }
  \item{init}{ a numerical vector  of length 2 providing an initial
  input to and value of the function \code{f}}
  \item{\dots}{ additional arguments to \code{f}
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A function.
}
\author{
Daniel Kaplan (\email{kaplan@macalester.edu}) and 
Randall Pruim (\email{rpruim@calvin.edu})
}

\examples{
fplot(mosaic:::.oldD(sin),xlim=c(-2*pi,2*pi))
fplot(mosaic:::.oldAntiD(sin),xlim=c(-2*pi,2*pi))
fplot( list( sin, mosaic:::.oldD(sin), mosaic:::.oldD(sin,2) ), xlim=c(-2*pi, 2*pi) )
f <- mosaic:::.oldAntiD(cos)
f(1)
sin(1)
}
\keyword{ calculus }
