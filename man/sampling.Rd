\name{sample}
\alias{sample}
\alias{resample}
\alias{sample.matrix}
\alias{sample.default}
\alias{sample.factor}
\alias{sample.data.frame}
\alias{deal}
\alias{shuffle}
\alias{rflip}
\alias{nflip}
\alias{print.cointoss}

\title{ Random Samples and Permutations}
\description{
These extensions to the \code{base::sample} make resampling and
randomization tests easier.
}

\usage{
sample(x, size, replace = FALSE, ...)

\method{sample}{matrix}(x, size, replace = FALSE, prob = NULL, 
	groups = NULL, orig.ids = FALSE, ...)

\method{sample}{data.frame}(x, size, replace = FALSE, prob = NULL, groups = NULL,
                 orig.ids = TRUE, fixed = names(x), shuffled = c(),
                 invisibly.return = NULL, ...)

\method{sample}{factor}(x, size, replace = FALSE, prob = NULL, 
	groups = NULL, orig.ids = FALSE, drop.unused.levels=FALSE, ...)

\method{sample}{default}(x, size, replace = FALSE, prob = NULL, groups = NULL, 
    orig.ids = FALSE, ...) 

resample (x, size, replace=TRUE, prob=NULL, groups=NULL, orig.ids=FALSE, ...) 

shuffle(x, replace = FALSE, prob = NULL, 
	groups = NULL, orig.ids = FALSE)
deal(x, size, replace = FALSE, prob = NULL, 
	groups = NULL, orig.ids = FALSE)

rflip(n=1, prob=.5, quiet=FALSE, verbose=!quiet)
nflip(n=1, prob=.5, ...)

\method{print}{cointoss}(x,...)

}

\arguments{
  \item{x}{ an object (typically a data frame, matrix, or vector)
}
  \item{size}{ size of the sample
}
\item{n}{ number of coins to toss}

  \item{replace}{ a logical indicating whether sampling is to be done with
  replacement.
}
  \item{prob}{ a vector of probability weights for obtaining the elements of
  the object being sampled, or the probability of heads for \code{rflip} and 
  \code{nflip}.
}
  \item{groups}{ If present, shuffling is done within groups given by
  this vector.  In this case \code{size}, if present, is ignored.
}
  \item{orig.ids}{ If \code{TRUE} and \code{x} is a data frame or a matrix,
  then the original locations of the data is recorded in the object returned.
}
	\item{quiet}{ If \code{TRUE}, suppress output.}
	\item{verbose}{ If \code{FALSE}, suppress output.}
	\item{drop.unused.levels}{a logical. Remove unused levels from factors?}
	\item{fixed}{vector of columns that are not permuted}
	\item{shuffled}{vector of columns that are permuted}
    \item{invisibly.return}{a logical indicating whether result should be returned invisibly.}
	\item{\dots}{ Additional arguments.}
}
\value{
an object of the same type as \code{x} unless \code{x} is an integer, which
case a vector is returned.
}
\author{
Daniel Kaplan (\email{kaplan@macalester.edu})
and Randall Pruim (\email{rpruim@calvin.edu})
}

\examples{
denominations <- c(2:10,c("J","Q","K","A"))
suits <- c("C","D","H","S")
cards <- as.vector(outer(denominations, suits, paste, sep="") )
shuffle(cards)
set.seed(52)
sample(cards, 13)
set.seed(52)
deal(cards, 13)      # same as sample(cards, 13)
resample(cards, 20)  # same as sample(..., replace=TRUE)
sample(cards, groups=rep(1:4, each=13))
rflip()              # flip 1 coin
rflip(20)            # flip 20 coins
nflip(20)            # flip 20 coins and only show the number of heads
table(do(1000) * nflip(20))   # a table based on flipping 20 coins 1000 times
table(do(1000) * nflip(20,.25))   # a table based on flipping 20 biased coins 1000 times
}
\keyword{ random }
