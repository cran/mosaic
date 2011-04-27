\name{cards}
\alias{cards}
\docType{data}
\title{
Standard Deck of Cards
}
\description{
A character vector with two-character representations of 
each card in a standard 52-card deck.
}
\usage{data(cards)}
\examples{
deal(cards, 13)        # bridge hand
deal(cards, 5)         # poker hand
shuffle(cards)         # shuffled deck
}
\keyword{datasets}
