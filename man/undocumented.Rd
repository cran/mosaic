\name{undocumented}
\alias{dotPlot}
\alias{panel.dotPlot}
\alias{surround}
\alias{adapt_seq}
\alias{SAT1}
\alias{latlon2xyz}
\alias{xyz2latlon}
\alias{rad2deg}
\alias{deg2rad}
\alias{statTally}
\alias{mosaic.getOption}
\alias{mosaic.options} 
\alias{mosaic.par.get} 
\alias{mosaic.par.set}
\alias{updateList}

\title{Undocumented Objects}

\description{Documentation is still pending for these objects}

\author{Randall Pruim}

\examples{
# Is the distribution of golf ball numbers uniform? 
golfballs <- c(137, 138, 107, 104)                 # sample data;  n = 486
rgolfballs <- do(1000) * table(rdata(486, 1:4))    # random data assuming uniform dist
# Now compare based on various test statistics
print(statTally(golfballs, rgolfballs, max))   
print(statTally(golfballs, rgolfballs, min))
print(statTally(golfballs, rgolfballs, function(x) { diff( range(x)) } ))
print(statTally(golfballs, rgolfballs, sd))
}
