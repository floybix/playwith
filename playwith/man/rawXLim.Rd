\name{rawXLim}
\alias{rawXLim}
\alias{rawYLim}
\title{Get or set current plot limits}
\description{
  Part of the \code{\link{playwith}} Application Programming Interface.
}
\usage{
rawXLim(playState, space = "plot")
rawYLim(playState, space = "plot")
rawXLim(playState) <- value
rawYLim(playState) <- value
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the plot, window and device. }
  \item{space}{  }
  \item{value}{  }
}
\details{
  
  \code{rawXLim} returns the current plot limits, on a numeric, linear scale. 
  This is as simple as:
  \code{playDo(playState, space=space, list(
	x=convertX(unit(0:1, "npc"), "native", valueOnly=TRUE),
	y=convertY(unit(0:1, "npc"), "native", valueOnly=TRUE)))}
  except that the default \code{space="plot"} will always return a value: if 
  it is a Lattice plot with multiple panels, one will be chosen arbitrarily.
  
  The assignment form converts a numeric range, in the raw native plot coordinates, to values suitable for 
  the plot \code{xlim} argument: it may convert back from log-transformed scales, and convert to factor levels if necessary. 
  It then updates the current plot call with the new value.
}
\value{
	
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playwith.API}} }
\examples{
}
\keyword{ programming }

