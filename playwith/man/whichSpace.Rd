\name{whichSpace}
\alias{whichSpace}
\alias{deviceCoordsToSpace}
\title{Convert device coordinates to plot space}
\description{
  Part of the \code{\link{playwith}} Application Programming Interface.
}
\usage{
whichSpace(playState, x.device, y.device)

deviceCoordsToSpace(playState, x.device, y.device, space = "plot")
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the plot, window and device. }
  \item{x.device}{ x coordinate in pixels on the graphics device. }
  \item{y.device}{ y coordinate in pixels on the graphics device. }
  \item{space}{ character, the plot space for which to get or set limits.
  See the \code{space} argument to \code{\link{playDo}}. }
}
\value{
	\code{whichSpace} returns the \code{space} value referring to the 
	plot space in which the point \code{(x.device, y.device)} lies.
	See the \code{space} argument to \code{\link{playDo}}.
	If the coordinate is not inside any plot spaces, \code{"page"} is returned.
	
	\code{deviceCoordsToSpace} converts the given device coordinates to native
	coordinates in the specified space. It returns a list with numeric components 
	\code{x} and \code{y}, and a logical \code{inside}, whether the given point 
	lies inside the space boundaries.
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playwith.API}} }
\examples{
if (interactive()) {

library(lattice)
packs <- paste("packet", rep(1:3, each=4))
playwith(xyplot(1:12 ~ 1:12 | packs))
playState <- playDevCur()
dc <- playPointInput(playState)$dc
whichSpace(playState, dc$x, dc$y)
deviceCoordsToSpace(playState, dc$x, dc$y, space="packet 1")
deviceCoordsToSpace(playState, dc$x, dc$y, space="packet 2")

}
}
\keyword{ programming }

