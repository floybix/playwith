\name{rawXLim}
\alias{rawXLim}
\alias{rawYLim}
\alias{rawXLim<-}
\alias{rawYLim<-}
\alias{spaceCoordsToDataCoords}
\alias{dataCoordsToSpaceCoords}
\title{Get or set current plot limits}
\description{
  Part of the \pkg{playwith} Application Programming Interface.
}
\usage{
rawXLim(playState, space = "plot")
rawYLim(playState, space = "plot")
rawXLim(playState) <- value
rawYLim(playState) <- value

spaceCoordsToDataCoords(playState, xy)
dataCoordsToSpaceCoords(playState, xy)
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the plot, window and device. }
  \item{space}{ character, the plot space for which to get or set limits.
  See the \code{space} argument to \code{\link{playDo}};
  however, in this case, \code{space="plot"} will always return a value: if
  it is a Lattice plot with multiple panels, one will be chosen arbitrarily.}
  \item{value}{ numeric length 2, the new nominal x or y limits
  	(for \code{xlim} or \code{ylim} plot arguments). }
  \item{xy}{ list with at least one of the elements \code{x} and \code{y} (as numeric). }
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

  \code{spaceCoordsToDataCoords} converts from the native viewport coordinates to the
  data coordinates, which simply involves converting from a log scale if necessary.
  \code{dataCoordsToSpaceCoords} is the inverse case: applying a log transformation if necessary.
  It used to refer to the position of data points in the viewport.
}
\value{
	the extractor form returns the x or y plot limits as numeric length 2.
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \link{playwith.API} }
\examples{
if (interactive()) {

playwith(plot(1:10, log="x"))
playState <- playDevCur()
rawXLim(playState)  # -0.04  1.04
rawXLim(playState) <- c(0, 2)
playReplot(playState)
## now xlim=c(1, 100)
(rawx <- rawXLim(playState))  # -0.08  2.08

spaceCoordsToDataCoords(playState, list(x=rawx))
dataCoordsToSpaceCoords(playState, list(x=1:10))

}
}
\keyword{ programming }

