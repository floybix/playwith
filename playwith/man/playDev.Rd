\name{playDev}
\alias{playDev}
\alias{playDevCur}
\alias{playDevList}
\alias{playDevSet}
\alias{playDevOff}
\title{Work with the playwith device}
\description{
  Part of the \code{\link{playwith}} Application Programming Interface.
}
\usage{
playDevCur()
playDevList()
playDevSet(playState)
playDevOff(playState = playDevCur())
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the plot, window and device. }
}
\details{
	These are analogous to \code{\link{dev.cur}}, \code{\link{dev.list}},
	\code{\link{dev.set}} and \code{\link{dev.off}}, and do in fact call these
	lower-level functions. Specifically, it is recommended to call
	\code{playDevSet(playState)} before any direct drawing operations, to
	ensure that this is the current graphics device.
}
\value{
	\code{playDevCur} returns the currently active \code{\link{playState}} object.
	
	\code{playDevList} returns a list of \code{\link{playState}} objects for all open \code{playwith} plot windows.
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playwith.API}} }
\examples{
if (interactive()) {

playwith(plot(1:10))
play.first <- playDevCur()
playwith(plot(20:1), new=TRUE)

## show the first window again
playDevSet(play.first)
playDevCur()

## replace it
playwith(plot(1:100))

playDevList()

## clean up
playDevOff()
invisible(lapply(playDevList(), playDevOff))

}
}
\keyword{ programming }

