\name{xyCoords}
\alias{xyCoords}
\alias{xyData}
\alias{getDataArg}
\title{Get playwith plot data points}
\description{
  Part of the \pkg{playwith} Application Programming Interface.
}
\usage{
xyCoords(playState = playDevCur(), space = "plot")
xyData(playState = playDevCur(), space = "plot")
getDataArg(playState = playDevCur(), eval = TRUE)
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the plot, window and device. }
  \item{space}{ character, the plot space for which to get data.
  This is only relevant to multi-panel lattice plots, where data is split across panels.
  In this case, if \code{space="page"}, the combined data from all panels is returned.
  See the \code{space} argument to \code{\link{playDo}}. }
}
\details{
	\code{xyCoords} ...TODO

	\code{xyData} is the same but does not convert the data to numeric.
	Thus the returned data objects may be factors, time series, etc.
}
\value{
	the returned value is a list with elements \code{x} and \code{y}, which are
	numeric vectors in the case of \code{xyCoords}.

	For lattice plots, the output is similar to that returned by
	\code{\link[lattice]{trellis.panelArgs}}, so can include elements such as
	\code{subscripts}.

	\code{x} and \code{y} vectors are recycled to the same length.
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{
  \code{\link{playwith.API}},
  \code{\link{plotCoords}}
}
\examples{
if (interactive()) {

library(lattice)
x <- as.Date("1990-01-01") + 1:20 - 1
ab <- rep(c("a", "b"), each=10)
playwith(xyplot(1:20 ~ x | ab))
playState <- playDevCur()
xyCoords(playState, space="packet 2")
xyData(playState, space="packet 2")
xyData(playState, space="plot")
getDataArg(playState)

}
}
\keyword{ programming }

