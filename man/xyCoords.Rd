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
  \item{eval}{ whether to evaluate the argument; otherwise return the
    it as it appears in the call. }
}
\details{
None yet.
}
\value{
  the returned value is a list with elements \code{x} and \code{y}, which are
  numeric vectors in the case of \code{xyCoords}.
  Can also include an element \code{subscripts}, and generally will for
  lattice plots (similar to the result of calling
  \code{\link[lattice]{trellis.panelArgs}}).
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{
  \link{playwith.API},
  \code{\link{plotCoords}}
}
\examples{
if (interactive()) {

library(lattice)
x <- as.Date("1990-01-01") + 1:20 - 1
ab <- rep(c("a", "b"), each=10)
playwith(xyplot(1:20 ~ x | ab, subscripts = TRUE))
playState <- playDevCur()
xyCoords(playState, space="packet 2")
xyData(playState, space="packet 2")
try(xyData(playState, space="plot"))
getDataArg(playState)

}
}
\keyword{ programming }

