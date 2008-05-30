\name{playReplot}
\alias{playReplot}
\alias{playNewPlot}
\title{Draw a playwith plot}
\description{
  Part of the \code{\link{playwith}} Application Programming Interface.
}
\usage{
playNewPlot(playState)
playReplot(playState)
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the plot, window and device. }
}
\details{
  \code{playReplot} actually draws the plot on the graphics device and 
  runs the post-plot actions associated with interface tools. It also stores information
  about the plot spaces (associated with viewports -- see \code{\link{playDo}}).
  
  \code{playNewPlot} is the same but also rebuilds all the toolbars, and thus runs all the
  tool constructor functions. 
  
  If only arguments to the plot call have changed, calling \code{playReplot}
  should be enough. If the high-level plot function has changed, or data has changed, 
  you should call \code{playNewPlot}, because some tools will need to initialise 
  themselves depending on the type of plot and data.
  
}
\value{
	the result of the plot call.
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playwith.API}} }
\examples{
if (interactive()) {

playwith(plot(1:10))
playState <- playDevCur()
playState$call <- quote(plot(c(10:1, 1:10)))
playNewPlot(playState)

}
}
\keyword{ programming }

