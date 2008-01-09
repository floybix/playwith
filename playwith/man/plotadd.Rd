\name{plotadd}
\alias{plotadd}
\title{ Augment a plot in-line }
\description{
  A simple way to encapsulate a plot and annotations in one call. 
}
\usage{
plotadd(FUN, ..., add.stuff = expression())
}
\arguments{
  \item{FUN}{ a high-level plot function. }
  \item{\dots}{ arguments to \code{FUN}. }
  \item{add.stuff}{ expressions to augment the plot, evaluated after plotting. }
}
\details{
	This function is designed to be used by \code{\link{playwith}},
  so that plots can be augmented while keeping the plot arguments in one call.
}
\value{
  the value returned by the high-level plot function.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\examples{
if (interactive()) {
	
	x <- rnorm(64)
	plot(x, x^2, cex=2)
	rug(x)
	grid()
	
	# do it all in one call with plotadd
	plotadd(plot, x, x^2, cex=2, 
		add.stuff=expression(rug(x), grid()))
}
}
\keyword{ hplot }
\keyword{ aplot }

