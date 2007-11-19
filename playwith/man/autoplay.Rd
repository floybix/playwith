\name{autoplay}
\alias{autoplay}
\alias{setAutoPlaywith}
\title{ Set playwith to run automatically. }
\description{
	Set \code{playwith} to run automatically with Lattice graphics and/or base graphics.
}
\usage{
autoplay(on = NA, lattice.on = on, base.on = on, grid.on = on, ask = FALSE)
}
\arguments{
	\item{on}{ \code{TRUE} to set \code{\link{playwith}} to run automatically to display
		Lattice and base graphics. \code{FALSE} to revert to the default plot device
		(i.e. \code{getOption("device")}). }
	\item{lattice.on}{ run automatically to display Lattice graphics. }
	\item{base.on}{ run automatically to display base graphics (\code{\link{plot}} etc). }
	\item{grid.on}{ run automatically to display grid graphics (except Lattice). }
	\item{ask}{ if \code{TRUE}, select the appropriate plot call from a list
		(the call stack) when automatically starting a base graphics plot. }
}
\details{
	When \code{lattice.on} is \code{TRUE}, the \code{\link[lattice]{print.trellis}} function, 
	which is typically called implicitly to create lattice plots, will trigger 
	\code{\link{playwith}}, passing the original high-level call. 
	So for lattice plots only, this is like changing your default plot device.
	It only replaces the screen device: plotting to a file device will work as normal.
	This feature requires \code{lattice} package version \code{0.17-1} or later.
	
	When \code{base.on} is \code{TRUE}, any new base graphics plot will trigger 
	\code{\link{playwith}} (via a hook in \code{\link{plot.new}}), and the high-level
	plot call is taken to be the first call (to a named function) on the call stack.
	The usual base graphics paradigm of building up a plot incrementally will not work well,
	because only the initial plot call is recorded, so any further additions will be lost when the plot is redrawn. 
	For similar reasons, multiple-figure plots may not be redrawn correctly.
	The high-level plot will be called twice initially, due to constraints of the mechanism.
	
	Note that this automatic behaviour is not a full replacement for calling
	\code{\link{playwith}} directly, since it does not allow to you specify any of the
	optional arguments.
	
	Another possibility is \code{options(device="playwith")}.
	This will act as a default device
	but without interactivity. This is not well tested.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{playwith}} }
\examples{
\dontrun{

## lattice graphics in the playwith interface:
autoplay(lattice=T)
xyplot(Sepal.Length ~ Sepal.Width | Species, data=iris)

dev.off()

## lattice graphics in your usual screen device:
autoplay(lattice=F)
xyplot(Sepal.Length ~ Sepal.Width | Species, data=iris)

## base graphics in the playwith interface:
autoplay(TRUE)
frog <- rnorm(64)
hist(frog)

dev.off()

## base graphics in your usual screen device:
autoplay(FALSE)
hist(frog)

}
}

\keyword{ device }
\keyword{ iplot }

