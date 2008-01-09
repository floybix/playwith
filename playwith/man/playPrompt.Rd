\name{playPrompt}
\alias{playPrompt}
\alias{playFreezeGUI}
\alias{playThawGUI}
\alias{blockRedraws}
\title{Show a message to the playwith user}
\description{
  Part of the \code{\link{playwith}} Application Programming Interface.
}
\usage{
playPrompt(playState, text = NULL)
playFreezeGUI(playState)
playThawGUI(playState)
blockRedraws(expr, playState = playDevCur())
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the plot, window and device. }
  \item{text}{ text to display to the user, or \code{NULL} to hide the prompt. }
  \item{expr}{ an expression to evaluate without redrawing or resizing the plot. }
}
\details{
	The \code{playwith} plot window can show a simple plot to the user. It is shown in the place
	normally occupied by the "call toolbar" (the top-most toolbar in the window).
	When the prompt is shown, all the toolbars and controls in the window are disabled.
	When the prompt is removed, the toolbars and controls are restored.
	When a plot is redrawn (with \code{\link{playReplot}}), the prompt is automatically removed.
	
	The other functions described here are only rarely useful:
	\code{playFreezeGUI} disables all the toolbars and controls, and
	\code{playThawGUI} restores them.
	\code{blockRedraws} is a wrapper around code that prevents the plot from being redrawn
	(typically as a result of resizing the plot area).
}
\value{
	nothing interesting.
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playwith.API}} }
\examples{
if (interactive()) {

playwith(plot(1:10))
playPrompt(playDevCur(), "Click to place a new point.")
points(locator(n=1), col=2)
playPrompt(playDevCur(), NULL)

}
}
\keyword{ programming }

