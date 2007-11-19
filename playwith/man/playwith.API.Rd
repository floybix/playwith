\name{playwith.API}
\alias{playwith.API}
\title{The playwith API}
\description{
  The \code{\link{playwith}} Application Programming Interface.
}
\details{
	A \code{playwith} tool is defined by a function ("constructor") that creates
	a graphical-user-interface widget (a \code{\link[RGtk2]{gtkToolItem}}).
	That widget may also have functions attached to it, which are run in
	response to user interaction, or every time the plot is drawn.
	
	It is best explained with an example. Here is the constructor function
	for a simple tool to draw the text "Hello world". It makes use of a 
	convenience function \code{quickTool}, which is just a quick way to
	make a \code{\link[RGtk2]{gtkToolButton}}.

	\preformatted{
	helloTool <- function(playState) {
		quickTool(playState,
			label = "Greeting",
			icon = "gtk-yes", 
			tooltip = "Draw 'Hello world' text",
			f = hello_handler,
			post.plot.action = hello_postplot_action,
			isToggle = TRUE)
	}
	
	## this is called when the button is clicked
	hello_handler <- function(widget, playState) {
		## re-draw plot, which will call the post.plot.action
		playReplot(playState)
	}
	
	## this is called after the plot is drawn (or re-drawn)
	hello_postplot_action <- function(widget, playState) {
		if (widget["active"]) {
			annot <- expression(grid.text("Hello world", gp=gpar(cex=2)))
			playDo(playState, annot, space="page")
		}
	}
	}
	
	The new button can then be added to a toolbar like this:
	\preformatted{
	playwith(plot(1:10), bottom=list(helloTool))
	}

	Here is a table of the API functions that can be used by tools.
	See the links to specific help pages for details.
	In case these are inadequate, you may work with the \code{\link{playState}} object
	itself.
	
\preformatted{
\link{playDevCur}()
playDevList()
playDevSet(playState)
playDevOff(playState = playDevCur())

playNewPlot(playState)
playReplot(playState)

callArg(playState, arg, name = NULL)
callArg(playState, arg, name = NULL) <- value

playDo(playState, expr, space = "plot", clip.off = FALSE)

xyCoords(playState, space = "plot")
xyData(playState, space = "plot")

playSelectData(playState, prompt = "Click or drag to select data points.")
playPointInput(playState, prompt = "Click on the plot")
playLineInput(playState, prompt = "Click and drag to define a line")
playRectInput(playState, prompt = "Click and drag to define a rectangular region")

rawXLim(playState, space = "plot")
rawYLim(playState, space = "plot")
rawXLim(playState) <- value
rawYLim(playState) <- value

whichSpace(playState, x.device, y.device)
deviceCoordsToSpace(playState, x.device, y.device, space = "plot")

playPrompt(playState, text = NULL)
playFreezeGUI(playState)
playThawGUI(playState)
blockRedraws(expr, playState = playDevCur())
}
  
  
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playwith}} }
\examples{
	
}
\keyword{ iplot }
\keyword{ programming }
\keyword{ utilities }
