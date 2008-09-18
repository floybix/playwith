\name{playwith.API}
\alias{playwith.API}
\title{The playwith API}
\description{
  The \code{\link{playwith}} Application Programming Interface.
}
\details{
  \code{\link{playwith}} plots (incorporating a plot, window and device)
  are represented by a \code{\link{playState}} object.

	A \code{playwith} tool is a function ("constructor") that creates
	a graphical-user-interface widget (a \code{\link[RGtk2]{gtkToolItem}}).
	That widget may also have functions attached to it, which are run in
	response to user interaction, or every time the plot is drawn.

	Constructors should use the
	convenience function \code{\link{quickTool}} where possible.
	If the constructor function returns \code{NA}, that tool is skipped.
	So one should check whether the tool can work with the current plot,
	and skip it otherwise.

	Simple tools to control parameters can be created with
	the convenience function \code{\link{parameterControlTool}},
	corresponding to the \code{parameters} argument of \code{\link{playwith}}.

	Following is a table of the API functions that can be used by tools.
	See the links to specific help pages for details.
	In case these are inadequate, you may work with the \code{\link{playState}} object
	itself.

\preformatted{
\link{playDevCur}()
\link{playDevList}()
\link{playDevSet}(playState)
\link{playDevOff}(playState)

\link{playGetIDs}(playState, labels = FALSE)

\link{playNewPlot}(playState)
\link{playReplot}(playState)
\link{playUnlink}(playState)

\link{callArg}(playState, arg, expr, data = NULL)
\link{callArg}(playState, arg, expr) <- value

\link{playDo}(playState, expr, space = "plot", clip.off)

\link{xyCoords}(playState, space = "plot")
\link{xyData}(playState, space = "plot")

\link{playSelectData}(playState, prompt, foo)
\link{playPointInput}(playState, prompt)
\link{playLineInput}(playState, prompt)
\link{playRectInput}(playState, prompt)

\link{rawXLim}(playState, space = "plot")
\link{rawYLim}(playState, space = "plot")
\link{rawXLim}(playState) <- value
\link{rawYLim}(playState) <- value
\link{spaceCoordsToDataCoords}(playState, xy)
\link{dataCoordsToSpaceCoords}(playState, xy)

\link{whichSpace}(playState, x.device, y.device)

\link{playPrompt}(playState, text = NULL)
\link{playFreezeGUI}(playState)
\link{playThawGUI}(playState)
\link{blockRedraws}(expr, playState = playDevCur())
}

}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playwith}} }
\examples{
if (interactive()) {

demo(package = "playwith")

}
\keyword{ programming }
\keyword{ utilities }
