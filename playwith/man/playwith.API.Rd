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
\dontrun{

## defining new buttons (and other widgets) for the toolbar...

require(lattice)

## A kind of "OK" button: It gets the subscripts of currently labelled points 
## (from 'identify') and passes them to a function: in this case, just 'print'.
showids_handler <- function(widget, user.data) {
	ids <- plotAndPlayGetState("ids")
	print(unique(unlist(ids)))
}
showids_button <- quote(quickTool("Show IDs", "gtk-yes", 
	tooltip="Print out indices of the selected points", f=showids_handler))
playwith(xyplot(Income ~ Population / Area, data=data.frame(state.x77)), 
	extra.buttons=list(showids_button))

## A toggle button to add a smoothing line to xyplot
## (assumes the 'type' argument will be passed to panel.xyplot)
smooth_handler <- function(widget, user.data) {
	tmp.state <- plotAndPlayGetState()
	# get the current 'type' argument
	plotType <- eval(tmp.state$call$type, tmp.state$env)
	if (is.null(plotType)) plotType <- "p"
	# remove "smooth" type if it is already there
	if (!is.na(i <- match("smooth", plotType))) plotType <- plotType[-i]
	# add "smooth" type if the toggle button is active
	if (widget["active"]) plotType <- c(plotType, "smooth")
	# update state
	tmp.state$call$type <- plotType
	plotAndPlaySetState(tmp.state)
	plotAndPlayUpdate()
}
smooth_button <- quote(quickTool("Smooth", "gtk-add", f=smooth_handler, 
	tooltip="Overlay loess smooth (with default span=2/3)", isToggle=T))
playwith(xyplot(sunspot.year ~ 1700:1988, type="l"), 
	extra.buttons=list(smooth_button))

## Get the current plot limits and print them (works with lattice / traditional)
## It would be simpler to grab the xlim/ylim arguments, but they might be NULL.
showlims_handler <- function(widget, user.data) {
	tmp.state <- plotAndPlayGetState()
	# get current plot limits
	if (tmp.state$is.lattice) {
		# lattice plot
		if (!any(tmp.state$focus)) {
			trellis.focus("panel", 1, 1, highlight=F)
		}
		require(grid)
		xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
		ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
		if (!any(tmp.state$focus)) trellis.unfocus()
	} else {
		# traditional graphics plot
		xlim <- par("usr")[1:2]
		ylim <- par("usr")[3:4]
	}
	xlim <- unlogX(xlim, tmp.state$call, tmp.state$is.lattice)
	ylim <- unlogY(ylim, tmp.state$call, tmp.state$is.lattice)
	print(list(x=signif(xlim, 3), y=signif(ylim, 3)))
}
showlims_button <- quote(quickTool("Limits", "gtk-yes",
	tooltip="Print out current plot limits", f=showlims_handler))
playwith(stripplot(Sepal.Length ~ Species, iris, jitter=T, factor=0.3),
	extra.buttons=c(list("logscale", "zero"), showlims_button))

## A more complex toolbar item:
## A "spinbutton" to choose a number 'n', then group the data into 'n' clusters.
## Should work with plot or xyplot.
my_cluster_handler <- function(widget, user.data) {
	tmp.state <- plotAndPlayGetState()
	n <- widget["value"]
	xy <- xy.coords.call(tmp.state$call, tmp.state$env)
	groups <- NULL
	if (n > 1) {
		clusts <- kmeans(cbind(xy$x,xy$y), n)
		labels <- paste("#", 1:n, " (n = ", clusts$size, ")", sep="")
		groups <- factor(clusts$cluster, labels=labels)
	}
	# update state
	#tmp.state$call$groups <- groups
	# to avoid putting a big vector in the plot call, store in local env:
	assign("auto_groups", groups, envir=tmp.state$env)
	tmp.state$call$groups <- if (!is.null(groups)) quote(auto_groups)
	plotAndPlaySetState(tmp.state)
	plotAndPlayUpdate()
}
my_cluster_spinner <- quote({
	spinner <- gtkSpinButton(min=1, max=10, step=1)
	spinner["value"] <- 1
	gSignalConnect(spinner, "value-changed", my_cluster_handler)
	vbox <- gtkVBox()
	vbox$packStart(gtkLabel("Clusters:"))
	vbox$packStart(spinner)
	foo <- gtkToolItem()
	foo$add(vbox)
	foo
})
# need to generate random data outside the plot call, otherwise it changes!
xdata <- rnorm(100)
ydata <- rnorm(100) * xdata / 2
playwith(xyplot(ydata ~ xdata, aspect="iso", auto.key=list(space="right")), 
	extra.buttons=list(my_cluster_spinner))

}
}

\keyword{ iplot }
\keyword{ programming }
\keyword{ utilities }
