\name{playwith.API}
\alias{playwith.API}
\title{The playwith API}
\description{
  The \code{\link{playwith}} Application Programming Interface.
}
\details{

\code{\link{playDevCur}}()
playDevList()
playDevSet(playState)
playDevOff(playState = playDevCur())

playNewPlot(playState)
playReplot(playState)

callArg(playState, arg, name = NULL)
callArg(playState, arg, name = NULL) <- value

playDo(playState, stuff, space = "plot", clip.off = FALSE)

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
  
  \code{plotAndPlayGetState} can be used by callback functions to get access to the plot window and plot state. 
  It returns a list (see below for details), or one of its elements (\code{item}), 
  which can be modified and passed back to \code{plotAndPlaySetState}.
  
  \code{plotAndPlaySetRawXLim} converts a numeric range, in the raw native plot coordinates, to values suitable for 
  the plot \code{xlim} argument: it may convert back from log-transformed scales, and convert to factor levels if necessary. 
  It then updates the current plot call with the new value.
  
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
