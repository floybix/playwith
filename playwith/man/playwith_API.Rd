\name{playwith_API}
\alias{playwith_API}
\alias{playState}
\alias{playDevCur}
\alias{playDevList}
\alias{playDevSet}
\alias{playDevOff}
\alias{playNewPlot}
\alias{playReplot}
\alias{callArg}
\alias{rawXLim}
\alias{rawYLim}
\alias{xRange}
\alias{yRange}
\alias{xClass}
\alias{yClass}
\alias{playFocus}
\alias{playPrompt}
\alias{playFreezeGUI}
\alias{playThawGUI}
\alias{blockRedraws}
\alias{unlogX}
\alias{unlogY}
\alias{gmessage.error}
\title{The playwith API}
\description{
  The \code{\link{playwith}} Application Programming Interface.
}
\usage{
playDevCur()
playDevList()
playDevSet(playState)
playDevOff(playState = playDevCur())

playNewPlot(playState)
playReplot(playState)

callArg(playState, arg, name=NULL)
callArg(playState, arg, name=NULL) <- value

rawXLim(playState)
rawXLim(playState) <- value
rawYLim(playState)
rawYLim(playState) <- value

xRange(playState)
yRange(playState)
xClass(playState)
yClass(playState)

playFocus(playState, highlight=TRUE, ...)

playPrompt(playState) <- value
playFreezeGUI(playState)
playThawGUI(playState)
blockRedraws(expr, playState = playDevCur())

unlogX(x, the.call, is.lattice = T)
unlogY(x, the.call, is.lattice = T)
}
\arguments{
  \item{playState}{  }
  \item{arg}{  }
  \item{name}{  }
  \item{value}{  }
  \item{highlight}{ passed to \code{\link{trellis.focus}}. }
  \item{expr}{  }
  \item{x}{ numeric vector. }
  \item{the.call}{ a plot call. Ignored if \code{is.lattice=F}. }
  \item{is.lattice}{ whether the plot call generates a lattice plot. }
}
\details{
  \code{plotAndPlayButtons} and \code{plotAndPlayBasicButtons} are lists of calls to generate \code{\link[RGtk2]{gtkToolItem}}s.
  These are used as buttons in a \code{\link{plotAndPlay}} plot window.
  
  New buttons can be defined: see the \code{Examples} section of this page.
  
  \code{quickTool} is a convenience function to create a \code{\link[RGtk2]{gtkToolButton}} with the given label, icon, tooltip and click handler.
  For example, one element of \code{plotAndPlayButtons} is
  \code{zoomout=quickTool("Zoom out", "gtk-zoom-out", f=.plotAndPlay_zoomout_event)}.
  
  \code{plotAndPlayGetState} can be used by callback functions to get access to the plot window and plot state. 
  It returns a list (see below for details), or one of its elements (\code{item}), 
  which can be modified and passed back to \code{plotAndPlaySetState}.
  
  \code{plotAndPlayUpdate} re-generates the current plot display (e.g. after updating the call).
  
  \code{plotAndPlayDoFocus} brings one lattice panel into focus. 
  If there is only one panel it is chosen automatically and without highlighting.
  Otherwise the user is prompted to choose one. 
  It returns the new focus (see \code{focus} item below) or NULL if the user cancelled.
  
  The \code{\link{playwith}} window can show a simple prompt to the user.
  This must be initialised with \code{plotAndPlayMakePrompt}, then set with \code{plotAndPlaySetPrompt}, 
  and finally removed with \code{plotAndPlayUnmakePrompt}.
  
  \code{plotAndPlayGetToolbar} returns the \code{gtkToolbar}, 
  and \code{plotAndPlayGetDA} returns the \code{gtkDrawingArea}.
  
  \code{plotAndPlaySetRawXLim} converts a numeric range, in the raw native plot coordinates, to values suitable for 
  the plot \code{xlim} argument: it may convert back from log-transformed scales, convert to factor levels,
  and convert to a date/time format. It then updates the current plot call with the new value.
  Note: this function requires a panel to be in focus, since it uses \code{trellis.panelArgs()}. 
  
  \code{unlogX} just converts back from log-transformed scales if necessary.
}
\value{
  \code{plotAndPlayGetState()} returns a list containing at least:
  \item{win}{a \code{gtkWindow} (the plot window).}
  \item{dev}{the plot device number.}
  \item{call}{the current plot call.}
  \item{env}{local environment used to store evaluated plot call arguments.}
  \item{id.call}{the call to \code{\link{identify}} or similar, used to label data points.}
  \item{label.args}{style arguments for labelling points.}
  \item{nav.scales}{see \code{\link{plotAndPlay}}.}
  \item{trans.scales}{see \code{\link{plotAndPlay}}.}
  \item{ids}{a list, each item contains a vector of subscripts for the points currently labelled in the named lattice packet. For non-lattice plots, there is only one item \code{"all"}.}
  \item{brushed}{a list, each item contains a vector of subscripts for the points currently brushed in the named lattice packet. }
  \item{is.lattice}{logical: is it a lattice plot?}
  \item{focus}{a list with items \code{col} and \code{row} for a lattice panel if one is in focus. Otherwise it is \code{list(col=0, row=0)}.}
  \item{page}{current lattice page number.}
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
