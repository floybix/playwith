\name{playwith.API}
\alias{playwith.API}
\title{The playwith API}
\description{
  The \code{\link{playwith}} Application Programming Interface.
}
\details{
	A \code{playwith} tool is a function ("constructor") that creates
	a graphical-user-interface widget (a \code{\link[RGtk2]{gtkToolItem}}).
	That widget may also have functions attached to it, which are run in
	response to user interaction, or every time the plot is drawn.
	
	Constructors should use the
	convenience function \code{\link{quickTool}} where possible.
	
	Following is a table of the API functions that can be used by tools.
	See the links to specific help pages for details.
	In case these are inadequate, you may work with the \code{\link{playState}} object
	itself.
	
\preformatted{
\link{playDevCur}()
\link{playDevList}()
\link{playDevSet}(playState)
\link{playDevOff}(playState = playDevCur())

\link{playNewPlot}(playState)
\link{playReplot}(playState)

\link{callArg}(playState, arg, name = NULL)
\link{callArg}(playState, arg, name = NULL) <- value

\link{playDo}(playState, expr, space = "plot", clip.off = FALSE)

\link{xyCoords}(playState, space = "plot")
\link{xyData}(playState, space = "plot")

\link{playSelectData}(playState, prompt)
\link{playPointInput}(playState, prompt)
\link{playLineInput}(playState, prompt)
\link{playRectInput}(playState, prompt, scales = c("x", "y"))

\link{rawXLim}(playState, space = "plot")
\link{rawYLim}(playState, space = "plot")
\link{rawXLim}(playState) <- value
\link{rawYLim}(playState) <- value

\link{whichSpace}(playState, x.device, y.device)
\link{deviceCoordsToSpace}(playState, x.device, y.device, 
	space = "plot")

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
	
	## 1. A toggle button to draw "Hello world" text.
	
	## constructor function
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
		## need to re-draw plot to remove label
		if (!widget["active"]) playReplot(playState)
		else hello_postplot_action(widget, playState)
	}
	
	## this is called after the plot is drawn (or re-drawn)
	hello_postplot_action <- function(widget, playState) {
		## do nothing if the toggle button is off
		if (!widget["active"]) return()
		## draw text centered on the page
		grid.text("Hello world", gp=gpar(cex=2))
	}
	
	## add new button to a plot window (the bottom toolbar)
	playwith(plot(1:10), bottom=list(helloTool))
	
	
	## 2. Select subset of data and show marginal histograms.
	##    It stores state info in the local environment.
	
	## constructor function
	subsetTool <- function(playState) {
		playState$subsetTool <- list()
		quickTool(playState,
			label = "Data subset",
			icon = "gtk-justify-fill", 
			tooltip = "Select a subset of data points for stats",
			f = subset_handler,
			post.plot.action = subset_postplot_action)
	}
	
	## this is called when the button is clicked
	subset_handler <- function(widget, playState) {
		foo <- playSelectData(playState)
		if (is.null(foo)) return()
		nSubsets <- length(playState$subsetTool)
		playState$subsetTool[[nSubsets+1]] <- foo
		drawSubsetBox(playState, foo)
	}
	
	## draw one subset box with marginal histograms
	drawSubsetBox <- function(playState, foo) {
		xy <- xyCoords(playState, space=foo$space)
		playDo(playState, with(foo, {
			xc <- mean(coords$x)
			yc <- mean(coords$y)
			wd <- abs(diff(coords$x))
			ht <- abs(diff(coords$y))
			pushViewport(viewport(default.units="native",
				x=xc, y=yc, width=wd, height=ht,
				xscale=range(coords$x), yscale=range(coords$y),
				gp=gpar(alpha=0.3), clip="off"))
			grid.rect(gp=gpar(fill="yellow"))
			## draw sample size text
			grid.text(paste("n =", length(x)), 
				x=unit(0.95, "npc"), y=unit(0.95, "npc"),
				just=c("top", "right"), gp=gpar(cex=1.5))
			## histogram of x values, outside x-axis
			h <- hist(x, plot=FALSE)
			hval <- unit(4 * h$counts / length(x), "cm")
			grid.rect(x=h$breaks[-1], y=unit(0, "npc"), 
				height=hval, width=diff(h$breaks), 
				just=c("right", "top"), default.units="native",
				gp=gpar(fill="purple"))
			## histogram of y values, outside y-axis
			h <- hist(y, plot=FALSE)
			hval <- unit(4 * h$counts / length(x), "cm")
			grid.rect(y=h$breaks[-1], x=unit(0, "npc"), 
				height=diff(h$breaks), width=hval, 
				just=c("right", "top"), default.units="native",
				gp=gpar(fill="purple"))
			popViewport()
		}), space=foo$space)
	}
	
	## this is called after the plot is drawn (or re-drawn)
	subset_postplot_action <- function(widget, playState) {
		for (foo in playState$subsetTool)
			drawSubsetBox(playState, foo)
	}
	
	## add new button to a plot window (the bottom toolbar)
	playwith(xyplot(temperature ~ radiation, environmental),
		bottom=list(subsetTool))
	
	
	## 3. A button to interactively add or remove data points.
	
	## constructor function, with handler in-line
	addTool <- function(playState) {
		quickTool(playState,
			label = "Add points",
			icon = "gtk-add", 
			tooltip = "Add data points by clicking",
			f = function(widget, playState) repeat {
				foo <- playSelectData(playState, prompt=paste(
					"Click to add a point.",
					"Shift-click to delete.",
					"Right-click to stop."))
				if (is.null(foo)) return()
				xy <- xyData(playState)
				if (foo$modifiers & GdkModifierType["shift-mask"]) {
					## shift-click: delete data points
					xy$x[foo$which] <- NA
					xy$y[foo$which] <- NA
				} else {
					## add data point at click location
					xy$x <- c(xy$x, foo$coords$x[1])
					xy$y <- c(xy$y, foo$coords$y[1])
				}
				## store in local environment
				playState$env$localxy <- xy
				if (playState$is.lattice) {
					## lattice plot: use `data` argument
					callArg(playState, 1) <- quote(y ~ x)
					callArg(playState, data) <- quote(localxy)
				} else {
					## otherwise set first argument to plot
					callArg(playState, 1) <- quote(localxy)
					callArg(playState, y) <- NULL
				}
				playReplot(playState)
			})
	}
	ydata <- c(1:4, 2:1, 5:8)
	playwith(xyplot(ydata ~ 1:10, type=c("p", "smooth"), pch=8), 
		left=list(addTool))
	
	
	## 4. A more complex toolbar item: a "spinbutton" to 
	##    group the data into `n` clusters (in plot or xyplot).
	
	## constructor function
	clusterTool <- function(playState) {
		spinner <- gtkSpinButton(min=1, max=10, step=1)
		spinner["value"] <- 1
		gSignalConnect(spinner, "value-changed", cluster_handler,
			data=playState)
		vbox <- gtkVBox()
		vbox$packStart(gtkLabel("Clusters:"))
		vbox$packStart(spinner)
		foo <- gtkToolItem()
		foo$add(vbox)
		foo
	}
	
	## this is called when the spinner value changes
	cluster_handler <- function(widget, playState) {
		n <- widget["value"]
		xy <- xyCoords(playState)
		groups <- NULL
		if (n > 1) {
			clusts <- kmeans(cbind(xy$x,xy$y), n)
			labels <- paste("#", 1:n, " (n = ", clusts$size, ")", sep="")
			groups <- factor(clusts$cluster, labels=labels)
		}
		## avoid a big vector inline in the call, store in local env
		if (playState$is.lattice) {
			playState$env$auto.groups <- groups
			callArg(playState, groups) <- quote(auto.groups)
		} else {
			playState$env$auto.groups <- unclass(groups)
			callArg(playState, col) <- quote(auto.groups)
			if (is.null(groups)) callArg(playState, col) <- NULL
		}
		playReplot(playState)
	}
	
	## need to generate random data outside the plot call!
	xdata <- rnorm(100)
	ydata <- rnorm(100) * xdata / 2
	## works with lattice::xyplot
	playwith(xyplot(ydata ~ xdata, aspect="iso", 
		auto.key=list(space="right")), 
		left=list(clusterTool))
	## same tool works with graphics::plot
	playwith(plot(ydata ~ xdata), left=list(clusterTool))
	
}
}
\keyword{ iplot }
\keyword{ programming }
\keyword{ utilities }
