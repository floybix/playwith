
library(playwith)

## 2. Select subset of data and show marginal histograms.
##    It stores state info in the local environment.

## constructor function
subsetTool <- function(playState) {
	## set up a list to store state
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
		grid.text(paste("n=", length(x), sep=""),
			x=unit(0.98, "npc"), y=unit(0.98, "npc"),
			just=c("right", "top"), gp=gpar(cex=1.5))
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
