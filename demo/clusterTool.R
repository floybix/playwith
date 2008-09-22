
library(playwith)

stop("Not working right now")

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
		callArg(playState, "groups") <- quote(auto.groups)
	} else {
		playState$env$auto.groups <- unclass(groups)
		callArg(playState, "col") <- quote(auto.groups)
		if (is.null(groups)) callArg(playState, "col") <- NULL
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
