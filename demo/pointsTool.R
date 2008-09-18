
library(playwith)

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
				callArg(playState, "data") <- quote(localxy)
			} else {
				## otherwise set first argument to plot
				callArg(playState, 1) <- quote(localxy)
				callArg(playState, "y") <- NULL
			}
			playReplot(playState)
		})
}
ydata <- c(1:4, 2:1, 5:8)
playwith(xyplot(ydata ~ 1:10, type=c("p", "smooth"), pch=8),
	left=list(addTool))
