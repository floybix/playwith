
library(playwith)

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
	if (!widget["active"]) {
		playReplot(playState)
		return()
	}
	hello_postplot_action(widget, playState)
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
