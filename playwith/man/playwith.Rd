\name{playwith}
\alias{playwith}
\alias{plotAndPlay}
\title{An interactive plot GUI}
\description{
  Open a graphical user interface for viewing and interacting with a plot.
  It tries to work out the structure of the plot, in order to interact with it.
  The built-in features include: navigating the data space, identifying data points,
  editing and annotating the plot, and saving to a file. New tools can be defined.
}
\usage{
playwith(expr, 
	new = FALSE,
	title = NULL, 
	time.mode = FALSE,
	labels = NULL, 
	data.points = NULL, 
	is.lattice = NA, 
	viewport = NULL, 
	top.tools = playApplicationTools, 
	left.tools = playInteractionTools, 
	bottom.tools = list(),
	right.tools = list(), 
	...,
	show.call = TRUE,
	win.size = c(640, 500), 
	modal = FALSE,
	on.close = NULL, 
	eval.args = NA, 
	invert.match = F, 
	envir = parent.frame(), 
	playState = if (!new) playDevCur(),
	plot.call)
}
\arguments{
  \item{expr}{ an expression to create a plot, like \code{plot(mydata)}. 
  	Note, this can have lots of arguments, just like a normal plot call (see examples).
  	Can also be a chunk of code in \code{\{}braces\code{\}}, but that form does not allow interaction. }
  \item{new}{ if \code{TRUE} open in a new window, otherwise replace the current window (if one exists). }
  \item{title}{ optional window title. If a plot window exists with the same title, 
  	the new plot will appear in that window, replacing the old plot. }
  \item{time.mode}{ whether the plot is to start in "time mode", with navigation along the x-axis. }
  \item{labels}{ a character vector of labels for data points, for use in \code{\link{identify}}. 
  	If missing, it will be guessed from the plot call arguments. }
  \item{data.points}{ a list with elements "x" and "y" (or other suitable plotting structure)
	giving locations of data points, in case these can not be guessed from the plot call arguments. }
  \item{is.lattice}{ whether the plot is a \link[lattice]{Lattice} plot, i.e. returns a trellis object.
  	If \code{NA}, the call name is matched against known Lattice function names. }
  \item{viewport}{ name or \code{\link[grid]{vpPath}} of the \code{\link[grid]{viewport}}
	representing the data space. This allows interaction with grid graphics plots 
	(but ignore this for Lattice plots). }
  \item{top.tools, left.tools, bottom.tools, right.tools}{
	each a list of tools for toolbars at the top, left, bottom or right sides of the window.
	Each list element should be the name of one of the pre-defined tools,
	or a function to construct a \code{\link[RGtk2]{gtkToolItem}}. 
  	See \link{playTools} for a description of the pre-defined tools,
	and \link{playwith.API} for help on defining new tools. 
  }
  \item{\dots}{ extra arguments are stored in the \code{\link{playState}} object.
  	These can then be accessed by user-defined tools. The default tools will recognise the following
	extra arguments:
	\describe{
		\item{\code{page}}{
			In multi-page Lattice plots, this will set the initial page to display.
		}
		\item{\code{time.index}}{
			a vector defining discrete times, as numeric, 
			\code{\link{Date}} or \code{\link[DateTimeClasses]{POSIXt}}. 
			It must be sorted, increasing.
			If given, then the "time mode" is used to navigate
			along these discrete times, rather than along the continuous x-axis.
			Special objects \code{cur.index} and \code{cur.time} will be provided in the
			plot environment, so the plot call can refer to these.
			\code{cur.index} is the current time step, between \code{1} and \code{length(time.index)}, 
			and \code{cur.time} is \code{time.index[cur.index]}.
			In this case \code{time.mode} will be on by default.
		}
		\item{\code{current.index}, \code{current.time}}{
			If \code{time.index} is given, this will set the initial time step.
		}
		\item{\code{label.style}}{
			output from \code{\link{gpar}}, specifying the style of labels.
			e.g. \code{gpar(cex=1.5, fontface="italic", fontfamily="serif")}.
		}
		\item{\code{arrow.style}}{
			output from \code{\link{gpar}}, specifying the style of arrows. 
			e.g. \code{gpar(lwd=2, col="red", alpha=0.5)}.
		}
		\item{\code{arrow.arrow}}{
			output from \code{\link{arrow}}, specifying the type of arrows to draw.
			e.g. \code{arrow(ends="both", type="closed")}.
		}
		\item{\code{annotation.mode}, \code{clip.annotations}, 
			\code{keep}, \code{stay.on.top}}{
			set the corresponding window options.
			All are logical except \code{annotation.mode} which can be 
			either \code{"page"} or \code{"plot"} (the default).
		}
	}
  }
  \item{show.call}{ set to \code{FALSE} to hide the plot call text-box ("address bar") and associated buttons.}
  \item{win.size}{ default initial size of the plot window (width, height) in pixels. }
  \item{modal}{ whether the window is modal: if \code{TRUE}, the session will freeze until the window is closed. }
  \item{on.close}{ a function to be called when the user closes the plot window. The \code{\link{playState}} object
	  will passed to the function. If the function returns \code{TRUE}, the window will not be closed. }
  \item{eval.args}{ whether to evaluate the plot call arguments: can be \code{TRUE}, \code{FALSE}, \code{NA} (don't eval global vars) 
	or a \link[=regex]{regular expression} matching symbols to evaluate. See below. }
  \item{invert.match}{ whether to evaluate arguments that do NOT match the regular expression given in \code{eval.args}. }
  \item{envir}{ \code{\link{environment}} to use in evaluating the call arguments (see \code{eval.args}) }
  \item{playState}{ the \code{\link{playState}} object for an existing plot window.
	If given, the new plot will appear in that window, replacing the old plot.
	This over-rides the \code{new} argument.}
  \item{plot.call}{ a plot call (\code{\link{call}} object), if given this is used instead of \code{expr}. }
}
\details{
	This function opens a GTK+ window containing a plot device 
	(from the \code{cairoDevice} package) and several toolbars. 
	There is a call toolbar (similar to the "address bar" of a web browser) at the top,
	showing the current plot call, which can also be edited in-place. 
	Then there are up to four toolbars, one on each side of the plot.
	The tools to be placed in these four toolbars can be specified 
	if necessary.
	
	With the \code{\link{autoplay}} facility, \code{playwith} can function 
	as a default graphics device (although it is not technically a 
	graphics device itself, it is a wrapper around one).
	
	The pre-defined tools can be listed with \code{names(toolConstructors)}.
	See \link{playTools} for a description of the pre-defined tools,
	and \link{playwith.API} for help on defining new tools.
	Note that different sets of tools will be used by default for calls to 
	\code{\link[lattice]{splom}}, \code{\link[lattice]{cloud}} and \code{\link[lattice]{wireframe}}.
	
	Three types of plots are handled slightly differently: 
	\itemize{
		\item Lattice graphics: a plot will be recognised as a Lattice plot if 
			the call is to one of the standard high-level functions.
			Otherwise you may need \code{is.lattice=TRUE}.
		\item base graphics: this is the default case. (If the call has the same
			name as a high-level Lattice function, you may need \code{is.lattice=FALSE}).
		\item grid graphics (not Lattice): you must give the \code{viewport}
			argument to enable interaction. This has not been well tested.
	}
	
	It is important to be aware that \code{playwith} makes a lot of guesses 
	about the plot, the data, and how to interact with them.
	Some interaction is based on evaluating and changing arguments to the plot call.
	This is designed to work in common cases, but could never work for all 
	plots. For basic interaction, ensure that the call accepts \code{xlim}
	and \code{ylim} arguments.
	Generally, it can be expected to work best with Lattice plots.
	
	The locations and type of data points will be guessed from the plot call.
	An attempt will also be made to construct labels to identify the data points.
	These guesses may be wrong in complex cases, in which case 
	you can pass the correct values in as \code{data.points} and/or \code{labels}.
	Please also contact the maintainer to help improve the guesses.
  
	In order to interact with a plot, its supporting data needs to be stored:
	i.e. all variables appearing in the plot call must remain accessible.
	By default, all non-global data is copied into an attached environment and stored.
	
	The default setting \code{eval.args=NA} causes variables appearing in the 
	plot call to be evaluated and stored, except if they are defined in the 
	global environment (i.e. user workspace). This method should work in most 
	cases, but: Functions appearing in the plot call will be evaluated each 
	time the plot is updated -- so random data as in \code{plot(rnorm(100))} 
	will keep changing, with confusing consequences! You should therefore 
	generate random data prior to the plot call. Changes to variables in the 
	global environment will also cause inconsistencies in previously generated 
	plots (e.g. labels on data points may be wrong).
	
	If \code{eval.args=T} then variables appearing in the plot call will be 
	evaluated and stored even if they are defined in the global environment. 
	Use this if the global variables might change (or be removed) before the 
	plot is destroyed.
	
	If a regular expression is given for \code{eval.args} then only variables 
	whose names match it will be evaluated, and this includes global variables, 
	as with \code{eval.args=T}. In this case you can set \code{invert.match=T} 
	to evaluate variables that are not matched.
	For example \code{eval.args="^tmp"} will evaluate variables whose names 
	begin with "tmp"; \code{eval.args="^foo$", invert=T} will evaluate 
	everything except \code{foo}.
	
	If \code{eval.args=F} then the plot call will be left alone 
	(not evaluated until plot time).
}
\value{
	\code{playwith} invisibly returns the value from the plot call.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{autoplay}}, \link{playTools}, \link{playwith.API} }
\examples{
\dontrun{

## `playwith` a base graphics plot: interaction and annotation
## -- from example(plot.default)
playwith(plot(cars$speed, cars$dist, xlab = "Speed", ylab = "Distance",
         panel.first = lines(lowess(cars$speed, cars$dist), lty = "dashed"),
         pch = 0, cex = 1.2, col = "blue"))

# you might get an error "Plot margins too small"; make window bigger and repeat

require(lattice)

## multiple lattice panels; identify points with nice labels
playwith(xyplot(Income ~ log(Population / Area) | state.region, 
	data=data.frame(state.x77)))

## same plot with 2 pages (2 panels per page); new plot window
playwith(xyplot(Income ~ log(Population / Area) | state.region, 
	data=data.frame(state.x77), layout=c(2,1)), name="other plot")

## time series plot: navigate x-axis only, transform y-axis
treering2 <- window(treering, start=1)
playwith(plot(treering2), nav.scales="x", trans.scales="y", 
	labels=paste(time(treering2),"CE"), 
	extra.buttons=list("logscale"))

## see what the current call is (for last focused plot window)
plotAndPlayGetState()$call

## example of evaluating and storing arguments in the plot call
globalOne <- 1:10
localStuff <- function() {
	localOne <- 10:1
	playwith(plot(localOne, globalOne))
}
localStuff()
## see which objects have been copied and stored with the plot
sapply(plotAndPlayGetState()$env, object.size)

## brushing multivariate scatterplots, see help(panel.brush.splom)
playwith(splom(environmental))

## simple spin and zoom for 3D plots
playwith(wireframe(volcano, drape=TRUE))

## example of the 'layers' button with a panel function
## -- from example(xyplot)
EE <- equal.count(ethanol$E, number=9, overlap=1/4)
playwith(xyplot(NOx ~ C | EE, data = ethanol,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       xlab = "Compression Ratio", ylab = "NOx (micrograms/J)",
       panel = function(x, y) {
           panel.grid(h=-1, v= 2)
           panel.xyplot(x, y)
           panel.loess(x,y, span=1)
       },
       aspect = "xy"))

## a code chunk: no interaction, only annotation, saving, etc
## -- from example(plot.default)
x <- 0:12
y <- sin(pi/5 * x)
playwith({
   op <- par(mfrow = c(3,3), mar = .1+ c(2,2,3,1))
   for (tp in c("p","l","b",  "c","o","h",  "s","S","n")) {
      plot(y ~ x, type = tp,
   	main = paste("plot(*, type = \"",tp,"\")",sep=""))
      if(tp == "S") {
         lines(x,y, type = "s", col = "red", lty = 2)
         mtext("lines(*, type = \"s\", ...)", col = "red", cex=.8)
      }
   }
   par(op)
})

## see help(plotAndPlayButtons) for examples of defining new buttons!

}
}

\keyword{ iplot }
\keyword{ dynamic }
\keyword{ device }

