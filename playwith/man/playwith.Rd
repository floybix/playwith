\name{playwith}
\alias{playwith}
\alias{plotAndPlay}
\alias{toolConstructors}
\alias{playApplicationTools}
\alias{playInteractionTools}
\alias{play3DTools}
\alias{playSplomTools}
\title{An interactive plot GUI}
\description{
  Open a graphical user interface for viewing and interacting with a plot.
  It tries to work out what kind of plot it is, and how you might want to interact with it.
  The built-in features include: navigating the data space, identifying data points,
  editing and annotating the plot, and saving to a file. New tools can be defined.
}
\usage{
playwith(expr, 
	title = NULL, 
	playState = NULL,
	time.mode = FALSE,
	labels = NULL, 
	data.points = NULL, 
	is.lattice = NA, 
	viewport = NULL, 
	...,
	top.tools = playApplicationTools, 
	left.tools = playInteractionTools, 
	bottom.tools = list(),
	right.tools = list(), 
	show.call = TRUE,
	win.size = c(640, 480), 
	modal = FALSE,
	on.close = NULL, 
	eval.args = NA, 
	invert.match = F, 
	envir = parent.frame(), 
	plot.call)
}
\arguments{
  \item{expr}{ an expression to create a plot, like \code{plot(mydata)}. 
  	Note, this can have lots of arguments, just like a normal plot call (see examples).
  	Can also be a chunk of code in \code{\{}braces\code{\}}, but that form does not allow interaction. }
  \item{title}{ optional window title. If a plot window exists with the same title, 
  	the new plot will appear in that window, replacing the old plot. }
  \item{playState}{ the \code{\link{playState}} object for an existing plot window.
	If given, the new plot will appear in that window, replacing the old plot.}
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
			a \code{list} of arguments passed to \code{\link{text}}
			or \code{\link{panel.text}}, to specify the style of labels: 
			can include \code{cex}, \code{col} and \code{font}.
		}
		\item{\code{annotation.mode}}{
			\code{"figure"} (the default) or \code{"plot"}.
		}
	}
  }
  \item{top.tools, left.tools, bottom.tools, right.tools}{
	each a list of tool widgets for toolbars at the top, left, bottom or right sides of the window.
	Each list element should be the name of one of the pre-defined tools in \code{toolConstructors},
	or a function to construct a \code{\link[RGtk2]{gtkToolItem}}. See \code{\link{quickTool}} for details
	of defining new tools. }
  \item{show.call}{ set to \code{FALSE} to hide the plot call text-box ("address bar") and associated buttons.}
  \item{win.size}{ default initial size of the plot window (width, height) in pixels. }
  \item{modal}{ whether the window is modal: if \code{TRUE}, the session will freeze until the window is closed. }
  \item{on.close}{ a function to be called when the user closes the plot window. The \code{\link{playState}} object
	  will passed to the function. If the function returns \code{TRUE}, the window will not be closed. }
  \item{eval.args}{ whether to evaluate the plot call arguments: can be \code{TRUE}, \code{FALSE}, \code{NA} (don't eval global vars) 
	or a \link[=regex]{regular expression} matching symbols to evaluate. See below. }
  \item{invert.match}{ whether to evaluate arguments that do NOT match the regular expression given in \code{eval.args}. }
  \item{envir}{ \code{\link{environment}} to use in evaluating the call arguments (see \code{eval.args}) }
  \item{plot.call}{ a plot call (\code{\link{call}} object), if given this is used instead of \code{expr}. }
}
\details{
	This function opens a GTK+ window containing a plot device 
	(a Cairo-based device from the \code{cairoDevice} package)
	and several toolbars.
	
	
  This is based firstly on whether the plot function name is recognised as a Lattice plot or not.
  If it is a not a standard high-level function you may need \code{is.lattice=TRUE}.
  
  \code{\link[lattice]{wireframe}} and \code{\link[lattice]{cloud}}

  \code{\link[lattice]{splom}}
  
  
  
  
  
  In order for the plot to be modified, its supporting data needs to be stored.
  By default, all non-global data is copied into an attached environment and stored.
  More on this below.
  
  This function will also try to guess labels for data points. 
  It does this by evaluating the \code{x} argument (\code{data} argument for Lattice plots), 
  or part of the formula. To suppress this evaluation, give \code{labels} explicitly 
  (may be \code{labels=NA}).
  
  Each element of \code{buttons} should be a call to generate a \code{\link[RGtk2]{gtkToolItem}} 
  (typically a \code{\link[RGtk2]{gtkToolButton}}),
  or text, in which case it is taken from the pre-defined set.
  Type \code{str(plotAndPlayButtons)} to see the pre-defined set of button handlers.
  However, you can usually ignore this, as most buttons will be automatically added to the toolbar for relevant plots. 
  
  
  Specifically: \itemize{
	  \item \code{"expand"} will be added to lattice plots with more than one panel per page. 
	  An alternative is \code{"focus"}, which is the same but does not expand the chosen panel to the full window.
	  \item \code{"pages"} will be added to lattice plots with more than one page.
	  \item \code{"layers"} might be added to lattice plots with any of the arguments \code{"layers", "sp.layout", "panel"}.
	  \item \code{c("zoomin.3d", "zoomout.3d", "fly.left.3d", "fly.right.3d")} will be added to \code{\link[lattice]{wireframe}} and \code{\link[lattice]{cloud}} 
	  (note, these do not yet work with non-Lattice plots like \code{\link{persp}}).
	  \item \code{c("brush", "brush.region", "brush.drag", "clear")} will be added to \code{\link[lattice]{splom}} 
	  (note, these do not yet work with non-Lattice plots like \code{\link{pairs}}).
  }
  But these may be overridden by specifying \code{buttons} explicitly.
  
  See \code{\link{plotAndPlayButtons}} if you want to define new buttons.
  
  The interaction features require that all variables appearing in the plot call remain accessible:
  
  The default setting \code{eval.args=NA} causes variables appearing in the plot call to be 
  \link[=eval]{evaluated} and stored, except if they are defined in the global environment (i.e. user workspace). 
  This method should work in most cases, but:
  Functions appearing in the plot call will be evaluated each time the plot is updated -- 
  so random data as in \code{plot(rnorm(100))} will keep changing, with confusing consequences!
  You should therefore generate random data prior to the plot call.
  Changes to variables in the global environment will also cause inconsistencies in previously generated plots 
  (e.g. \code{labels} on data points may be wrong).
  
  If \code{eval.args=T} then variables appearing in the plot call will be evaluated and stored
  even if they are defined in the global environment. 
  Use this if the global variables might change (or be removed) before the plot is destroyed.
  
  If a regular expression is given for \code{eval.args} then only variables whose names match it will be evaluated,
  and this includes global variables, as with \code{eval.args=T}. 
  In this case you can set \code{invert.match=T} to evaluate variables that are not matched.
  For example \code{eval.args="^tmp"} will evaluate variables whose names begin with "tmp";
  \code{eval.args="^foo$", invert=T} will evaluate everything except \code{foo}.
  
  If \code{eval.args=F} then the plot call will be left alone (not evaluated until plot time).
}
\value{
	\code{playwith} invisibly returns the value from the plot call.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{autoplay}}, \code{\link{playState}}, \code{\link{quickTool}},
	\link[lattice]{Lattice} }
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

