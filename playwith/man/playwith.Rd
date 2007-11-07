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
  Open a GTK+ window to view and interact with a plot.
}
\usage{

playwith(expr, 
	title = NULL, 
	playDev = NULL,
	...,
	time.mode = FALSE,
	top.tools = playApplicationTools, 
	left.tools = playInteractionTools, 
	bottom.tools = list(),
	right.tools = list(), 
	show.call = TRUE,
	label.points = NULL, 
	labels = NULL, 
	label.style = list(cex=1),
	is.lattice = NA, 
	viewport = NULL, 
	eval.args = NA, 
	invert.match = F, 
	envir = parent.frame(), 
	win.size = c(640, 480), 
	modal = FALSE,
	deletable = TRUE,
	restore.on.close = NULL, 
	plot.call)
	
playwith(expr, name = "plot", nav.scales = c("x","y"), trans.scales = c("y"), 
	 buttons = list("annotate", "identify", "zoom", "zoomfit"), 
	 extra.buttons = list("zero"), basic.buttons=plotAndPlayBasicButtons, 
	 labels = NULL, label.args = list(cex = 1), identify.call = NULL, 
	 plot.call, is.lattice = NA, eval.args = NA, invert.match = F, 
	 envir = parent.frame(), restore.on.close = NULL)
}
\arguments{
  \item{expr}{ an expression to create a plot, like \code{plot(mydata)}. 
  	Note, this can have lots of arguments, just like a normal plot call (see examples).
  	Can also be a chunk of code in \code{\{braces\}}, but that form does not allow interaction. }
  \item{name}{ character value to identify the plot window. }
  \item{nav.scales}{ one or more of \code{c("x","y")} defining which scale(s) to navigate along (e.g. zoom in). }
  \item{trans.scales}{ one or more of \code{c("x","y")} defining which scale(s) to transform (e.g. log). }
  \item{buttons}{ a list of buttons for the toolbar.
	Each element should be a call to generate a \code{\link[RGtk2]{gtkToolItem}}, or a character string. 
	If it is a character string, that element is taken from the pre-defined \code{plotAndPlayButtons}.
  	See the \code{Details} and \code{Examples} sections. }
  \item{extra.buttons}{ same as \code{buttons}, provided for convenient additions to the default set. }
  \item{basic.buttons}{ same as \code{buttons}, the default here is almost always useful, so you probably do not want to change this. }
  \item{labels}{ a character vector of labels for data points, for use in \code{\link{identify}}. 
  	If missing, it will be guessed from the plot call. If \code{identify.call} is given, this is ignored. }
  \item{label.args}{ a list of arguments passed to \code{\link{identify}} and \code{\link{text}}: can include \code{cex}, \code{col} and \code{font}. 
	Will also be added to \code{identify.call} if that is given. }
  \item{identify.call}{ a \code{\link{call}} to be evaluated when the identify button is clicked. 
  	If missing, it will be constructed from the plot call. Generally this argument should not be needed. }
  \item{plot.call}{ a plot call (\code{\link{call}} object), if given this is used instead of \code{expr}. }
  \item{is.lattice}{ whether the plot is a \link[lattice]{Lattice} plot, i.e. returns a trellis object.
  	If \code{NA}, the call name is matched against known Lattice function names. }
  \item{eval.args}{ whether to evaluate the plot call arguments: can be \code{TRUE}, \code{FALSE}, \code{NA} (don't eval global vars) 
	or a \link[=regex]{regular expression} matching symbols to evaluate. See below. }
  \item{invert.match}{ whether to evaluate arguments that do NOT match the regular expression given in \code{eval.args}. }
  \item{envir}{ \code{\link{environment}} to use in evaluating the call arguments (see \code{eval.args}) }
  \item{restore.on.close}{ a \code{gtkWindow} to bring to front when the plot window is closed. }
}
\details{
  This function generates a general-purpose graphical user interface for R plots.
  It tries to work out what kind of plot it is, and how you might want to interact with it.
  This is based firstly on whether the plot function name is recognised as a Lattice plot or not.
  If it is a not a standard high-level function you may need \code{is.lattice=T}.
  
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
	\code{playwith} invisibly returns the value from the plot call
	(which for lattice plots is a \code{\link[=print.trellis]{trellis}} object).
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{setAutoPlaywith}}, \code{\link{plotAndPlayButtons}}, 
	\code{\link[lattice]{xyplot}}, \link[lattice]{Lattice} }
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

