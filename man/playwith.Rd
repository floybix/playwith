\name{playwith}
\alias{playwith}
\title{An interactive plot GUI}
\description{
  Open a graphical user interface for viewing and interacting with \R plots.
  It tries to work out the structure of a plot, in order to interact with it.
  The built-in features include: navigating the data space, identifying data points,
  editing and annotating the plot, and saving to a file. New tools can be defined.
}
\usage{
playwith(expr,
         new = playwith.getOption("new"),
         title = NULL,
         labels = NULL,
         data.points = NULL,
         viewport = NULL,
         top.tools = playwith.getOption("top.tools"),
         left.tools = playwith.getOption("left.tools"),
         bottom.tools = playwith.getOption("bottom.tools"),
         right.tools = playwith.getOption("right.tools"),
         parameters = list(),
         ...,
         width = playwith.getOption("width"),
         height = playwith.getOption("height"),
         pointsize = playwith.getOption("pointsize"),
         modal = FALSE,
         on.close = NULL,
         show.call = TRUE,
         eval.args = playwith.getOption("eval.args"),
         invert.match = FALSE,
         envir = parent.frame(),
         playState = if (!new) playDevCur(),
         plot.call,
         main.function)
}
\arguments{
  \item{expr}{ an expression to create a plot, like \code{plot(mydata)}.
    Note, this can have lots of arguments, just like a normal plot call (see examples).
    Could also be a chunk of code in \code{\{}braces\code{\}}. }
  \item{new}{ if \code{TRUE} open in a new window, otherwise replace the current window (if one exists). }
  \item{title}{ optional window title. If a plot window exists with the same title,
    the new plot will appear in that window, replacing the old plot. }
  \item{labels}{ a character vector of labels for data points, for use in \code{\link{identify}}.
    If missing, it will be guessed from the plot call arguments. }
  \item{data.points}{ a data frame with elements "x" and "y" (or other suitable plotting structure)
    giving locations of data points, in case these can not be guessed from the plot call arguments. }
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
  \item{parameters}{
    defines simple tools for controlling values of any parameters
    appearing in the plot call. This must be a named list, where the value given for
    each name defines the possible or initial values of that parameter. The
    supported values are:
    \itemize{
      \item \code{integer} or \code{AsIs} (\code{I()}):
      creates a numeric spinbutton.
      \item \code{numeric} scalar: creates a text entry box for
      numeric values.
      \item \code{numeric} vector: creates a slider with given range.
      \item \code{character}: creates a text entry box.
      \item \code{character} vector: creates a combo box (including
      text entry).
      \item \code{logical}: creates a checkbox.
    }
  }
  \item{\dots}{ extra arguments are stored in the \code{\link{playState}} object.
    These can then be accessed by user-defined tools. The default tools will recognise the following
    extra arguments:
    \describe{
      \item{\code{show.tooltips}}{
	show tooltips for toolbar items. This uses the GTK event loop,
	which might, occasionally, cause the R terminal to freeze.
      }
      \item{\code{page}}{
	In multi-page Lattice plots, this will set the initial page to display.
      }
      \item{\code{time.mode}}{ whether the plot is to start in "time mode",
	with navigation along the x-axis.
      }
      \item{\code{time.vector}}{
	a vector defining discrete times, as numeric,
	\code{\link{Date}} or \code{\link[=DateTimeClasses]{POSIXt}}.
	It must be sorted, increasing.
	If given, then the "time mode" is used to navigate
	along these discrete times, rather than along the continuous x-axis.
	Special objects \code{cur.index} and \code{cur.time} will be provided in the
	plot environment, so the plot call can refer to these.
	\code{cur.index} is the current time step, between \code{1} and \code{length(time.vector)},
	and \code{cur.time} is \code{time.vector[cur.index]}.
	In this case \code{time.mode} will be on by default.
      }
      \item{\code{cur.index}, \code{cur.time}, \code{time.mode.page.incr}}{
	If \code{time.vector} is given, either of \code{cur.index}
	or \code{cur.time} will set the initial time step.
	\code{time.mode.page.incr} sets the number of steps to jump
	if the user clicks on the scroll bar.
      }
      \item{\code{label.style}}{
	output from \code{\link[grid]{gpar}}, specifying the style of labels.
	e.g. \code{gpar(cex=1.5, fontface="italic", fontfamily="serif")}.
      }
      \item{\code{label.offset}}{
	the distance from a data point to its identifying label.
	Can be numeric, in units of characters, or
	a grid \code{\link[grid]{unit}}.
	Default is 0.5, or \code{unit(0.5, "char")}.
      }
      \item{\code{arrow.style}}{
	output from \code{\link[grid]{gpar}}, specifying the style of arrows.
	e.g. \code{gpar(lwd=2, col="red", alpha=0.5)}.
      }
      \item{\code{arrow.arrow}}{
	output from \code{\link[grid]{arrow}}, specifying the type of arrows to draw.
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
  \item{width, height}{ initial size of the plot device in inches. }
  \item{pointsize}{ default point size for text, used in the
    \code{\link[cairoDevice]{Cairo}} device. }
  \item{modal}{ whether the window is modal: if \code{TRUE}, the session will freeze until the window is closed. }
  \item{on.close}{ a function to be called when the user closes the plot window. The \code{\link{playState}} object
    will passed to the function. If the function returns \code{TRUE}, the window will not be closed. }
  \item{show.call}{ set to \code{FALSE} to hide the plot call text-box ("address bar") and associated buttons.}
  \item{eval.args}{ whether to evaluate the plot call arguments: can be \code{TRUE}, \code{FALSE}, \code{NA} (don't eval global vars)
    or a \link[=regex]{regular expression} matching symbols to evaluate. See below. }
  \item{invert.match}{ whether to evaluate arguments that do NOT match the regular expression given in \code{eval.args}. }
  \item{envir}{ \code{\link{environment}} to use in evaluating the call arguments (see \code{eval.args}) }
  \item{playState}{ the \code{\link{playState}} object for an existing plot window.
    If given, the new plot will appear in that window, replacing the old plot.
    This over-rides the \code{new} argument.}
  \item{plot.call}{ a plot call (\code{\link{call}} object), if given
    this is used instead of \code{expr}. }
  \item{main.function}{ the function (or its name) appearing in the call
    which accepts typical plot arguments like \code{xlim} and
    \code{main}. This will only be needed in unusual cases when the
    default guess fails. }
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

  See \link{playTools} for a description of the pre-defined tools,
  and \link{playwith.API} for help on defining new tools.
  For the special case of tools to control parameter values, it is possible
  to create the tools automatically using the \code{parameters} argument.

  Four types of plots are handled slightly differently:
  \itemize{
    \item Lattice graphics: recognised by returning an object of class \code{trellis}.
    \item ggplot graphics: recognised by returning an object of class \code{ggplot}.
    This case is poorly supported.
    \item other grid graphics: you must give the \code{viewport}
    argument to enable interaction.
    \item base graphics: this is the default case.
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
  The automatic subscripts used to identify points in Lattice plots
  will be usually be the within-panel indices unless \code{subscripts=TRUE} is
  specified in the plot call: see \code{\link[lattice]{xyplot}}.
  If so a warning dialog box will be shown.

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

  If \code{eval.args=TRUE} then variables appearing in the plot call will be
  evaluated and stored even if they are defined in the global environment.
  Use this if the global variables might change (or be removed) before the
  plot is destroyed.

  If a regular expression is given for \code{eval.args} then only variables
  whose names match it will be evaluated, and this includes global variables,
  as with \code{eval.args=TRUE}. In this case you can set \code{invert.match=TRUE}
  to evaluate variables that are not matched.
  For example \code{eval.args="^tmp"} will evaluate variables whose names
  begin with "tmp"; \code{eval.args="^foo$", invert.match=TRUE} will evaluate
  everything except \code{foo}.

  If \code{eval.args=FALSE} then the plot call will be left alone
  (not evaluated until plot time).
}
\value{
  \code{playwith} invisibly returns the value from the plot call.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{autoplay}}, \link{playTools}, \link{playwith.API},
  \code{\link{playwith.options}}, \code{\link{latticist}} }
\examples{
if (interactive()) {

## scatterplot (base graphics)
## labels are taken from rownames of data.
## just click on the plot to identify points.
rownames(USArrests) <- state.name
playwith(plot(Assault ~ UrbanPop, data=USArrests,
  xlab="Percent urban population, 1973",
  ylab="Assault arrests (per 100,000), 1973"))

## time series plot (base graphics)
## click and drag to zoom; right-click to zoomout.
tt <- time(treering)
treeyears <- paste(abs(tt) + (tt <= 0),
                  ifelse(tt > 0, "CE", "BCE"))
playwith(plot(treering, xlim = c(1000, 1300)),
	labels = treeyears)

## time series plot (lattice)
## date-time range can be entered directly in time mode.
library(zoo)
playwith(xyplot(sunspots ~ yearmon(time(sunspots)),
                xlim=c(1900, 1930), type="l"))

## multi-panel lattice plot
## note label style can also be set with playwith.options()
playwith(xyplot(Income ~ log(Population / Area) | state.region,
	data = data.frame(state.x77), subscripts=TRUE),
	label.style = gpar(col="red", fontfamily="HersheySans",
	cex=0.7))

## interactive control of a parameter with a slider
xx <- rnorm(50)
playwith(plot(density(xx, bw = bandwidth), panel.last = rug(xx)),
	parameters = list(bandwidth = seq(0.05, 1, by=0.01)))

## the same with a spinbutton (use I() to force spinbutton).
## initial value is set as the first in the vector of values.
xx <- rnorm(50)
playwith(plot(density(xx, bw = bandwidth), panel.last = rug(xx)),
	parameters = list(bandwidth = I(c(0.1, 5:100/100))) )

## more parameters
playwith(stripplot(yield ~ site, data = barley,
    jitter=TRUE, type=c("p","a"),
    aspect=aspect, groups=barley[[groups]],
    scales=list(abbreviate=abbrev),
    par.settings=list(plot.line=list(col=linecol))),
  parameters = list(abbrev=FALSE, aspect=0.5,
                    groups=c("none", "year", "variety"),
                    linecol="red"))

## brushing a multivariate scatterplot
playwith(splom(environmental))

## simple spin and zoom for a 3D plot
playwith(wireframe(volcano, drape=TRUE))

## ggplot (NOTE: only qplot()-based calls will work)
## labels come from data frame automatically.
library(ggplot2)
playwith(qplot(qsec, wt, data=mtcars) + stat_smooth())

## a minimalist grid plot
gridPlot <- function(x, y, xlim = NULL, ylim = NULL) {
	if (is.null(xlim)) xlim <- extendrange(x)
	if (is.null(ylim)) ylim <- extendrange(y)
	grid.newpage()
	pushViewport(plotViewport())
	grid.rect()
	pushViewport(viewport(xscale=xlim, yscale=ylim,
		name="theData"))
	grid.points(x, y)
	grid.xaxis()
	grid.yaxis()
	upViewport(0)
}
playwith(gridPlot(1:10, 11:20), viewport="theData")

## acting like a dialog box (confirm close)
subTools <- list("identify", "clear", "zoom", "zoomout", "zoomfit")
confirmClose <- function(playState) {
	if (gWidgets::gconfirm("Close window and report IDs?")) {
		cat("Indices of identified data points:\n")
		print(playGetIDs(playState))
		return(FALSE)
	} else TRUE ## don't close
}
xy <- list(x=rnorm(20), y=rnorm(20))
playwith(plot(xy), on.close=confirmClose, modal=TRUE,
	width=4, height=3.5, show.call=FALSE,
	top=NULL, left=subTools)

## only local variables appearing in the plot call are stored
## (use eval.args=TRUE to store all data)
globalOne <- rnorm(1000)
localStuff <- function() {
	localOne <- rnorm(1000)
	playwith(plot(localOne, globalOne))
}
localStuff()
## list objects that have been copied and stored
sapply(playDevCur()$env, object.size)
## if global object is removed, redraws will fail
rm(globalOne)

## see help(playwith.API) for examples of new tools.

}
}
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ device }

