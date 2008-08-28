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
         parameters = list(),
         tools = list(),
         update.actions = list(),
         init.actions = list(),
         ...,
         width = playwith.getOption("width"),
         height = playwith.getOption("height"),
         pointsize = playwith.getOption("pointsize"),
         eval.args = playwith.getOption("eval.args"),
         on.close = playwith.getOption("on.close"),
         modal = FALSE,
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
  \item{tools}{  }

  \item{\dots}{ extra arguments are stored in the \code{\link{playState}} object.
    These can then be accessed by user-defined tools. The default tools will recognise the following
    extra arguments:
    \describe{
      \item{\code{show.tooltips}}{
	show tooltips for toolbar items. This uses the GTK event loop internally,
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
  \item{pointsize}{ default point size for text in the
    \code{\link[cairoDevice]{Cairo}} device. }
  \item{eval.args}{ whether to evaluate the plot call arguments: can be
    \code{TRUE}, \code{FALSE}, \code{NA} (don't eval global vars)
    or a \link[=regex]{regular expression} matching symbols to evaluate.
    Or a list. See below. }
  \item{on.close}{ a function to be called when the user closes the plot
    window. The \code{\link{playState}} object
    will passed to the function. If the function returns \code{TRUE},
    the window will not be closed. }
  \item{modal}{ whether the window is modal: if \code{TRUE},
    the session will freeze until the window is closed. }
  \item{playState}{ the \code{\link{playState}} object for an existing plot window.
    If given, the new plot will appear in that window, replacing the old plot.
    This over-rides the \code{new} argument.}
  \item{plot.call}{ a plot call (\code{\link{call}} object), if given
    this is used instead of \code{expr}. }
  \item{main.function}{ the function (or its name) appearing in the call
    which accepts typical plot arguments like \code{xlim} and
    \code{ylab}. This will only be needed in unusual cases when the
    default guess fails. }
}
\details{
  This function opens a GTK+ window containing a plot device
  (from the \pkg{cairoDevice} package) and several toolbars.
  There is a call toolbar (similar to the "address bar" of a web browser) at the top,
  showing the current plot call, which can also be edited in-place.
  Then there are up to four toolbars, one on each side of the plot.
  The tools to be placed in these four toolbars can be specified
  if necessary.

  With the \code{\link{autoplay}} facility, \code{playwith} can function
  like a default graphics device (although it is not technically a
  graphics device itself, it is a wrapper around one).

  See \link{playTools} for a description of the pre-defined tools,
  and \link{playwith.API} for help on defining new tools.
  For the special case of tools to control parameter values, it is possible
  to create the tools automatically using the \code{parameters} argument.

  Four types of plots are handled somewhat differently:
  \itemize{
    \item \pkg{Lattice} graphics: recognised by returning an object of class
    \code{trellis}. This is the best-supported case.
    \item \pkg{ggplot2} graphics: recognised by returning an object of class \code{ggplot}.
    This case is rather poorly supported.
    \item other \pkg{grid} graphics: you must give the \code{viewport}
    argument to enable interaction.
    \item base graphics: this is the default case. If a multiple-plot
    layout is used, interaction can only work in the last sub-plot, i.e.
    the settings defined by \code{par()}.
  }

  Some forms of interaction are based on evaluating and changing arguments to the plot call.
  This is designed to work in common cases, but could never work for all
  types of plots. To enable zooming, ensure that the main call accepts \code{xlim}
  and \code{ylim} arguments. Furthermore, you may need to specify \code{main.function} if the
  relevant high-level call is nested in a complex block of expressions.

  To enable identification of data points, the locations of data points
  are required, along with appropriate labels.
  By default, these locations and labels will be guessed from the plot call,
  but this may fail.
  You can pass the correct values in as \code{data.points} and/or \code{labels}.
  Please also contact the maintainer to help improve the guesses.
  If identification of data points is not required, passing
  \code{data.points = NA, labels = NA} may speed things up.

  Some lattice functions require \code{subscripts = TRUE} in order to correctly
  identify points in a multiple-panel layout. Otherwise the subscripts used will then
  refer to the data in each panel separately, rather than the original dataset.
  In this case a warning dialog box will be shown.

  In order to interact with a plot, its supporting data needs to be stored:
  i.e. all variables appearing in the plot call must remain accessible.
  By default (\code{eval.args = NA}), objects that are not globally
  accessible will be copied into an attached environment and stored with
  the plot window.
  I.e. objects are stored unless they exist in the global environment
  (user workspace) or in an attached namespace.
  This method should work in most cases.
  However, it may end up copying more data than is really necessary,
  potentially using up memory. Note that if e.g. \code{foo$bar} appears
  in the call, the whole of \code{foo} will be copied.

  If \code{eval.args = TRUE} then variables appearing in the plot call will be
  evaluated and stored even if they are defined in the global environment.
  Use this if the global variables might change (or be removed) before the
  plot is destroyed.

  If \code{eval.args = FALSE} then the plot call will be left alone
  and no objects will be copied. This is OK if all the data are
  globally accessible, and will speed things up.

  If a regular expression is given for \code{eval.args} then only variables
  whose names match it will be evaluated, and this includes global variables,
  as with \code{eval.args=TRUE}. In this case you can set \code{invert.match=TRUE}
  to store variables that are not matched.
  For example \code{eval.args="^tmp"} will store variables whose names
  begin with "tmp"; \code{eval.args=list("^foo$", invert.match=TRUE)} will store
  everything except \code{foo}.

  \emph{Note:} function calls appearing in the plot call will be evaluated each
  time the plot is updated -- so random data as in \code{plot(rnorm(100))}
  will keep changing, with confusing consequences! You should therefore
  generate random data prior to the plot call. Changes to variables
  in the workspace (if they are not stored locally) may also cause inconsistencies in previously generated
  plots.

  \emph{Warning:} the playwith device will tend to make itself the active
  device any time it is clicked on, so be careful if any other devices
  are left open.
}
\value{
  \code{playwith} invisibly returns the \code{\link{playState}} object representing
  the plot, window and device.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{autoplay}}, \link{playTools}, \link{playwith.API},
  \code{\link{playwith.options}}, \code{\link{latticist}} }
\examples{
if (interactive()) {
options(device.ask.default = FALSE)

## Scatterplot (Lattice graphics).
## Labels are taken from rownames of data.
## Just click on the plot to identify points.
playwith(xyplot(Income ~ log(Population / Area),
   data = data.frame(state.x77), groups = state.region,
   type = c("p", "smooth"), span = 1, auto.key = TRUE,
   ylab = "Income per capita, 1974"))

## Scatterplot (base graphics); similar.
urbAss <- USArrests[,c("UrbanPop", "Assault")]
playwith(plot(urbAss, panel.first = lines(lowess(urbAss)),
   col = "blue", main = "Assault vs urbanisation",
   xlab = "Percent urban population, 1973",
   ylab = "Assault arrests (per 100,000), 1973"))

## Time series plot (Lattice).
## Date-time range can be entered directly in "time mode"
## (supports numeric, Date, POSIXct, yearmon and yearqtr).
## Click and drag to zoom in; right-click to zoom out;
## and use the scrollbar to move along the x-axis.
library(zoo)
playwith(xyplot(sunspots ~ yearmon(time(sunspots)),
                xlim = c(1900, 1930), type = "l"))

## Time series plot (base graphics); similar.
## Custom labels are passed directly to playwith.
## Label style can also be set with playwith.options(),
## or from a menu item inside the window.
tt <- time(treering)
treeyears <- paste(abs(tt) + (tt <= 0),
                  ifelse(tt > 0, "CE", "BCE"))
playwith(plot(treering, xlim = c(1000, 1300)),
   labels = treeyears, label.style = gpar(col="red",
      fontfamily = "HersheySans", cex = 0.7))

## Multi-panel Lattice plot.
## Need subscripts = TRUE to correctly identify points.
## Scales are "same" so zooming affects all panels.
Depth <- equal.count(quakes$depth, number = 3, overlap = 0.1)
playwith(xyplot(lat ~ long | Depth, data = quakes,
      subscripts = TRUE, aspect = "iso", pch = ".", cex = 2),
   labels = paste("mag", quakes$mag))

## Interactive control of a parameter with a slider.
xx <- rnorm(50)
playwith(plot(density(xx, bw = bandwidth), panel.last = rug(xx)),
	parameters = list(bandwidth = seq(0.05, 1, by = 0.01)))

## The same with a spinbutton (use I() to force spinbutton).
## Initial value is set as the first in the vector of values.
## This also shows a combobox for selecting text options.
xx <- rnorm(50)
kernels <- c("gaussian", "epanechnikov", "rectangular",
   "triangular", "biweight", "cosine", "optcosine")
playwith(plot(density(xx, bw = bandwidth, kern = kernel), lty = lty),
	parameters = list(bandwidth = I(c(0.1, 5:100/100)),
            kernel = kernels, lty = 1:6))

## More parameters (logical, numeric, text).
playwith(stripplot(yield ~ site, data = barley,
    jitter = TRUE, type = c("p", "a"),
    aspect = aspect, groups = barley[[groups]],
    scales = list(abbreviate = abbrev),
    par.settings = list(plot.line = list(col = linecol))),
  parameters = list(abbrev = FALSE, aspect = 0.5,
                    groups = c("none", "year", "variety"),
                    linecol = "red"))

## Composite plot (base graphics).
## Adapted from an example in help("legend").
## In this case, the initial plot() call is detected correctly;
## in more complex cases may need e.g. main.function="plot".
## Here we also construct data points and labels manually.
x <- seq(-4*pi, 4*pi, by = pi/24)
pts <- data.frame(x = x, y = c(sin(x), cos(x), tan(x)))
labs <- rep(c("sin", "cos", "tan"), each = length(x))
labs <- paste(labs, round(180 * x / pi) \%\% 360)
playwith( {
   plot(x, sin(x), type = "l", xlim = c(-pi, pi),
       ylim = c(-1.2, 1.8), col = 3, lty = 2)
   points(x, cos(x), pch = 3, col = 4)
   lines(x, tan(x), type = "b", lty = 1, pch = 4, col = 6)
   legend("topright", c("sin", "cos", "tan"), col = c(3,4,6),
       lty = c(2, -1, 1), pch = c(-1, 3, 4),
       merge = TRUE, bg = 'gray90')
}, data.points = pts, labels = labs)

## Simple spin and zoom for a 3D Lattice plot.
## Drag on the plot to rotate in 3D.
playwith(cloud(-depth ~ long * lat, quakes,
    zlab = "altitude", screen = list()))

## Brushing a multivariate scatterplot.
playwith(splom(environmental))

## A ggplot example.
## NOTE: only qplot()-based calls will work.
## Labels are taken from rownames of the data.
library(ggplot2)
playwith(qplot(qsec, wt, data = mtcars) + stat_smooth())

## A minimalist grid plot.
## This shows how to get playwith to work with custom plots:
## accept xlim/ylim and pass "viewport" to enable zooming.
myGridPlot <- function(x, y, xlim = NULL, ylim = NULL, ...)
{
   if (is.null(xlim)) xlim <- extendrange(x)
   if (is.null(ylim)) ylim <- extendrange(y)
   grid.newpage()
   pushViewport(plotViewport())
   grid.rect()
   pushViewport(viewport(xscale = xlim, yscale = ylim,
      name = "theData"))
   grid.points(x, y, ...)
   grid.xaxis()
   grid.yaxis()
   upViewport(0)
}
playwith(myGridPlot(1:10, 11:20, pch = 17), viewport = "theData")

## Presenting the window as a modal dialog box.
## When the window is closed, ask user to confirm.
## Use only a subset of the default tools, and hide plot call.
subTools <- list("identify", "clear", "zoom", "zoomout", "zoomfit")
confirmClose <- function(playState) {
	if (gWidgets::gconfirm("Close window and report IDs?")) {
		cat("Indices of identified data points:\n")
		print(playGetIDs(playState))
		return(FALSE) ## close
	} else TRUE ## don't close
}
xy <- data.frame(x = 1:20, y = rnorm(20), row.names = letters[1:20])
playwith(plot(xy), on.close = confirmClose, modal = TRUE,
	width = 4, height = 3.5, show.call = FALSE,
	top = NULL, left = subTools)

## Demonstrate cacheing of objects in local environment.
## By default, only local variables in the plot call are stored.
x_global <- rnorm(100)
doLocalStuff <- function(...) {
   y_local <- rnorm(100)
   angle <- (atan2(y_local, x_global) / (2*pi)) + 0.5
   color <- hsv(h = angle, v = 0.75)
   doRays <- function(x, y, col) {
      segments(0, 0, x, y, col = col)
   }
   playwith(plot(x_global, y_local, pch = 8, col = color,
      panel.first = doRays(x_global, y_local, color)),
   ...)
}
doLocalStuff(title = "locals only") ## eval.args = NA is default
## List objects that have been copied and stored:
sapply(playDevCur()$env, object.size)
## i.e. if you rm(x_global) now, redraws will fail.
## Next: store all data objects (in a new window):
doLocalStuff(title = "all stored", eval.args = TRUE, new = TRUE)
sapply(playDevCur()$env, object.size)
## Now there are two devices open:
playDevList()
playDevCur()
playDevOff()
playDevCur()

\dontrun{
## Memory usage test.
## Big data object, do not try to guess labels or time.mode.
gc()
bigobj <- rpois(5000000, 1)
object.size(bigobj) / 1048576 ## in MB
gc()
playwith(qqmath(~ bigobj, f.value = ppoints(500)),
   data.points = NA, labels = NA, time.mode = FALSE)
playDevOff()
gc()
## or generate the trellis object first:
trel <- qqmath(~ bigobj, f.value = ppoints(500))
playwith(update(trel))
rm(trel)
## in this case, better to compute the sample first:
subobj <- quantile(foo, ppoints(500), na.rm = TRUE)
playwith(qqmath(~ subobj))
rm(subobj)
rm(bigobj)
}

## See help(playwith.API) for examples of new tools.
}
}
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ device }

