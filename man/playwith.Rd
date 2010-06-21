\name{playwith}
\alias{playwith}
\alias{plotOnePage}
\title{An interactive plot GUI}
\description{
  A GTK+ graphical user interface for exploring and editing \R plots.
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
         init.actions = list(),
         preplot.actions = list(),
         update.actions = list(),
         ...,
         width = playwith.getOption("width"),
         height = playwith.getOption("height"),
         pointsize = playwith.getOption("pointsize"),
         eval.args = playwith.getOption("eval.args"),
         on.close = playwith.getOption("on.close"),
         modal = FALSE,
         link.to = NULL,
         playState = if (!new) playDevCur(),
         plot.call,
         main.function)
}
\arguments{
  \item{expr}{
    an expression to create a plot, like \code{plot(mydata)}.
    Note, arguments and nested calls are allowed, just like a normal
    plot call (see examples).
    Could also be a chunk of code in \code{\{}braces\code{\}}.
    For quoted calls, use the \code{plot.call} argument.
  }
  \item{new}{
    if \code{TRUE} open in a new window, otherwise replace the
    current window (if one exists).
  }
  \item{title}{
    optional window title; otherwise derived from the plot call.
  }
  \item{labels}{
    a character vector of labels for data points.
    If missing, it will be guessed from the plot call arguments if
    possible.
  }
  \item{data.points}{
    a data frame (or other suitable plotting
    structure: see \code{\link{xy.coords}}) giving locations of data
    points, in case these can not be guessed from the plot call
    arguments. If a data frame, extra variables may be included; these
    can be used to label or locate points in the GUI. Note, if a
    suitable data argument is found in the plot call, that plays the
    same role.
  }
  \item{viewport}{
    name or \code{\link[grid]{vpPath}} of the
    \code{\link[grid]{viewport}} representing the data space. This
    allows interaction with grid graphics plots (but ignore this for
    Lattice plots). Experimental: can also be a named list.
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
      \item \code{function}: creates a button, which calls
      the given function with a single argument \code{playState}.
    }

    These can also be lists, where the first item is the value as
    above. In this case an item named \code{label} can specify a label
    for the widget, and an item named \code{handler} can specify a
    function to run when the widget is changed. This function should be
    a \code{function(playState, value)}; the parameter values are then
    accessed from \code{playState$env}. If the function returns
    \code{FALSE} the plot is not redrawn.
  }
  \item{tools}{
    a list of tool specifications. These are technically
    \code{\link[RGtk2]{GtkActionEntry}}s but should be specified as
    lists with the following structure. Elements can be specified in
    this order, or named (as with a function call).
    \describe{
      \item{\code{name}}{
	The name of the action (used internally to control
	the action state, or in a custom UI XML file). This item is
	required and must be the first element. All other elements are
	optional.
      }
      \item{\code{stock_id}}{
	The stock icon ID, or the name of an icon from the
	icon theme. See \code{unlist(gtkStockListIds())} or
	\url{http://library.gnome.org/devel/gtk/unstable/gtk-Stock-Items.html}
	for a list.
      }
      \item{\code{label}}{
	The label for the action. If label is NULL, the default
	label for the given stock.id is used.
      }
      \item{\code{accelerator}}{
	The accelerator for the action, in the format
	understood by \code{\link[RGtk2]{gtkAcceleratorParse}}. See
	\link[RGtk2]{gdkKeySyms}.
      }
      \item{\code{tooltip}}{
	The tooltip for the action.
      }
      \item{\code{callback}}{
	The function to call when the action is activated.
      }
      \item{\code{is_active}}{
	Only for toggle actions: sets the initial state (TRUE / FALSE).
      }
      \item{\code{update.action}, \code{init.action}}{
	If present these items must be named. Their values are included
	in the \code{update.actions} and \code{init.actions} lists.
      }
    }
  }
  \item{preplot.actions, update.actions}{
    a list of actions to be run, respectively, \emph{before} and
    \emph{after} the plot is drawn (and each time it is redrawn).
    Note that \code{preplot.actions} can not assume that
    \code{playState$is.lattice} (or other state values) are set. They
    can, however, modify the plot call or its data before the plot is
    drawn.

    These may be functions,
    or names of functions, or expressions. Functions are passed one
    argument, which is the \code{\link{playState}}. Note, these
    are in addition to any given in
    \code{\link{playwith.options}("update.actions")}.
  }
  \item{init.actions}{
    \code{init.actions} are run whenever the plot type changes or its
    data changes. They are not run when only simple arguments to the
    call change, but they are run whenever the plot call is edited
    manually. Same format as \code{update.actions}.
  }
  \item{\dots}{
    extra arguments are stored in the \code{\link{playState}} object.
    These can then be accessed by tools. The default tools
    will recognise the following extra arguments:
    \describe{
      \item{\code{click.mode}}{
	sets the initial action when clicking
	and dragging on the plot: one of \code{"Zoom"},
	\code{"Identify"}, \code{"Brush"}, \code{"Annotation"}, or
	\code{"Arrow"}.
      }
      \item{\code{time.mode}}{
	whether the plot is to start in "time mode",
	with navigation along the x-axis. If \code{NA}, it will guess
	whether to start in time.mode based on whether the current plot
	looks like a time series plot (but this can chew some extra
	memory). The default is taken from
	\code{\link{playwith.options}("time.mode")}.
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
      \item{\code{page}}{
	In multi-page Lattice plots, this will set the initial page to display.
      }
      \item{\code{label.offset}}{
	the distance from a data point to its identifying label.
	Numeric, in units of character widths.
      }
      \item{\code{arrow}}{
	a list with arguments to \code{\link[lattice]{panel.arrows}},
	specifying the type of arrows to draw.
	e.g. \code{list(ends="both", type="closed")}.
      }
      \item{\code{show.tooltips}}{
	show tooltips for toolbar items. This uses the GTK event loop internally,
	which might, occasionally, cause the R terminal to freeze.
      }
      \item{ \code{show.toolbars}, \code{show.statusbar},
	\code{page.annotation}, \code{clip.annotations},
	\code{keep}, \code{stay.on.top}}{
	set the corresponding window options. All are logical. Defaults
	are taken from \code{\link{playwith.options}}.
      }
    }
  }
  \item{width, height}{
    initial size of the plot device in inches.
  }
  \item{pointsize}{
    default point size for text in the
    \code{\link[cairoDevice]{Cairo}} device.
  }
  \item{eval.args}{
    whether to evaluate the plot call arguments: can be
    \code{TRUE}, \code{FALSE}, \code{NA} (don't eval global vars)
    or a \link[=regex]{regular expression} matching symbols to evaluate.
    Or a list. See below.
  }
  \item{on.close}{
    a function to be called when the user closes the plot
    window. The \code{\link{playState}} object
    will passed to the function. If the function returns \code{TRUE},
    the window will not be closed.
  }
  \item{modal}{
    whether the window is modal: if \code{TRUE},
    the session will freeze until the window is closed.
  }
  \item{link.to}{
    an existing \code{playState} (i.e. \code{playwith}
    plot) to link to. The set of brushed data points will then be
    synchronised between them. It is assumed that the data subscripts of
    the two plots correspond directly. Links can be broken with
    \code{\link{playUnlink}}.
  }
  \item{playState}{
    the \code{\link{playState}} object for an existing plot window.
    If given, the new plot will appear in that window, replacing the old plot.
    This over-rides the \code{new} argument.
  }
  \item{plot.call}{
    a plot call (\code{\link{call}} object), if given
    this is used instead of \code{expr}.
  }
  \item{main.function}{
    the function (or its name) appearing in the call
    which accepts typical plot arguments like \code{xlim} or
    \code{ylab}. This will only be needed in unusual cases when the
    default guess fails.
  }
}
\details{
  This function opens a GTK+ window containing a plot device
  (from the \pkg{cairoDevice} package), a menubar and toolbars.
  There is a call toolbar (similar to the "address bar" of a web browser) at the top,
  showing the current plot call, which can be edited in-place.
  Then there are up to four toolbars, one on each side of the plot.
  The user interface is customisable: see \code{\link{playwith.options}}.

  With the \code{\link{autoplay}} facility, \code{playwith} can function
  like a default graphics device (although it is not technically a
  graphics device itself, it is a wrapper around one).

  See \link{playwith.API} for help on controlling the plot once open, as
  well as defining new tools.
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

  Some lattice functions need to be called with \code{subscripts = TRUE}
  in order to correctly
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
  in the workspace (if they are not stored locally) may also cause
  inconsistencies in previously generated plots.

  \emph{Warning:} the playwith device will tend to make itself the active
  device any time it is clicked on, so be careful if any other devices
  are left open.
}
\value{
  \code{playwith} invisibly returns the \code{\link{playState}} object representing
  the plot, window and device. The result of the plot call is available
  as component \code{$result}.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{
  \code{\link{playwith.options}},
  \code{\link{autoplay}},
  \link{playwith.API}
}
\examples{
if (interactive()) {
options(device.ask.default = FALSE)

## Scatterplot (Lattice graphics).
## Labels are taken from rownames of data.
## Right-click on the plot to identify points.
playwith(xyplot(Income ~ log(Population / Area),
   data = data.frame(state.x77), groups = state.region,
   type = c("p", "smooth"), span = 1, auto.key = TRUE,
   xlab = "Population density, 1974 (log scale)",
   ylab = "Income per capita, 1974"))

## Scatterplot (base graphics); similar.
## Note that label style can be set from a menu item.
urbAss <- USArrests[,c("UrbanPop", "Assault")]
playwith(plot(urbAss, panel.first = lines(lowess(urbAss)),
   col = "blue", main = "Assault vs urbanisation",
   xlab = "Percent urban population, 1973",
   ylab = "Assault arrests per 100k, 1973"))

## Time series plot (Lattice).
## Date-time range can be entered directly in "time mode"
## (supports numeric, Date, POSIXct, yearmon and yearqtr).
## Click and drag to zoom in, holding Shift to constrain;
## or use the scrollbar to move along the x-axis.
library(zoo)
playwith(xyplot(sunspots ~ yearmon(time(sunspots)),
                xlim = c(1900, 1930), type = "l"),
         time.mode = TRUE)

## Time series plot (base graphics); similar.
## Custom labels are passed directly to playwith.
tt <- time(treering)
treeyears <- paste(abs(tt) + (tt <= 0),
                  ifelse(tt > 0, "CE", "BCE"))
playwith(plot(treering, xlim = c(1000, 1300)),
   labels = treeyears, time.mode = TRUE)

## Multi-panel Lattice plot.
## Need subscripts = TRUE to correctly identify points.
## Scales are "same" so zooming applies to all panels.
## Use the 'Panel' tool to expand a single panel, then use
## the vertical scrollbar to change pages.
Depth <- equal.count(quakes$depth, number = 3, overlap = 0.1)
playwith(xyplot(lat ~ long | Depth, data = quakes,
      subscripts = TRUE, aspect = "iso", pch = ".", cex = 2),
   labels = paste("mag", quakes$mag))

## Spin and brush for a 3D Lattice plot.
## Drag on the plot to rotate in 3D (can be confusing).
## Brushing is linked to the previous xyplot (if still open).
## Note, brushing 'cloud' requires a recent version of Lattice.
playwith(cloud(-depth ~ long * lat, quakes, zlab = "altitude"),
   new = TRUE, link.to = playDevCur(), click.mode = "Brush")

## Set brushed points according to a logical condition.
playSetIDs(value = which(quakes$mag >= 6))

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
	parameters = list(bandwidth = I(c(0.1, 1:50/50)),
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

## Looking through 100 time series and comparing to a reference;
## Use buttons to save the current series number or its mean value.
dat <- ts(matrix(cumsum(rnorm(100*100)), ncol = 100), start = 1900)
colnames(dat) <- paste("Series", 1:100)
ref <- (dat[,3] + dat[,4]) / 2
playwith(xyplot(cbind(dat[,i], ref = ref)),
  parameters = list(i = 1:100,
     print_i = function(playState) print(playState$env$i),
     print_mean = function(p) print(mean(dat[,p$env$i])),
     save_to_ii = function(playState)
       .GlobalEnv$ii <- playState$env$i,
     append_to_ii = function(playState) {
       if (!exists("ii")) ii <- c()
       .GlobalEnv$ii <- c(ii, playState$env$i)
     })
)

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
confirmClose <- function(playState) {
	if (gconfirm("Close window and report IDs?",
                     parent = playState$win)) {
		cat("Indices of identified data points:\n")
		print(playGetIDs(playState))
		return(FALSE) ## close
	} else TRUE ## don't close
}
xy <- data.frame(x = 1:20, y = rnorm(20),
                 row.names = letters[1:20])
playwith(xyplot(y ~ x, xy, main = "Select points, then close"),
        width = 4, height = 3.5, show.toolbars = FALSE,
        on.close = confirmClose, modal = TRUE,
        click.mode = "Brush")

## Ask user to save plot to PNG when window is closed:
saveOnClose <- function(playState) {
    playDevSet(playState)
    if (!gconfirm("Save plot to PNG file? (Cancel = no)")) return(FALSE)
    fname <- gfile("Save PNG file as:", type = "save")
    if (is.na(fname)) return(TRUE) ## cancel
    dev.off(dev.copy(Cairo_png, file = fname,
        width = dev.size()[1], height = dev.size()[2]))
    FALSE 
}
#playwith.options(on.close = saveOnClose)


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
## Note: if you rm(x_global) now, redraws will fail.
ls(playDevCur()$env)
## Next: store all data objects (in a new window):
doLocalStuff(title = "all stored", eval.args = TRUE, new = TRUE)
ls(playDevCur()$env)
## Now there are two devices open:
str(playDevList())
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
   data.points = NA, labels = NA)
playDevOff()
gc()
## or generate the trellis object first:
trel <- qqmath(~ bigobj, f.value = ppoints(500))
playwith(trel)
rm(trel)
## in this case, better to compute the sample first:
subobj <- quantile(foo, ppoints(500), na.rm = TRUE)
playwith(qqmath(~ subobj))
rm(subobj)
rm(bigobj)
}

## See demo(package = "playwith") for examples of new tools.
}
}
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ device }

