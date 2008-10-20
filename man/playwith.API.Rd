\name{playwith.API}
\alias{playwith.API}
\alias{playDevCur}
\alias{playDevSet}
\alias{playDevList}
\alias{playDevOff}
\alias{playReplot}
\alias{playNewPlot}
\alias{playSourceCode}
\alias{playUndo}
\alias{playUnlink}
\alias{playAnnotate}
\alias{playPrompt}
\alias{playFreezeGUI}
\alias{playThawGUI}
\title{The playwith API}
\description{
  The \pkg{playwith} Application Programming Interface.
}
\details{
  \code{\link{playwith}} plots (incorporating a plot, window and device)
  are represented by a \code{\link{playState}} object.

	Following is a table of the API functions that can be used to
	work with the plot, and to write new interactive tools.
	See the links to specific help pages for details.
	In case these are insufficient, you may work with the
	\code{\link{playState}} object itself.

	\section{Device management}{
	  These are similar to \code{\link{dev.set}} etc.
	  \describe{
	    \item{\code{playDevCur()}}{
	      returns the current or last
	      active \code{playState} -- this is not necessarily the active
	      graphics device.
	    }
	    \item{\code{playDevList()}}{
	      lists all open \code{playState}s.
	    }
	    \item{\code{playDevSet(playState)}}{
	      sets the current \code{playState}, and sets the active
	      graphics device.
	    }
	    \item{\code{playDevOff(playState)}}{
	      closes the device and window (\code{dev.off()} also
	      works).
	    }
	  }
	}

	\section{Common user commands}{
	  These functions are also available as menu items.
	  \describe{
	    \item{\code{\link{playGetIDs}(playState, type, labels)}}{
	      returns indices (or labels) of currently brushed or labelled
	      data points.
	    }
	    \item{\code{\link{playSetIDs}(playState, value, type, space,
		add, redraw, pos)}}{
	      sets which data points are brushed or
	      labelled. \code{type} defaults to \code{"brushed"};
	      \code{space} is ignored unless \code{type = "labelled"}.
	    }
	    \item{\code{\link{playClear}(playState, type, redraw)}}{
	      remove one or more of the \code{type}s
	      \code{"annotations"}, \code{"labelled"}, \code{"brushed"}. The
	      latter will also apply to any linked plots. If
	      \code{redraw = FALSE} the display will not be updated.
	    }
	    \item{\code{playUndo(playState)}}{
	      reverts the last change any annotations (including the set
	      of labelled and brushed data points).
	    }
	    \item{\code{playUnlink(playState)}}{ removes links from the
	      given plot to any other plots (for linked brushing).
	    }
	    \item{\code{playSourceCode(playState)}}{ returns (deparsed)
	      R code to reproduce the current plot display.
	    }
	  }
	}

	\section{Interaction}{
	  These functions allow the user to click or drag on the plot.
	  Click or drag locations are converted into the native
	  coordinates of whatever plot \dfn{space} they occurred in (but
	  are available as device coordinates too).
	  \describe{
	    \item{\code{\link{playSelectData}(playState, prompt, scales,
		foo)}}{
	      interactively select data from the plot (one point or a
	      whole region). }
	    \item{\code{\link{playPointInput}(playState, prompt)}}{ prompt for
	      a click on the plot. Similar to \code{\link{locator}}. }
	    \item{\code{\link{playLineInput}(playState, prompt,
		scales)}}{
	      prompt to drag a line. }
	    \item{\code{\link{playRectInput}(playState, prompt,
		scales)}}{ prompt to drag a rectangular region. }
	    \item{\code{playPrompt(playState, text)}}{ sets the
	      statusbar text. Pass \code{NULL} to reset. The GUI is
	      frozen when the prompt is set and unfrozen when reset. }
	    \item{\code{playFreezeGUI(playState)},
	      \code{playThawGUI(playState)}}{
	      disables or re-enables the GUI. }
	  }
	}

	\section{Working with the display}{
	  \describe{
	    \item{\code{\link{playDo}(playState, expr, space, clip.off,
		return.code)}}{ evalutes the given expression in the
	      given plot \dfn{space}, i.e. after moving to the
	      corresponding grid viewport.
	    }
	    \item{\code{playAnnotate(playState, annot, space)}}{
	      adds the annotation (a call or expression to draw on the
	      plot) to the list of persistent annotations, and draws
	      them. These will always be drawn after plotting (unless they
	      are removed by the user).
	    }
	    \item{\code{\link{rawXLim}(playState, space)}}{
	      gets or sets the x axis limits, in native coordinates of
	      the given \dfn{space} (viewport).
	    }
	    \item{\code{rawYLim(playState, space)}}{
	      same as \code{rawXLim}, for y axis limits.
	    }
	    \item{\code{\link{spaceCoordsToDataCoords}(playState, xy)}}{
	      converts raw space (viewport) coordinates to the data
	      scale by applying an inverse log transformation if
	      necessary. If there are no log scales this does nothing.
	    }
	    \item{\code{dataCoordsToSpaceCoords(playState, xy)}}{
	      converts data coordinates to raw space (viewport)
	      coordinates by applying a log transformation if
	      necessary. If there are no log scales this does nothing.
	    }
	  }
	}

	\section{Working with the call}{
	  \describe{
	    \item{\code{\link{callArg}(playState, arg, eval, data)}}{
	      gets or sets arguments to the main plot call.
	    }
	    \item{\code{\link{mainCall}(playState)}}{ gets or replaces
	      the main plot call (which is not necessarily the same as
	      the top-level call, \code{playState$call}).
	    }
	    \item{\code{\link{updateMainCall}(playState)}}{
	      locates the main plot call within the top-level call -- by
	      matching against the given \code{main.function}, or by
	      guessing. This allows \code{callArg()} to work correctly;
	      it should be called if \code{playState$call} is replaced.
	    }
	    \item{\code{playReplot(playState)}}{
	      redraws the plot by evaluating the plot call, and runs
	      \dfn{update actions}.
	      This should
	      be called after changing plot arguments (or annotations if
	      that requires a redraw). However, if the data has changed
	      or the type of plot (e.g. the high-level plot function)
	      has changed, \code{playNewPlot} should be used instead.
	      \code{playReplot} is triggered when zooming,
	      editing plot settings, removing annotations, etc.
	    }
	    \item{\code{playNewPlot(playState)}}{
	      redraws the plot by evaluating the plot call,
	      updates the main call, runs
	      \dfn{init actions} (such as detecting whether zooming,
	      identifying or brushing data points is possible),
	      as well as running \dfn{update actions} as with
	      \code{playReplot}.
	      Note: \code{playNewPlot} is triggered when the call text
	      is edited manually in the GUI.
	    }
	  }
	}

	\section{Working with data}{
	  \describe{
	    \item{\code{\link{xyData}(playState, space)}}{
	      attempts to extract plot data in terms of x and y
	      locations, but in the original data form (such as factor,
	      date or time). This uses the generic function
	      \code{\link{plotCoords}} to generate plot locations;
	      new methods can be defined for non-standard plot types.
	    }
	    \item{\code{\link{xyCoords}(playState, space)}}{
	      same as \code{xyData}, converted to numeric coordinates.
	    }
	    \item{\code{\link{getDataArg}(playState, eval)}}{
	      attempts to extract the underlying data set (typically a
	      data.frame), which may contain more variables than those
	      currently plotted. This may come from a \code{data}
	      argument to the plot call, or from a \code{with()} block.
	    }
	  }
	}

}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{
  \code{\link{playwith}},
  \code{\link{playState}},
  \code{\link{convertFromDevicePixels}}
}
\examples{
if (interactive()) {

demo(package = "playwith")

}
}
\keyword{ programming }
\keyword{ utilities }
