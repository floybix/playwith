\name{playState}
\alias{playState}
\alias{print.playState}
\title{Object representing the plot, window and device.}
\description{
  The \code{playState} object is created by \code{\link{playwith}} to
  represent the state of the plot, window and device. It is central to
  the \link{playwith.API}.
}
\details{
  A \code{playState} object is an \code{\link{environment}}
  (of class \code{"playState"}) containing many other objects,
  including:
  \describe{
    \item{\code{win}}{
      the plot window (\code{\link[RGtk2]{gtkWindow}}).
    }
    \item{\code{dev}}{
      the plot device, as returned by \code{dev.cur}.
    }
    \item{\code{call}}{
      the current plot call.
    }
    \item{\code{env}}{
      local environment used to store plot data objects.
    }
    \item{\code{accepts.arguments}}{
      whether the current main plot function accepts arguments.
    }
    \item{\code{callName}}{
      name of the current main plot function.
    }
    \item{\code{is.lattice}, \code{is.ggplot}, \code{is.vcd}, \code{is.base}}{
      whether the current plot is a Lattice / ggplot / base graphics
      plot. \code{is.base} is defined as TRUE if neither of the others is
      TRUE and \code{viewport} is undefined.
    }
    \item{\code{result, trellis}}{
      \code{result} is the result of evaluating the plot call.
      If \code{is.lattice}, the trellis object is also stored in
      \code{trellis}.
    }
    \item{\code{viewport}}{
      a named list of viewport paths (or names).
      One of these must be named "plot".
      \code{NULL} if the plot is a base graphics or Lattice plot.
    }
    \item{\code{spaces}}{
      a character vector listing all \dfn{spaces} defined in the current plot, not
      including \code{"page"}.
    }
    \item{\code{labels}}{
      labels for data points, either given or guessed.
    }
    \item{\code{data.points}}{
      given data points or \code{NULL}.
    }
    \item{\code{ids}}{
      a named list of currently labelled data points.
      Each name corresponds to a "space", which can be "page"
      (positioned on page) or "plot" (positioned in plot coordinates).
      With Lattice graphics the space can be "packet 1" etc.
      Each list element is a data frame of numeric
      \code{subscripts} and \code{pos} (for label placement).
    }
    \item{\code{annotations}}{
      a named list of calls to be evaluated in a target viewport:
      each name corresponds to a "space", as in \code{ids}.
    }
    \item{\code{linked}}{
      an environment, containing a list "ids" and a
      list "subscribers". Elements of the former are
      subscripts of brushed data points. Elements of
      the latter are the \code{playState} objects of all
      linked plots in this group. This \code{linked}
      object is shared by all subscribers.
    }
    \item{\code{tools}}{
      hmm...
    }
    \item{\code{uiManager}, \code{actionGroups}}{
      the \code{\link[RGtk2]{GtkUIManager}} and a named list of its
      action groups.
    }
    \item{\code{widgets}}{
      A list of the GUI widgets. The most useful one
      is \code{drawingArea} (the plot device widget).
    }
    \item{\code{tmp}}{
      a list of temporary objects, including:
      \describe{
	\item{\code{plot.ready}}{
	  whether the plot has been drawn and is ready for interaction.
	}
	\item{\code{identify.ok}}{
	  whether data points and labels are defined (or a plausible
	  guess could be made), allowing points to be identified.
	}
      }
    }
  }

  There are several other standard objects which can be inspected with
  \code{ls.str(playDevCur())}.

  Other objects can be passed in through the \code{...} argument to
  \code{\link{playwith}}, or defined by tools.

}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{playwith}}, \link{playwith.API} }

\keyword{ programming }

