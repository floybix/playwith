\name{playState}
\alias{playState}
\title{Object representing the plot, window and device.}
\description{
  This object is used by \code{\link{playwith}} to 
  represent 
  The playState object is central to the \link{playwith.API}.
  However, it may also be seen by users controlling the "device" with
  \code{\link{playDevCur}}, \code{\link{playDevList}} etc.
}
\details{
A playState object is actually an \code{\link{environment}} containing many other objects.
It will always contain:
	\describe{
		\item{\code{win}}{
			
		}
		\item{\code{dev}}{
			
		}
		\item{\code{call}}{
			
		}
		\item{\code{env}}{
			local environment used to store evaluated plot call arguments.
		}
		\item{\code{is.lattice}}{
		}
		\item{\code{viewport}}{
		}
		\item{\code{labels}}{
		}
		\item{\code{data.points}}{
		}
		\item{\code{time.mode}}{
		}
		\item{\code{ids}}{
		}
		\item{\code{annotations}}{
		}
		\item{\code{brushed}}{
		}
		\item{\code{tools}}{
		}
		\item{\code{widgets}}{
drawingArea
topToolbar 
leftToolbar
bottomToolbar
rightToolbar
callToolbar
callEntry
undoButton
redoButton
pageEntry
pageScrollbar
pageScrollBox
timeEntry
timeScrollbar
timeScrollBox
promptBox
promptLabel
vbox
hbox
		}
		\item{\code{plot.ready}}{
			
		}
	}
	
	trellis
	
	page
	pages
	time.index
	current.index
	on.close
	.args
	
	deviceToSpace
	baseViewports
	
	others passed in through the \code{...} argument to \code{\link{playwith}}
	or defined by tools.

}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{playwith}}, \link{playwith.API} }

\keyword{ programming }

