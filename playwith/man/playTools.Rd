\name{playTools}
\alias{playTools}
\alias{toolConstructors}
\alias{playApplicationTools}
\alias{playInteractionTools}
\alias{play3DTools}
\alias{playSplomTools}
\title{Built-in tools for playwith}
\description{
  This document describes the built-in tools that can be placed in 
  a \code{\link{playwith}} window. The constructor functions themselves
  can be found in the named list \code{toolConstructors}.
}
\details{
	Tools in the set \code{playApplicationTools}, by default on the top toolbar:
	\describe{
		\item{\code{options}}{
			set interface options, such as toolbar style and annotation mode.
		}
		\item{\code{stayontop}}{
			make the window stay above others, so it is always visible.
		}
		\item{\code{keep}}{
			keep this plot window, do not replace it with the next \code{\link{playwith}} plot.
		}
		\item{\code{save}}{
			save current plot to a file. For Lattice plots, if you intend to print,
			you might want to change the theme to one suitable for black and white printing first.
		}
		\item{\code{copy}}{
			copy current plot (to the clipboard).
		}
		\item{\code{print}}{
			print current plot. For Lattice plots you might want to change
			the theme to one suitable for black and white printing first.
		}
		\item{\code{data}}{
			opens a browser to show data objects holding the plot data.
			Double-click an item to edit it.
		}
		\item{\code{settings}}{
			show a dialog box to control the plot type and settings.
			(This will only work for basic plot types).
		}
		\item{\code{theme}}{
			choose the Lattice theme.
		}
		\item{\code{time.mode}}{
			switch to time mode, allowing navigation along the x axis
			or through a speciefied time sequence.
		}
	}
	
	Tools in the set \code{playInteractionTools}, by default on the left toolbar:
	\describe{
		\item{\code{expand}}{
			show only one selected lattice panel.
		}
		\item{\code{identify}}{
			identify points by adding labels to them.
		}
		\item{\code{annotate}}{
			draw text on the plot or page, with various options.
		}
		\item{\code{arrow}}{
			draw an arrow on the plot or page.
		}
		\item{\code{edit.annotations}}{
			edit the annotation text and arrows in R code.
		}
		\item{\code{clear}}{
			remove identifying labels, annotations and/or brushing.
		}
		\item{\code{zoom}}{
			select a new plot region by dragging with the mouse.
		}
		\item{\code{zoomout}}{
			show 4x the plot area.
		}
		\item{\code{zoomfit}}{
			show all data (default scales).
		}
		\item{\code{zero}}{
			show the full scale(s) down to zero.
		}
		\item{\code{coords}}{
			this widget shows plot coordinates when you click on the plot.
		}
	}
	
	Tools in the set \code{play3DTools}:
	\describe{
		\item{\code{zoomin.3d}}{
			zoom in.
		}
		\item{\code{zoomout.3d}}{
			zoom out.
		}
		\item{\code{fly.left.3d}}{
			fly left.
		}
		\item{\code{fly.right.3d}}{
			fly right.
		}
	}
	
	Tools in the set \code{playSplomTools}:
	\describe{
		\item{\code{brush}}{
			TODO.
		}
	}

}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{playwith}}, \link{playwith.API} }
\examples{
	
}
\keyword{ iplot }
\keyword{ dynamic }


