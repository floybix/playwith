\name{playSelectData}
\alias{playSelectData}
\alias{playPointInput}
\alias{playLineInput}
\alias{playRectInput}
\title{Get input from the playwith user}
\description{
  Part of the \code{\link{playwith}} Application Programming Interface.
}
\usage{
playSelectData(playState, prompt = "Click or drag to select data points.")
playPointInput(playState, prompt = "Click on the plot")
playLineInput(playState, prompt = "Click and drag to define a line")
playRectInput(playState, prompt = "Click and drag to define a rectangular region",
	scales = c("x", "y"))
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the plot, window and device. }
  \item{prompt}{ text to display in the prompt. }
  \item{scales}{ one or more of \code{"x"} and \code{"y"}, 
	  constraining the dimensions of the region to select.
	  E.g. if \code{scales="x"} then a section along the x-axis is selected,
	  as might be appropriate for a time series.
  }
}
\details{
	\code{playSelectData} is similar to \code{\link{identify}}.
	The user is prompted to click or drag to select data points.
	If a click, the nearest point is selected, if it is within 18 points.
	If it is a drag, all points within the rectangular region are selected.
	Note that data points can be selected from one panel of a multi-panel 
	lattice plot without specifying the panel beforehand.
	
	\code{playPointInput} is similar to \code{\link{locator}}, but
	returns native coordinates in whichever plot space was clicked on.
	Device coordinates and normalised device coordinates are also available.
	
	\code{playRectInput} and \code{playLineInput} allow the user to
	interactively draw a rectangle or line.
}
\value{
	All these functions return \code{NULL} if the user cancelled (e.g. by right-clicking).
	
	\code{playPointInput}, \code{playRectInput} and \code{playLineInput} 
	return a list with:
	\describe{
		\item{\code{space}}{
			character, specifies the plot space in which the user clicked or dragged.
			See the \code{space} argument to \code{\link{playDo}}.
		}
		\item{\code{coords}}{
			native coordinates of the point or shape in \code{space}.
			A list with numeric vectors \code{x} and \code{y}.
			For a rectangle or line, these are length 2 where the first element
			refers to the start of the drag motion. For a point these are length 1.
			If \code{space} is \code{"page"}, this is \code{NULL}.
		}
		\item{\code{dc}}{
			device coordinates of the point or shape (pixels).
		}
		\item{\code{ndc}}{
			normalised device coordinates of the point or shape.
		}
		\item{\code{is.click}}{
			logical, whether it was a click 
			(so probably should not be treated as a rectangle or line).
			This is not returned by \code{playPointInput}.
		}
		\item{\code{modifiers}}{
			a \code{flag} representing which modifier keys were
			pressed during the click (or at the end of a drag).
			E.g. to test for Shift key: 
			\code{if (foo$modifiers & GdkModifierType["shift-mask"])}.
			See \code{\link[RGtk2]{enums-and-flags}}.
		}
	}
	
	\code{playSelectData} returns a list with:
	\describe{
		\item{\code{space}}{
			character, specifies the plot space in which data points were selected.
			See the \code{space} argument to \code{\link{playDo}}.
		}
		\item{\code{which}}{
			indices of the data points selected. This relies on the data being guessed
			correctly from the plot call, unless \code{data.points} was specified explicitly.
			Note that for lattice plots, these are indices into the packet/panel data,
			not indices into the original data set, so not the same as \code{subscripts}.
		}
		\item{\code{x, y}}{
			native coordinates of the selected data points.
		}
		\item{\code{is.click}}{
			logical, whether it was a click (otherwise a drag).
		}
		\item{\code{pos}}{
			position of click relative to the closest point, 
			as in the \code{pos} argument to \code{\link{text}}.
			If \code{is.click} is false, this is \code{NULL}.
		}
		\item{as well as all the other elements listed above.}
	}
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playwith.API}} }
\examples{
if (interactive()) {
	
	library(lattice)
	playwith(xyplot(Sepal.Width ~ Petal.Width | Species, data = iris))
	playPointInput(playDevCur())
	playRectInput(playDevCur())
	playSelectData(playDevCur())
	
}
}
\keyword{ programming }

