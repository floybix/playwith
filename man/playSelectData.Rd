\name{playSelectData}
\alias{playSelectData}
\title{Let playwith user select data points}
\description{
  Part of the \code{\link{playwith}} Application Programming Interface.
}
\usage{
playSelectData(playState = playDevCur(),
    prompt = paste("Click or drag to select data points;",
                   "Right-click or Esc to cancel."),
    foo = playRectInput(playState, prompt = prompt))
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the plot, window and device. }
  \item{prompt}{ text to display in the prompt. }
  \item{foo}{ rectangular plot region structure, usually generated
    interactively. }
}
\details{
	\code{playSelectData} is similar to \code{\link{identify}}.
	The user is prompted to click or drag to select data points.
	If a click, the nearest point is selected, if it is within 11 points.
	If it is a drag, all points within the rectangular region are selected.
	Note that data points can be selected from one panel of a multi-panel
	lattice plot without specifying the panel beforehand.
}
\value{
	\code{playSelectData} returns \code{NULL} if the user cancelled (e.g. by right-clicking).
	Otherwise a list with:
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
		\item{as well as all the other elements returned by \code{\link{playRectInput}}.
	}
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playRectInput}}, \code{\link{playwith.API}} }
\examples{
if (interactive()) {

library(lattice)
playwith(xyplot(Sepal.Width ~ Petal.Width | Species, data = iris))
playSelectData(playDevCur())

}
}
\keyword{ iplot }

