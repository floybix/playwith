\name{playPointInput}
\alias{playPointInput}
\alias{playLineInput}
\alias{playRectInput}
\title{Get point, line or rect input from playwith user}
\description{
  Part of the \pkg{playwith} Application Programming Interface.
}
\usage{
playPointInput(playState = playDevCur(), prompt)
playLineInput(playState = playDevCur(), prompt, scales = "dynamic")
playRectInput(playState = playDevCur(), prompt, scales = "dynamic")
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the plot, window and device. }
  \item{prompt}{ text to display in the prompt. }
  \item{scales}{ the default allows the user to hold Shift while
    dragging, to constrain the selection to x or y scales. Otherwise
    this should be one or more of "x" and "y", specifying which axes to
    select along. }
}
\details{
  \code{playPointInput} is similar to \code{\link{locator}}, but
  returns native coordinates in whichever plot space was clicked on.
  Device coordinates and normalised device coordinates are also available.

  \code{playRectInput} and \code{playLineInput} allow the user to
  interactively draw a rectangle or line.
}
\value{

  All these functions return \code{NULL} if the user cancelled (e.g. by
  right-clicking). Otherwise a list with:

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
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playSelectData}}, \link{playwith.API} }
\examples{
if (interactive()) {

library(lattice)
playwith(xyplot(Sepal.Width ~ Petal.Width | Species, data = iris))
playPointInput()
playRectInput()

}
}
\keyword{ programming }

