\name{playDo}
\alias{playDo}
\title{Do something in a specified plot space}
\description{
  Part of the \code{\link{playwith}} Application Programming Interface.
}
\usage{
playDo(playState, expr, space = "plot",
       clip.off = !isTRUE(playState$clip.annotations),
       return.code = FALSE)
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the plot, window and device. }
  \item{expr}{ an expression, typically a drawing operation, to evaluate in the plot space.
	  It will be quoted (see examples). }
  \item{space}{ the plot space (viewport) to go to before evaluating \code{expr}.
  	This can be "plot", "page", or for lattice plots "packet N" (where N is a packet number),
  	or for grid plots the name of a viewport passed to the \code{viewport} argument of \code{\link{playwith}}.}
  \item{clip.off}{ enforce no clipping of drawing operations:
    see \code{clip} argument to \code{\link[grid]{viewport}}. }
  \item{return.code}{ if \code{TRUE}, return code (as an expression) for
    the given actions, rather than executing them. }
}
\details{
  This function allows an arbitrary expression to be evaluated while some part of the plot has been made active
  (i.e. moving to a grid graphics viewport). Grid viewports are used also to represent spaces in a base graphics
  plot (using the \code{gridBase} package. That means \code{expr} can always use Grid drawing operations.

  The default \code{space="plot"} will work for base graphics, grid graphics and for single-panel lattice plots.
  It will also work for multi-panel lattice plots when one panel is in focus (see \code{\link[lattice]{trellis.focus}}).

  Using \code{space="page"} will apply to the whole device space in
  normalised device coordinates (0--1).

  Other functions such as \code{\link{playSelectData}} and \code{\link{playPointInput}}
  return values that can be used directly for the \code{space} argument.
}
\value{
	the value returned by \code{expr}.
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{
  \code{\link{playwith.API}},
  \code{\link{playSelectData}},
  \code{\link{playPointInput}}
}
\examples{
if (interactive()) {

library(lattice)
library(grid)

packs <- paste("packet", rep(1:4, each=4))
playwith(xyplot(1:16 ~ 1:16 | packs))
myGp <- gpar(fill="red", col="black", alpha=0.5)

## draw in a specific packet
playDo(playDevCur(), grid.circle(gp=myGp), space="packet 2")

## draw in default space="plot" after focussing on one panel
trellis.focus("panel", 1, 1)
packet.number()  # 1, same as space="packet 1"
playDo(playDevCur(), grid.circle(gp=myGp))
trellis.unfocus()

## space="plot" does not work in a multi-panel plot
## unless one panel is in focus
try(playDo(playDevCur(), grid.circle(gp=myGp)))

## draw on the whole page
playDo(playDevCur(), grid.circle(gp=myGp), space="page")

}
}
\keyword{ programming }

