\name{playGetIDs}
\alias{playGetIDs}
\alias{playSetIDs}
\alias{playClear}
\title{Get or set IDs of selected points}
\description{
  Part of the \pkg{playwith} Application Programming Interface.
}
\usage{
playGetIDs(playState = playDevCur(),
           type = c("ids", "brushed"),
           labels = FALSE)

playSetIDs(playState = playDevCur(), value,
           type = "brushed", space = "plot", redraw = NA)

playClear(playState = playDevCur(),
          type = c("annotations", "ids", "brushed"),
          redraw = TRUE)
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the
    plot, window and device. }
  \item{type}{ which type of points to get or set subscripts for. }
  \item{labels}{ \code{TRUE} to return the labels as displayed;
    otherwise the subscripts into the original data object. }
  \item{value}{ specifies the set of points to be selected. Can be an
    integer vector of subscripts, or a logical vector (not
    recycled). }
  \item{space}{ the space to draw labels in if \code{type = "ids"}. }
  \item{redraw}{ whether to redraw the plot. The default \code{NA} only
    redraws if necessary (i.e. if an existing selection was
    replaced). If \code{FALSE} nothing is drawn. }
}
\details{
  Not yet.
}
\value{
  \code{playGetIDs} returns an integer vector, or if \code{labels =
           TRUE} a character vector.
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \link{playwith.API} }
\examples{
if (interactive()) {

playwith(xyplot(1:100 ~ 1:100 | 1:2, subscripts = TRUE),
    labels = paste("label", 1:100, sep=""))
playSetIDs(value = c(50:60, 100))
playGetIDs()
playSetIDs(value = c(10, 20, 30), type = "ids",
    space = "packet 1")
playGetIDs(labels = TRUE)
playClear()
playGetIDs()

}
}
\keyword{ iplot }

