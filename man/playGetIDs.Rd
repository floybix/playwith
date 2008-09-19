\name{playGetIDs}
\alias{playGetIDs}
\title{Get IDs of identified points}
\description{
  Part of the \code{\link{playwith}} Application Programming Interface.
}
\usage{
playGetIDs(playState = playDevCur(),
           type = c("ids", "brushed"),
           labels = FALSE)
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the
    plot, window and device. }
  \item{type}{ subscripts of which points to return. }
  \item{labels}{ \code{TRUE} to return the labels as displayed;
    otherwise the subscripts into the original data object. }
}
\details{
  This plays a role similar to \code{\link{identify}}.
}
\value{
  an integer vector, or if \code{labels = TRUE} a character vector.
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playwith.API}} }
\examples{
if (interactive()) {

playwith(xyplot(1:100 ~ 1:100 | 1:2),
  labels = paste("label", 1:100, sep=""))
## identify some points... then
playGetIDs()
playGetIDs(labels = TRUE)

}
}
\keyword{ iplot }

