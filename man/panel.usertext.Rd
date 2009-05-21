\name{panel.usertext}
\alias{panel.usertext}
\alias{panel.brushpoints}
\alias{panel.brushlines}
\title{ Annotations with custom style }
\description{
  These are versions of \code{panel.text} and \code{panel.points} with
  different (customisable) style settings.
}
\usage{
panel.usertext(x, y = NULL, labels = seq_along(x), col,
               alpha, cex, srt = 0, lineheight, font,
               fontfamily, fontface, adj = c(0.5, 0.5),
               pos = NULL, offset = 0.5, ...)

panel.brushpoints(x, y = NULL, col, pch, alpha,
                  fill, cex, ...)

panel.brushlines(x, y = NULL, type, col, alpha,
                 lty, lwd, ...)
}
\arguments{
  \item{x, y}{ text or point locations. }
  \item{labels, col, alpha, cex, srt, lineheight}{ see the usual lattice
    functions in \code{\link[lattice]{llines}}. }
  \item{font, fontfamily, fontface, adj, pos, offset}{ as above. }
  \item{pch, fill, type, lty, lwd, ...}{ as above. }
}
\details{
  The settings for \code{panel.usertext} are taken from
  \code{trellis.par.get("user.text")}, but default to
  \code{trellis.par.get("add.text")} if undefined.
  \code{"user.text"} is preferred to \code{"add.text"} for annotations
  because the latter also applies to strip text and key text.

  The settings for \code{panel.brushpoints} are taken from
  \code{trellis.par.get("brush.symbol")}, but default to
  hard-coded settings if undefined.

  The settings for \code{panel.brushlines} are taken from
  \code{trellis.par.get("brush.line")}, but default to
  hard-coded settings if undefined.

  These functions are used in \code{\link{playwith}}.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link[lattice]{llines}}, \code{\link[lattice]{trellis.par.get}} }
\examples{
xyplot(1:10 ~ 1:10, panel = function(...) {
    panel.brushpoints(...)
    panel.usertext(..., pos = 1)
})
}
\keyword{ aplot }
