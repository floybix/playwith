\name{latticeStyleDemo}
\alias{latticeStyleDemo}
\title{ Demonstrate the current Lattice style settings }
\description{
  Plots incorporating all the major elements of Lattice style settings.
}
\usage{
latticeStyleDemo(type = c("plot", "superpose", "polygons", "regions"))
}
\arguments{
  \item{type}{ which of the 4 plot types to display. If more than one
    are given they are shown in panels. The default is to show all 4. }
}
\details{
  Note, this uses \code{"user.text"}, which is not a standard element of
  Lattice style. However, this falls back to \code{"add.text"} -- the
  default used by \code{panel.text()} -- if not defined.

  \code{"user.text"} is preferred to \code{"add.text"} for annotations
  because the latter also applies to strip text and key text.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link[lattice]{show.settings}}, \code{\link{panel.usertext}} }
\examples{
latticeStyleDemo()
latticeStyleDemo("plot")
latticeStyleDemo(c("superpose", "polygons"))
}
\keyword{ color }
