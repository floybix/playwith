\name{identifyGrob}
\Rdversion{1.1}
\alias{identifyGrob}
\title{
  return names of clicked grid-objects.
}
\description{
  This identifies which \code{\link{grob}}s (grid objects) were clicked
  on (or otherwise identified by pixel coordinates), and returns their
  names. The names can be used by other grid functions, like
  \code{\link{grid.edit}} or \code{\link{grid.remove}}.
}
\usage{
identifyGrob(xy.pixels = grid.locator(), classes = NULL)
}
\arguments{
  \item{xy.pixels}{
    if given, a list with components x and y, being pixel coordinates in
    the current plot device.
}
  \item{classes}{
    if given, one or more class names of grobs. This can be used to
    filter the results to only specified types of objects.
    Examples: "text", "lines", "segments", "rect".
}
}
\details{
None yet.
}
\value{
  a character vector of grob names, or NULL.
}
\author{
  Felix Andrews \email{felix@nfrac.org}
}
\note{
  This will give an error if the current plot has no grobs, as returned by \code{\link{grid.ls}}.
}
\seealso{
  \code{\link{showGrobsBB}}
}
\examples{
if (interactive()) {

xyplot(1:10 ~ 1:10)
identifyGrob()
str(grid.get(identifyGrob()[1]))
grid.remove(identifyGrob(class = "points")[1])

}
}
\keyword{ iplot }
