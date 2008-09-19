\name{convertFromDevicePixels}
\alias{convertFromDevicePixels}
\alias{convertToDevicePixels}
\alias{inViewport}
\alias{grobBBDevicePixels}
\alias{showGrobsBB}
\title{ Utilities for working with grobs and viewports in device coordinates. }
\description{
  Utilities for working with grobs and viewports in device coordinates. }
}
\usage{
convertFromDevicePixels(x.px, y.px, unitTo = "native", valueOnly = FALSE)
convertToDevicePixels(x, y)

inViewport(x.px, y.px, viewport)
grobBBDevicePixels(grob, viewport)

showGrobsBB(draw = TRUE,
    gp.box = gpar(col = "yellow", lwd = 5, alpha = 0.2),
    gp.text = gpar(cex = 0.75, alpha = 0.5))
}
\arguments{
  \item{x.px, y.px}{ locations in device coordinates
    (i.e. pixels, with origin at top-left corner of device).
    May be vectors. }
  \item{unitTo}{ the \code{\link[grid]{unit}} to convert to. }
  \item{valueOnly}{ to return values as numeric (native) rather than
    \code{unit}s. }
  \item{x, y}{ locations in the current viewport (numeric native or
    \code{unit}s). May be vectors. }
  \item{viewport}{ a viewport name or \code{\link[grid]{vpPath}}. }
  \item{grob}{ a \code{\link[grid]{grob}}. }
  \item{draw}{ whether to draw bounding boxes and grob names. }
  \item{gp.box}{ graphical parameters for bounding boxes. }
  \item{gp.text}{ graphical parameters for grob name text. }
}
\details{
  Not yet...
}
\value{
\code{convertFromDevicePixels} returns a list with \code{x, y} (units or
numeric) locations in the current viewport.

\code{convertToDevicePixels} returns a list with \code{x, y} (numeric)
locations in pixels from the top-left corner of the device.

\code{inViewport} returns a logical: whether the given pixel location is
inside the given viewport.

\code{grobBBDevicePixels} returns a list with \code{x, y} (numeric)
locations in pixels giving the bounding box of the given grob. The grob
must exist in the given viewport.

\code{showGrobsBB} returns a \code{data.frame} giving information about all
grobs in the current scene.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{
  \code{\link[grid]{grid.convert}},
  \code{\link[grid]{grobX}},
  \code{\link[grid]{grid.ls}},
  \code{\link[grid]{grid.show.layout}} }
\examples{
print(xyplot(1:10 ~ 1:10 | c("a", "b")))

vpname <- trellis.vpname("panel", 1, 1)
downViewport(vpname)

convertToDevicePixels(x = 5:10, y = 5:10)
convertToDevicePixels(unit(0, "npc"), unit(0, "npc"))
convertFromDevicePixels(x = 100, y = 100)

inViewport(x = 100, y = 100, vpname)
inViewport(x = c(0, 100), y = c(0, 100), vpname)

myGrob <- grid.circle(r = 0.3, name = "myCircle")
grobBBDevicePixels(myGrob, vpname)

str(showGrobsBB(draw = TRUE))
}
\keyword{ dplot }
\keyword{ iplot }
