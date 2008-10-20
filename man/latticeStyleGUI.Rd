\name{latticeStyleGUI}
\alias{latticeStyleGUI}
\alias{latticeStyleToBasePar}
\title{ A plot style settings GUI }
\description{
  A graphical user interface to edit the Lattice theme for a given device.
  It can also set some simple base graphics parameters.
}
\usage{
latticeStyleGUI(width = 480, height = 480, pointsize = 12,
                target.device = dev.cur(), base.graphics = FALSE)

latticeStyleToBasePar()
}
\arguments{
  \item{width, height}{ initial size in pixels of the embedded graphic device. }
  \item{pointsize}{ pointsize for the embedded graphic device. }
  \item{target.device}{ the device for which to edit style settings;
  defaults to the current device. }
  \item{base.graphics}{ whether to apply settings to the base graphics
    parameters (\code{\link{par}}) as well as the lattice graphics
    parameters. This is quite limited compared to the lattice system:
    not all controls in the interface will have an effect. Also, base
    graphics plots may or may not use the \code{par()} settings.}
}
\details{
  \code{latticeStyleGUI()} is an interface to
  \code{\link[lattice]{trellis.par.get}} / \code{trellis.par.set}.
  It is not a comprehensive interface: some simplifications and
  omissions have been made from the full list of settings.

  The graphical user interface is built on the \pkg{gWidgets} package.
  As such it can run under different GUI toolkits
  such as \pkg{gWidgetsRGtk2}, \pkg{gWidgetstcltk}, etc.

  Changes take effect immediately. Load a new theme to reset.
  The embedded graphic device (or non-embedded, depending
  on the GUI toolkit) will show a preview of your settings. You can also
  plot on the target device while keeping the GUI open, for testing.

  The GUI may be very slow to initialise. It is recommended to update to
  the latest version of the \pkg{gWidgets} package (and its toolkit
  implementations), as these include significant speed-ups.

  \code{latticeStyleToBasePar()} attempts to apply the current
  lattice style settings to the base graphics system
  (\code{\link{par}} and \code{\link{palette}}). Only basic settings are
  used. See the function definition for details.
}
\value{
  \code{latticeStyleGUI} does not return anything, but does change
  graphical settings for the given device. It also creates an
  object \code{trellis.par.theme} in the global workspace containing the
  Lattice \emph{theme} (i.e. list of settings). Another object
  \code{trellis.par.log} holds a subset of that: just the
  changes made from a built-in theme.

  \code{latticeStyleToBasePar} returns the previous \code{par}
  settings.
}
\references{
  For an excellent introduction to and coverage of Lattice:

Sarkar, Deepayan (2008) "Lattice: Multivariate Data Visualization with
R", Springer. \url{http://lmdvr.r-forge.r-project.org/}
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\note{
  Plot calls can over-ride these settings, so there is no guarantee that the
  settings will be visible on a given plot.
}
\seealso{ \code{\link[lattice]{trellis.par.get}}, \code{\link{par}},
  \code{\link{latticeStyleDemo}} }
\examples{
if (interactive())
{
    latticeStyleGUI()
}

## a base graphics plot (from example(matplot))
sines <- outer(1:20, 1:4, function(x, y) sin(x / 20 * pi * y))
matplot(sines, pch = 1:4, type = "o")

## apply Lattice settings and re-plot
opar <- latticeStyleToBasePar()
matplot(sines, pch = 1:4, type = "o")

## apply a different Lattice theme
trellis.par.set(custom.theme.black())
latticeStyleToBasePar()
plot(Ozone ~ Wind, data = airquality, col = 1)

## reset:
par(opar)
palette("default")
}
\keyword{ color }
