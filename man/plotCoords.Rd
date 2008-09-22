\name{plotCoords}
\alias{plotCoords}
\alias{plotCoords.default}
\alias{plotCoords.qqnorm}
\alias{plotCoords.qqplot}
\alias{plotCoords.qqmath}
\alias{plotCoords.cloud}
\alias{labels.ts}
\alias{labels.zoo}
\title{ API for defining data coordinates of a plot }
\description{
  Given a call to a plot function, return the data coordinates.
}
\usage{
plotCoords(name, call, envir, ...)

\method{plotCoords}{default}(name, call, envir, panel.args, data, ...)

\method{plotCoords}{qqnorm}(name, call, envir, ...)
\method{plotCoords}{qqplot}(name, call, envir, ...)

\method{plotCoords}{qqmath}(name, call, envir, panel.args, ...)

\method{plotCoords}{cloud}(name, call, envir, panel.args, ...)
}
\arguments{
  \item{name}{ The class of this object is the name of the plot
    function. Hence methods can be defined for different plot functions. }
  \item{call}{ the plot call. }
  \item{envir}{ environment containing objects referenced by the
    call. Call arguments should be evaluated in this environment. }
  \item{panel.args}{ passed to Lattice plots only: panel arguments for the
    relevant panel. }
  \item{data}{ passed to non-Lattice plots only: a "data" argument, or NULL,
  to be used in evaluating call arguments (in addition to \code{envir}). }
  \item{\dots}{ ignored. }
}
\details{
  None yet...
}
\value{
  a list with components:
  \item{x, y}{ data point coordinates
    (in native panel / user coordinates). }
  \item{subscripts}{ (optional) data point subscripts. }
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{xyData}}, \code{\link{xy.coords}} }
\examples{
## Note, these are not designed to be called directly;
## they are used internally in playwith().
## But for demonstration purposes:
pargs <- trellis.panelArgs(qqmath(rnorm(20)), packet = 1)
plotCoords(structure("qqmath", class = "qqmath"),
           call = quote(qqmath(rnorm(20))), envir = new.env(),
           panel.args = pargs)
}
\keyword{ dplot }

