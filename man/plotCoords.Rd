\name{plotCoords}
\alias{plotCoords}
\alias{plotCoords.default}
\alias{plotCoords.qqnorm}
\alias{plotCoords.qqplot}
\alias{plotCoords.plot}
\alias{plotCoords.plot.default}
\alias{plotCoords.plot.dendrogram}
\alias{plotCoords.plot.hclust}
\alias{plotCoords.plot.mca}
\alias{plotCoords.biplot}
\alias{plotCoords.biplot.default}
\alias{plotCoords.biplot.prcomp}
\alias{plotCoords.biplot.princomp}
\alias{plotCoords.qqmath}
\alias{plotCoords.cloud}
\alias{plotCoords.parallel}
\alias{plotCoords.splom}
\alias{case.names.dendrogram}
\alias{case.names.hclust}
\alias{case.names.prcomp}
\alias{case.names.princomp}
\alias{case.names.mca}
\alias{case.names.ts}
\alias{case.names.zoo}
\alias{case.names.Date}
\alias{case.names.POSIXt}
\alias{case.names.yearmon}
\alias{case.names.yearqtr}
\title{ API for defining data coordinates of a plot }
\description{
  Given a call to a plot function, return the data coordinates.
}
\usage{
plotCoords(name, object, call, envir, ...)

\method{plotCoords}{default}(name, object, call, envir, data, panel.args, ...)

\method{plotCoords}{qqnorm}(name, object, call, envir, ...)
\method{plotCoords}{qqplot}(name, object, call, envir, ...)

plotCoords.plot(name, object, call, envir, ...)
\method{plotCoords.plot}{default}(name, object, call, envir, data, ...)
\method{plotCoords.plot}{dendrogram}(name, object, call, envir, ...)
\method{plotCoords.plot}{mca}(name, object, call, envir, ...)

plotCoords.biplot(name, object, call, envir, ...)
\method{plotCoords.biplot}{default}(name, object, call, envir, ...)
\method{plotCoords.biplot}{prcomp}(name, object, call, envir, ...)
\method{plotCoords.biplot}{princomp}(name, object, call, envir, ...)

\method{plotCoords}{qqmath}(name, object, call, envir, panel.args, ...)
\method{plotCoords}{cloud}(name, object, call, envir, panel.args, ...)
\method{plotCoords}{parallel}(name, object, call, envir, panel.args, ...)
\method{plotCoords}{splom}(name, object, call, envir, panel.args,
    packet, ...)
}
\arguments{
  \item{name}{ The class of this object is the name of the plot
    function. Hence methods can be defined for different plot
    functions. }
  \item{object}{ the object passed as first argument to the plot call. }
  \item{call}{ the plot call. }
  \item{envir}{ environment containing objects referenced by the
    call. Call arguments should be evaluated in this environment. }
  \item{panel.args}{ passed for Lattice plots only: panel arguments for the
    relevant panel. }
  \item{packet}{ passed for Lattice plots only: packet number for which
    to return data (corresponds to \code{panel.args}). }
  \item{data}{ passed for non-Lattice plots only: a "data" argument, or NULL,
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

