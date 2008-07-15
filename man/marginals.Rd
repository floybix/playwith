\name{marginals}
\alias{marginals}
\title{ Display marginal distributions }
\description{
  Display marginal distributions of several variables,
  which may be numeric and/or categorical, on one plot.
}
\usage{
marginals(data,
          reorder = TRUE,
          subset = TRUE,
          plot.points = FALSE,
          ref = TRUE,
          origin = 0,
          levels.fos = NULL,
          xlab = NULL, ylab = NULL,
          cex = 0.5,
          ...,
          as.table = TRUE,
          subscripts = TRUE)
}
\arguments{
  \item{data}{ a data frame. }
  \item{reorder}{ whether to reorder factor variables by frequency. }
  \item{subset}{ data subset expression, evaluated with \code{data}. }
  \item{plot.points, ref}{ passed to \code{panel.densityplot}. }
  \item{origin, levels.fos}{ passed to \code{panel.dotplot}. }
  \item{xlab, ylab, cex, as.table,
    subscripts}{
    see \code{\link[lattice]{xyplot}}.
  }
  \item{\dots}{ passed to \code{\link[lattice]{panel.densityplot}} and
    \code{\link[lattice]{panel.dotplot}}. }
}
\details{
  This implementation is currently a bit of a hack, in the case of
  mixed numeric and categorical variables.
}
\value{
  a trellis object.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link[lattice]{panel.dotplot}},
  \code{\link[lattice]{panel.densityplot}} }
\examples{
enviro <- transform(environmental, bad = ozone > 50)
marginals(enviro)
}
\keyword{ hplot }
