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
          ref = TRUE,
          levels.fos = NULL,
          plot.points = FALSE,
          xlab = NULL, ylab = NULL,
          cex = 0.5,
          ...,
          as.table = TRUE,
          subscripts = TRUE,
          default.scales = list(relation = "free", draw = FALSE))
}
\arguments{
  \item{data}{ a data frame. }
  \item{reorder}{ whether to reorder factor variables by frequency. }
  \item{subset}{ data subset expression, evaluated with \code{data}. }
  \item{ref, plot.points}{ passed to \code{panel.densityplot}. }
  \item{levels.fos}{ passed to \code{panel.dotplot}. }
  \item{xlab, ylab, cex, as.table,
    subscripts, default.scales}{
    see \code{\link[lattice]{xyplot}}.
  }
  \item{\dots}{ passed to \code{\link[lattice]{panel.densityplot}} and
    \code{\link[lattice]{panel.dotplot}}. }
}
\details{

}
\value{
  a trellis object.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link[lattice]{panel.dotplot}},
  \code{\link[lattice]{panel.densityplot}} }
\examples{

}
\keyword{ hplot }
