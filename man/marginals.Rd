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
          plot.points = FALSE,
          ref = TRUE,
          origin = 0,
          levels.fos = NULL,
          xlab = NULL, ylab = NULL,
          cex = 0.5,
          ...,
          subset = TRUE,
          as.table = TRUE,
          subscripts = TRUE,
          default.scales = list(
            x = list(relation="free", abbreviate=TRUE,
                     rot=60, cex=0.5, tick.number=3),
            y = list(relation="free", draw=FALSE)))
}
\arguments{
  \item{data}{ a data frame. }
  \item{reorder}{ whether to reorder factor variables by frequency. }
  \item{subset}{ data subset expression, evaluated with \code{data}. }
  \item{plot.points, ref}{ passed to \code{panel.densityplot}. }
  \item{origin, levels.fos}{ passed to \code{panel.dotplot}. }
  \item{xlab, ylab, cex, as.table,
    subscripts, default.scales}{
    see \code{\link[lattice]{xyplot}}.
  }
  \item{\dots}{ passed to \code{\link[lattice]{panel.densityplot}} and
    \code{\link[lattice]{panel.dotplot}}. }
}
\details{
  In the case of mixed numeric and categorical variables,
  the trellis objects from \code{dotplot()} and \code{densityplot}
  are merged.
}
\value{
  a trellis object.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link[lattice]{panel.dotplot}},
  \code{\link[lattice]{panel.densityplot}} }
\examples{
enviro <- environmental
## make an ordered factor (so it will not be reordered)
enviro$smell <- cut(enviro$ozone, breaks=c(0,30,50,Inf),
    labels=c("ok","hmmm","yuck"), ordered=TRUE)
marginals(enviro)
}
\keyword{ hplot }
