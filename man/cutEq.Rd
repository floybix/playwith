\name{cutEq}
\alias{cutEq}
\alias{reorderByFreq}
\title{ Cut into equal-sized groups. }
\description{
  Discretize using equally spaced quantiles for the breaks.
}
\usage{
cutEq(x, n, type = 2, dig.lab = 4, ...)

reorderByFreq(x)
}
\arguments{
  \item{x}{ for \code{cutEq}, a numeric vector.
    for \code{reorderByFreq}, a factor variable. }
  \item{n}{ number of levels (groups) to cut into. }
  \item{type}{ type of quantile, see \code{\link{quantile}}. }
  \item{dig.lab}{ digits to use in formatting labels. }
  \item{\dots}{ ignored. }
}
\details{

}
\value{

}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{cut}}, \code{cut2} in \code{Hmisc} package, \code{\link{reorder}} }
\examples{

}
\keyword{ utilities }
