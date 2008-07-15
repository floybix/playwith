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
\code{cutEq} is similar to \code{\link{cut}} but divides into roughly
equally-sized groups, rather than dividing into bins of equal width.

\code{reorderByFreq} orders levels of a factor by their frequency.
It is basically \code{reorder(x, x, length)}, but reversed.
}
\value{
  \code{cutEq} returns an ordered factor.
  \code{reorderByFreq} returns a factor with the same values as its
  input, but with levels reordered.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{cut}}, \code{cut2} in \code{Hmisc} package, \code{\link{reorder}} }
\examples{
summary(cut(rnorm(100), 4, dig=2))
summary(cutEq(rnorm(100), 4, dig=2))

foo <- factor(c("c","b","b","a"))
levels(foo)
# "a" "b" "c"
levels(reorderByFreq(foo))
# "b" "a" "c"
}
\keyword{ utilities }
