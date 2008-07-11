\name{latticist}
\alias{latticist}
\title{ A Lattice GUI }
\description{
  Interactively visualise a data set using \code{\link[lattice]{Lattice}} displays.
}
\usage{
latticist(dat, plot.call = quote(marginals(dat)), ...)
}
\arguments{
  \item{dat}{ a data frame containing numeric and/or categorical variables. }
  \item{plot.call}{ call describing the initial plot display. }
  \item{\dots}{ further arguments passed to \code{\link{playwith}}. }
}
\details{
  TODO
}
\value{
  invisibly returns the \code{playState} object representing the plot
  window: see \code{\link{playDevCur}}.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{playwith}}, \code{\link[lattice]{Lattice}} }
\examples{
if (interactive()) {

library(rattle)
audit <- read.csv(system.file("csv", "audit.csv", package = "rattle"))
latticist(audit)

}
}
\keyword{ hplot }
\keyword{ iplot }
\keyword{ dynamic }
