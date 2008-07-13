\name{latticist}
\alias{latticist}
\title{ A Lattice GUI }
\description{
  Interactively visualise a data set using \code{\link[lattice]{Lattice}} displays.
}
\usage{
latticist(dat,
          reorder.levels = TRUE,
          plot.call = quote(marginals(dat)),
          ...)
}
\arguments{
  \item{dat}{ a data frame containing numeric and/or categorical variables. }
  \item{reorder.levels}{ if \code{TRUE}, change the ordering of levels of factor
  variables so they are in order of frequency. Note that this does not
  change the data, only its internal representation: see
  \code{\link{reorder}}. This is recommended for effective graphic
  displays. Factor variables of class \code{ordered} are not reordered.
  Therefore you should ensure that any variables whose levels have an inherent
  order are of class \code{ordered}: see \code{\link{as.ordered}}.
  Warning: the result of \code{\link{cut}} is by default NOT \code{ordered}!
  }
  \item{plot.call}{ call defining the initial plot display. }
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
