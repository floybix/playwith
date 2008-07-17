\name{latticist}
\alias{latticist}
\title{ A Lattice GUI }
\description{
  Interactively visualise a data set using \code{\link[lattice]{Lattice}} displays.
}
\usage{
latticist(dat,
          reorder.levels = TRUE,
          plot.call = quote(marginals(dat, reorder=FALSE)),
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
  Warning: the result of \code{\link{cut}} is by default NOT
  \code{ordered}!

  Integer variables with only one or two levels are converted to factors.
  }
  \item{plot.call}{ call defining the initial plot display. }
  \item{\dots}{ further arguments passed to \code{\link{playwith}}. }
}
\details{
  To change the style of lattice graphics, use \code{lattice.options(default.theme=...)}.
See \code{\link[lattice]{show.settings}} and \code{\link[lattice]{simpleTheme}}.
}
\value{
  invisibly returns the \code{playState} object representing the plot
  window: see \code{\link{playDevCur}}.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{playwith}}, \code{\link[lattice]{Lattice}} }
\examples{
if (interactive()) {
op <- options(device.ask.default = FALSE)

latticist(CO2)

if (require(rattle)) {
  invisible(readline("When you are ready..."))
  audit <- read.csv(system.file("csv", "audit.csv",
                    package = "rattle"))
  latticist(audit, new=TRUE)
}

options(op)
}
}
\keyword{ hplot }
\keyword{ iplot }
\keyword{ dynamic }
