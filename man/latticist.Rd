\name{latticist}
\alias{latticist}
\alias{try.prepanel.loess}
\title{ A Lattice GUI }
\description{
  Interactively visualise a data set using \code{\link[lattice]{Lattice}} displays.
}
\usage{
latticist(dat,
          reorder.levels = TRUE,
          plot.call = quote(marginals(dat, reorder=FALSE)),
          ...,
          labels = rownames(dat),
          time.mode = FALSE)
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
  Note: the result of \code{\link{cut}} is by default NOT
  \code{ordered}!
  Numeric variables with discrete values in {0, 1, -1} are converted to factors.
  }
  \item{plot.call}{ call defining the initial plot display. }
  \item{\dots, labels, time.mode}{
    further arguments passed to \code{\link{playwith}}.
  }
}
\details{
  Just try it...

  It is recommended to install \pkg{hexbin}, which is a BioConductor
  package, as follows:

  \code{source("http://bioconductor.org/biocLite.R")}

  \code{biocLite("hexbin")}

  To change the style of lattice graphics, use \code{lattice.options(default.theme=...)}.
See \code{\link[lattice]{show.settings}},
\code{\link[lattice]{simpleTheme}}
and \code{\link[latticeExtra]{custom.theme}}.
}
\value{
  invisibly returns the \code{playState} object representing the plot
  window: see \code{\link{playDevCur}}.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\references{
  For an excellent introduction to and coverage of Lattice:

Sarkar, Deepayan (2008) "Lattice: Multivariate Data Visualization with
R", Springer. \url{http://lmdvr.r-forge.r-project.org/}
}
\seealso{ \code{\link{playwith}}, \code{\link[lattice]{Lattice}} }
\examples{
if (interactive()) {
options(device.ask.default = FALSE)

latticist(CO2)

if (require(hexbin)) {
  readline("Hit Enter for 'NHANES' example...")
  data(NHANES)
  latticist(NHANES)
}

if (require(rattle)) {
  readline("Hit Enter for 'audit' example...")
  audit <- read.csv(system.file("csv", "audit.csv",
                    package = "rattle"))
  latticist(audit, new = TRUE)
}

}
}
\keyword{ hplot }
\keyword{ iplot }
\keyword{ dynamic }
