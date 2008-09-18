\name{latticist}
\alias{latticist}
\alias{try.prepanel.loess}
\title{ A Lattice GUI }
\description{
  Interactively explore a data set using \code{\link[lattice]{Lattice}} displays.
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
  \item{dat}{ a data frame (with numeric and/or categorical variables). }
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
  \item{plot.call}{ call defining the initial plot display.
  This can be given instead of \code{dat}, and the dataset will be
  derived from it (see examples). }
  \item{\dots, labels, time.mode}{
    further arguments passed to \code{\link{playwith}}.
  }
}
\details{
  Latticist is an extension to \code{\link{playwith}}, adding a
  toolbar to construct lattice displays for a given dataset.
  As such, many of the features, such as style settings and linked
  brushing, are (or should be) described in the help for
  \code{\link{playwith}}.

  It is recommended to install the \pkg{deldir} package, as well as
  \pkg{hexbin}, a BioConductor package, which can be installed as follows:

  \code{source("http://bioconductor.org/biocLite.R")}

  \code{biocLite("hexbin")}
}
\value{
  invisibly returns the \code{playState} object representing the plot
  window: see \code{\link{playwith.API}}.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\references{
  For an excellent introduction to and coverage of Lattice:

Sarkar, Deepayan (2008) "Lattice: Multivariate Data Visualization with
R", Springer. \url{http://lmdvr.r-forge.r-project.org/}
}
\seealso{
  \code{\link{playwith}},
  \code{\link[lattice]{Lattice}},
  \code{lattice.demo} in the \pkg{TeachingDemos} package. }
\examples{
if (interactive()) {
options(device.ask.default = FALSE)

## CO2 dataset: see ?CO2
## From an experiment on the cold tolerance of a grass species.
## Try: uptake ~ conc
##      uptake ~ conc | Plant
##      uptake ~ conc | Type, groups = Treatment
latticist(CO2)

## National Health and Nutrition Examination Survey.
## Try: ~ Smoke, groups = Sex
##      Sex ~ Smoke
##      Smoke ~ Age | Sex
##      ~ Diet.Iron, groups = Sex
##      Hemoglobin ~ Diet.Iron | Sex
##      (with both x and y "discretized")
if (require(hexbin)) {
  readline("Hit Enter for 'NHANES' example...")
  data(NHANES)
  latticist(NHANES)
}

## audit dataset: artificial data representing tax work.
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
