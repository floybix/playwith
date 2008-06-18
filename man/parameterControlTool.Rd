\name{parameterControlTool}
\alias{parameterControlTool}
\title{ Create playwith tools for controlling parameter values }
\description{
  Part of the \code{\link{playwith}} Application Programming Interface.
}
\usage{
## Note: this is only to be called inside a tool constructor function.
parameterControlTool(playState, name, value,
                     label = name, horizontal = TRUE,
                     spinbutton = FALSE)
}
\arguments{
  \item{playState}{ a \code{playState} object, as passed in to the constructor function. }
  \item{name}{ the parameter name as it appears in the plot call. }
  \item{value}{ the possible or starting values for the parameter.
    Can be a single or vector value, of
    \code{integer}, \code{numeric}, \code{character} or \code{logical}.
    See \code{\link{playwith}} for details. }
  \item{label}{ label for the widget. }
  \item{horizontal}{ FALSE to make a tool for a vertical toolbar. }
  \item{spinbutton}{ make a spinbutton rather than a slider for numerics. }
}
\details{
  Note: this is only to be called inside a tool constructor function.
  It is not intended to be called directly.
}
\value{
  a \code{\link[RGtk2]{gtkToolButton}}.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{quickTool}}, \code{\link{playwith}} }
\examples{

}
\keyword{ iplot }
\keyword{ programming }
