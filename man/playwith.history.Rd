\name{playwith.history}
\alias{playwith.history}
\title{Session history for playwith}
\description{
  A basic history of plot calls in playwith, like \code{\link{history}}.
}
\usage{
playwith.history(max.show = 100, ...)
}
\arguments{
  \item{max.show}{ maximum number of history items to show. }
  \item{...}{ ignored. }
}
\details{
  The returned history is a combination of the stored session history
  (stored in \code{playwith:::.PlaywithEnv$history} and the current
  history for all open \code{playwith} windows.
}
\seealso{ \code{\link{playwith}} }
\examples{
if (interactive()) {
    playwith.history()
}
}
\keyword{ utilities }
