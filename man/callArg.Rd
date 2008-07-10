\name{callArg}
\alias{callArg}
\alias{callArg<-}
\alias{mainCall}
\alias{mainCall<-}
\title{Work with plot call arguments}
\description{
  Part of the \code{\link{playwith}} Application Programming Interface.
}
\usage{
callArg(playState, arg, expr, eval = TRUE, data = NULL)
callArg(playState, arg, expr) <- value

mainCall(playState)
mainCall(playState) <- value
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the plot, window and device. }
  \item{arg}{ the argument name or number in the plot call (\code{playState$call}).
  	Argument numbers start from 1 (so 0 refers to the function name).
  	This can also be a language object (e.g. \code{quote(scales$log)}).
  }
  \item{expr}{ an alternative way to specify \code{arg}: as a raw
    expression. This is evaluated in the calling environment, so can
    refer to local variables (e.g. \code{data[[myname]]}). }
  \item{eval}{ whether to evaluate the argument before returning it.
    Otherwise, just return the quoted expression that appears in the
    call. }
  \item{data}{ a list or environment in which to evaluate the argument.
    Typically this will be the "data" argument to lattice or qplot functions. }
  \item{value}{ the value to assign. }
}
\details{
  These functions get and set argument values in the playState plot call.

  As convenience functions for setting arguments in \code{playState$call},
  the \code{callArg} function helps by:
  \itemize{
        \item referring to the main call that accepts plot arguments,
 which is not necessarily the top-level call.
	\item automatically evaluating variables that have been stored in a local environment
	(\code{playState$env}).
	\item converting lists to language objects so that the call can be deparsed elegantly.
	\item enforcing exact matching of argument names \code{exact=TRUE} (see \code{\link{[[}}).
  }
}
\value{
	returns the value of the specified argument,
	as if using the \code{$} or \code{[[} extractor functions, but
	evaluated in a local environment (\code{playState$env}).
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playwith.API}} }
\examples{
if (interactive()) {

library(lattice)
playwith(xyplot(1:10 ~ 1:10))
playState <- playDevCur()

callArg(playState, "pch")  ## NULL
callArg(playState, "pch") <- "$"
callArg(playState, "pch")  ## "$"
playReplot(playState)

## referring to local variables
tmp <- "x"
callArg(playState, expr = scales[[tmp]]$cex) <- 2
playReplot(playState)

## quoted expressions can also be used
callArg(playState, quote(scales$x$cex))

}
}
\keyword{ programming }

