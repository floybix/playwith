\name{callArg}
\alias{callArg}
\alias{callArg<-}
\title{Work with plot call arguments}
\description{
  Part of the \code{\link{playwith}} Application Programming Interface.
}
\usage{
callArg(playState, arg, name = NULL)
callArg(playState, arg, name = NULL) <- value
}
\arguments{
  \item{playState}{ a \code{\link{playState}} object representing the plot, window and device. }
  \item{arg}{ the argument name or number in the plot call (\code{playState$call}).
  	Argument numbers start from 1 (so 0 refers to the function name).
  	For setting, this can also be an expression
  }
  \item{name}{ the argument name given as a character string. 
  This is sometimes a convenient alternative to the quoted \code{arg}. }
  \item{value}{ the value to set for argument \code{arg}. }
}
\details{
  These functions get and set argument values in the playState plot call.
  
  While it would be easy to get and set arguments in \code{playState$call} directly, 
  these API functions help by:
  \itemize{
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

callArg(playState, pch)  # NULL
callArg(playState, pch) <- "$"
playReplot(playState)

callArg(playState, pch)         # "$"
callArg(playState, name="pch")  # "$"

callArg(playState, scales$x$cex) <- 2
playReplot(playState)

}
}
\keyword{ programming }

