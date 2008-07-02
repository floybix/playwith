\name{playwith.options}
\alias{playwith.options}
\alias{playwith.getOption}
\title{User default settings for playwith}
\description{
  A basic user settings facility, like \code{\link{options}} and
  \code{\link[lattice]{lattice.options}}.
}
\usage{
playwith.options(...)
playwith.getOption(name)
}
\arguments{
  \item{name}{ character giving the name of a setting. }
  \item{...}{ new options can be defined, or existing ones modified, using
          one or more arguments of the form 'name = value' or by
          passing a list of such tagged values.  Existing values can be
          retrieved by supplying the names (as character strings) of
          the components as unnamed arguments.
    }
}
\details{
  These functions are direct copies of the lattice equivalents:
  see \code{\link[lattice]{lattice.options}}.

  The available options can be seen with \code{str(playwith.options())}.
  Most of these simply provide defaults for corresponding arguments
  to the \code{\link{playwith}} function.
}
\seealso{ \code{\link{playwith}} }
\examples{
str(playwith.options())
playwith.getOption("top.tools")
}
\keyword{ programming }

