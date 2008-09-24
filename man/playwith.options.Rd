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
  Many of these simply provide defaults for corresponding arguments
  to the \code{\link{playwith}} function.
}
\seealso{ \code{\link{playwith}} }
\examples{
str(playwith.options())

## list options are merged, not replaced
playwith.getOption("arrow")
playwith.options(arrow = list(type = "closed", length = 0.1))
playwith.getOption("arrow")

oopt <- playwith.options()

playwith.options(save.as.format = "png")
playwith.options(toolbar.style = "icons")
playwith.options(deparse.options =
    c("keepInteger", "showAttributes", "keepNA"))

## make a new "style shortcut" (an arbitrary expression)
## to add a standard sub-title to the plot:
doMySub <- quote({
    txt <- ginput("Enter subtitle text:",
                  text = paste(Sys.time(), Sys.info()["login"],
                               R.version.string, sep = ", "))
    if (!is.na(txt))
        callArg(playState, "sub") <- if (nchar(txt) > 0) txt
})
playwith.options(styleShortcuts = list("mySub" = doMySub))

## try it:
if (interactive())
  playwith(plot(1:10))

## reset
playwith.options(oopt)
}
\keyword{ programming }

