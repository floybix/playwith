## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### HELP

toolConstructors$help <- function(playState)
{
    quickTool(playState,
              label = "Help",
              icon = "gtk-dialog-question",
              tooltip = "Show help page for this plot function",
              f = help_handler)
}

help_handler <- function(widget, playState)
{
    if (playState$accepts.arguments == FALSE) {
        gmessage.error("Do not know the name of the plot function.")
        return()
    }
    callName <- deparseOneLine(playState$call[[1]])
    ## work out which (S3) method was called, if any
    methNames <- methods(callName)
    if ((length(methNames) > 0) && length(playState$call > 1)) {
        myClass <- class(callArg(playState, 1))
        myMeth <- paste(callName, myClass, sep=".")
        ok <- (myMeth %in% methNames)
        if (any(ok)) callName <- myMeth[ok][1]
    }
    print(help(callName))
}
