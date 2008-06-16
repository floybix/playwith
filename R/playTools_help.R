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
    callName <- deparseOneLine(callArg(playState, 0))
    ## work out which (S3) method was called, if any
    methNames <- methods(callName)
    if (length(methNames) > 0) {
        myClass <- try(class(callArg(playState, 1)), silent=TRUE)
        if (!inherits(myClass, "try-error")) {
            myMeth <- paste(callName, myClass, sep=".")
            ok <- (myMeth %in% methNames)
            if (any(ok)) callName <- myMeth[ok][1]
        }
    }
    print(help(callName))
}
