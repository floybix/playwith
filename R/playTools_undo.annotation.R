## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### UNDO.ANNOTATION

toolConstructors$undo.annotation <- function(playState)
{
    ## only works in basic device mode!
    if (!isBasicDeviceMode(playState)) return(NA)
    quickTool(playState,
              label = "Undo ann.",
              icon = "gtk-undo",
              tooltip = "Remove last annotation",
              f = undo.annotation_handler,
              show = !is.null(playState$.recorded.plot))
}

undo.annotation_handler <- function(widget, playState)
{
    redoPlot <- recordPlot()
    try(replayPlot(playState$.recorded.plot))
    generateSpaces(playState)
    playState$.recorded.plot <- redoPlot
}
