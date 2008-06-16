## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### CLEAR

toolConstructors$clear <- function(playState)
{
    if (isBasicDeviceMode(playState))
        ## do not know the call; it cannot be redrawn (TODO?)
        return(NA)
    types <- c(
               if (length(playState$ids) > 0) "ids",
               if (length(playState$annotations) > 0) "annotations",
               if (length(playState$brushed) > 0) "brushed"
               )
    quickTool(playState,
              label = "Clear",
              icon = "gtk-clear",
              tooltip = "Remove labels and annotations",
              f = clear_handler,
              show = (length(types) > 0))
}

clear_handler <- function(widget, playState)
{
    ## TODO: allow new tools to specify more items to clear
    types <- c(
               if (length(playState$ids) > 0) "ids",
               if (length(playState$annotations) > 0) "annotations",
               if (length(playState$brushed) > 0) "brushed"
               )
    if (length(types) == 0) { widget$hide(); return() }
    clear.types <- types
    if (length(types) > 1) {
        clear.types <- select.list(types, preselect = types, multiple = TRUE,
                                   title = "Clear what?")
    }
    for (x in clear.types)
        playState[[x]] <- list()
    if (length(clear.types) == length(types)) {
        ## everything was cleared
        widget$hide()
    }
    if ("annotations" %in% clear.types) {
        editAnnTool <- playState$tools$edit.annotations
        if (!is.null(editAnnTool)) editAnnTool["visible"] <- FALSE
    }
    playReplot(playState)
}
