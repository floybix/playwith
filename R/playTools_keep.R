## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### KEEP

toolConstructors$keep <- function(playState)
{
    widget <- quickTool(playState,
                        label = "Keep plot",
                        icon = "gtk-media-stop",
                        tooltip = "Keep this window, do not replace it (open next plot in a new window)",
                        f = keep_handler,
                        isToggle = TRUE)
    if (isTRUE(playState$keep)) widget["active"] <- TRUE
    widget
}

keep_handler <- function(widget, playState)
    playState$keep <- widget["active"]

