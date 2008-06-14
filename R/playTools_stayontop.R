## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### STAYONTOP

toolConstructors$stayontop <- function(playState)
{
    widget <- quickTool(playState,
                        label = "Stay on top",
                        icon = "gtk-leave-fullscreen",
                        tooltip = "Always show this window",
                        f = stayontop_handler,
                        isToggle = TRUE)
    if (isTRUE(playState$stay.on.top)) widget["active"] <- TRUE
    widget
}

stayontop_handler <- function(widget, playState)
{
    playState$stay.on.top <- widget["active"]
    playState$win$setKeepAbove(widget["active"])
}

