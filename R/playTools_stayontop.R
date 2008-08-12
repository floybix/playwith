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
                        tooltip = "Show this window above all others",
                        f = stay.on.top_handler,
                        isToggle = TRUE)
    if (isTRUE(playState$stay.on.top)) widget["active"] <- TRUE
    widget
}

stay.on.top_handler <- function(widget, playState)
{
    playState$stay.on.top <- widget["active"]
    playState$win$setKeepAbove(widget["active"])
}

