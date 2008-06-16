## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### EXPAND

toolConstructors$expand <- function(playState)
{
    if (playState$accepts.arguments == FALSE) return(NA)
    if (!playState$is.lattice) return(NA)
    quickTool(playState,
              label = "Panel",
              icon = "gtk-fullscreen",
              tooltip = "Choose a panel to expand and focus (for further interaction)",
              f = expand_handler,
              post.plot.action = expand_postplot_action,
              show = FALSE,
              isToggle = T)
}

expand_handler <- function(widget, playState)
{
    playDevSet(playState)
    ## check new expanded setting
    if (widget["active"]) {
        playPrompt(playState,
                   "Click on a panel to expand. (Right-click to cancel)")
        on.exit(playPrompt(playState, NULL))
        newFocus <- trellis.focus()
        if (!any(newFocus)) {
            widget["active"] <- FALSE
            return()
        }
        playState$.old.call.layout <- callArg(playState, layout)
        callArg(playState, layout) <- c(0,1,1)
        playState$.old.page <- playState$page
        playState$page <- packet.number()
        playState$.old.pages <- playState$pages
    } else {
        if (is.null(playState$.old.page)) return()
        callArg(playState, layout) <- playState$.old.call.layout
        playState$page <- playState$.old.page
        playState$pages <- playState$.old.pages
        rm(.old.call.layout, .old.page, .old.pages, envir=playState)
    }
    playReplot(playState)
}

expand_postplot_action <- function(widget, playState)
{
    widget["visible"] <- (widget["active"] ||
                          (length(trellis.currentLayout()) > 1))
}
