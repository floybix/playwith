## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

globalActionGroup <- function(playState)
{
    entries <-
        list( ## : name, stock icon, label, accelerator, tooltip, callback
             list("Clone", "gtk-new", "Clo_ne window", "<Ctrl>N", "Duplicate this window", clone_handler),
             list("Save", "gtk-save", "_Save", "<Ctrl>S", "Export current plot to an image file", save_handler),
             list("Copy", "gtk-copy", "_Copy", "<Ctrl>C", "Copy current plot as an image", copy_handler),
             list("Print", "gtk-print", "_Print", "<Ctrl>P", "Print current plot", print_handler),
             list("Close", "gtk-close", "Close", "<Ctrl>W", "Close window and device", close_handler),
             list("SetSize", NULL, "Set device _size...", "<Ctrl>0", NULL, set.size_handler),
             list("IncrFont", NULL, "_Increase font size", "<Ctrl>plus", NULL, incr.font_handler),
             list("DecrFont", NULL, "De_crease font size", "<Ctrl>minus", NULL, decr.font_handler),
             list("SetLabelStyle", NULL, "Set label st_yle...", NULL, NULL, set.label.style_handler),
             list("CustomTheme", "gtk-select-color", "Custom _Theme...", NULL, "Customise plot style...", custom.theme_handler),
             list("EditCall", "gtk-edit", "_Edit call...", "<Ctrl>E", "Edit the plot call", edit.call_handler),
             list("Back", "gtk-go-back", "Back", "<Alt>Left", "Go back to previous plot call", back_handler),
             list("Forward", "gtk-go-forward", "Forward", "uparrow", "Go to next plot call", forward_handler),
             list("Redraw", "gtk-refresh", "Re_draw", "<Ctrl>R", NULL, redraw_handler),
             list("Reload", "gtk-refresh", "_Reload and redraw", "<Ctrl><Shift>R", NULL, reload_handler),
             list("SaveCode", "gtk-save", "Save c_ode", NULL, "Save R code for this plot and (optionally) data", save.code_handler),
             list("Source", NULL, "Plot s_ource", "<Ctrl>U", NULL, view.source_handler),
             list("HelpPlot", "gtk-help", "_Help for this plot", "<Ctrl>H", "Open help page for this plot", help_handler),
             list("HelpPlaywith", NULL, "help(playwith)", NULL, NULL, help.playwith_handler),
             list("About", NULL, "_About playwith", NULL, NULL, about_handler),
             list("Website", NULL, "_Website", NULL, "The playwith website (for contact, bugs, etc)", website_handler),
             list("SummariseData", NULL, "Summarise object"),
             list("EditData", NULL, "Edit object"),
             list("SaveData", NULL, "Save object"),
             list("PurgeObjects", NULL, "Purge object cache")
             )

    toggleEntries <-
        list( ## : name, stock icon, label, accelerator, tooltip, callback, active?
             list("Keep", "gtk-media-stop", "_Do not replace", "<Ctrl>D", "Do not replace with the next plot", keep_handler, FALSE),
             list("StayOnTop", "gtk-leave-fullscreen", "St_ay on top", NULL, "Show this window above all others", stay.on.top_handler, FALSE),
             list("Toolbars", NULL, "Toolbars", NULL, NULL, show.toolbars_handler, isTRUE(playState$show.toolbars)),
             list("Statusbar", NULL, "Status _bar", NULL, NULL, show.statusbar_handler, TRUE)
             )

    ## construct action group with playState passed to callbacks
    aGroup <- gtkActionGroupNew("GlobalActions")
    aGroup$addActions(entries, playState)
    aGroup$addToggleActions(toggleEntries, playState)
    aGroup
}

updateGlobalActionStates <- function(playState)
{
    aGroup <- playState$actionGroups[["GlobalActions"]]
    ## Back, Forward
    callEntry <- playState$widgets$callEntry
    nHistory <- callEntry$getModel()$iterNChildren()
    aGroup$getAction("Forward")$setSensitive(callEntry["active"] > 0)
    aGroup$getAction("Back")$setSensitive(callEntry["active"] < nHistory-1)
    ## Keep
    aGroup$getAction("Keep")$setActive(isTRUE(playState$keep))
    ## StayOnTop
    aGroup$getAction("StayOnTop")$setActive(isTRUE(playState$stay.on.top))
    ## Statusbar
    aGroup$getAction("Statusbar")$setActive(isTRUE(playState$show.statusbar))
}

clone_handler <- function(widget, playState)
    NA

close_handler <- function(widget, playState)
    window.close_handler(playState = playState)

set.size_handler <- function(widget, playState)
    NA

incr.font_handler <- function(widget, playState)
    NA
decr.font_handler <- function(widget, playState)
    NA

custom.theme_handler <- function(widget, playState)
    NA

back_handler <- function(widget, playState) {
    with(playState$widgets,
        callEntry["active"] <- callEntry["active"] + 1)
}

forward_handler <- function(widget, playState) {
    with(playState$widgets,
        callEntry["active"] <- callEntry["active"] - 1)
}

redraw_handler <- function(widget, playState)
    playReplot(playState)

reload_handler <- function(widget, playState)
    playNewPlot(playState)


save.code_handler <- function(widget, playState)
    NA

view.source_handler <- function(widget, playState)
    NA

help.playwith_handler <- function(widget, playState)
    print(help("playwith"))

about_handler <- function(widget, playState) {
    activate.email <- function(about, link, data)
        browseURL(paste("mailto:", link, sep=""))
    activate.url <- function(about, link, data)
        browseURL(link)
    gtkAboutDialogSetEmailHook(activate.email)
    gtkAboutDialogSetUrlHook(activate.url)

    gtkShowAboutDialog(playState$win,
                       title = "playwith",
                       name = "playwith",
                       version = packageDescription("playwith")$Version,
                       comments = "interactive plots in R using GTK+",
                       copyright = "(C) 2008 Felix Andrews",
                       license = "GNU General Public Licence version 2 or later",
                       website = "http://playwith.googlecode.com/",
                       authors = c("Felix Andrews <felix@nfrac.org>"),
                       documenters = c("Felix Andrews")
                       )
}

website_handler <- function(...)
    browseURL("http://playwith.googlecode.com/")

keep_handler <- function(widget, playState)
    playState$keep <- widget["active"]

stay.on.top_handler <- function(widget, playState) {
    playState$stay.on.top <- widget["active"]
    playState$win$setKeepAbove(widget["active"])
}

show.statusbar_handler <- function(widget, playState) {
    playState$widgets$statusbar["visible"] <- widget["active"]
}

show.toolbars_handler <- function(widget, playState) {
    playState$widgets$leftToolbar["visible"] <- widget["active"]
}
