## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

identifyActionGroup <- function(playState)
{
    entries <-
        list( ## : name, stock icon, label, accelerator, tooltip, callback
             list("Identify", "gtk-info", "_Identify...", NULL, "Identify all points in a selected region", identify_handler),
             list("IdTable", "gtk-info", "Select from _table...", NULL, "Select points from a table", id.table_handler),
             list("FindLabels", "gtk-find", "_Find...", "<Ctrl>F", "Find points with labels matching...", id.find_handler),
             list("SaveIDs", NULL, "_Save IDs...", NULL, "_Save current IDs to an object", save.ids_handler)
             )

    ## construct action group with playState passed to callbacks
    aGroup <- gtkActionGroupNew("IdentifyActions")
    aGroup$addActions(entries, playState)
    aGroup
}

id.table_handler <- function(widget, playState) {
}

id.find_handler <- function(widget, playState) {
}

save.ids_handler <- function(widget, playState) {

}
