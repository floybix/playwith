## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer


.defaultPlaywithOptions <- function()
    list(
         ## named arguments to playwith():
         new = FALSE,
         top.tools = list(
         "options",
         "stayontop",
         "keep",
         "save",
         "copy",
         "print",
         "data",
         "--",
         "settings",
         "theme",
         "---",
         "time.mode"
         ),
         left.tools = list(
         "expand",
         "identify",
         "annotate",
         "arrow",
         "edit.annotations",
         "undo.annotation",
         "brush",
         "clear",
         "--",
         "zoomin.3d",
         "zoomout.3d",
         "fly.left.3d",
         "fly.right.3d",
         "zoom",
         "zoomout",
         "zoomfit",
         "zero",
         "--",
         "inspector",
         "---",
         "coords",
         "clickhandler"
         ),
         bottom.tools = list(),
         right.tools = list(),
         width = 6,
         height = 5,
         pointsize = 10,
         eval.args = NA,
         ## implicit arguments to playwith():
         ## (can be over-ridden by explicit arguments)
         show.tooltips = FALSE,
         annotation.mode = "plot",
         clip.annotations = FALSE,
         label.style = NULL,
         label.offset = 0.5,
         arrow.style = NULL,
         arrow.arrow = quote(arrow(length=unit(0.15, "inches"))),
         ## global:
         parameters.toolbar = "bottom",
         toolbar.style = "both",
         catch.errors = TRUE,
         deparse.options = c("keepInteger")
         )

## code below copied from lattice

playwith.getOption <- function(name)
{
    .PlaywithEnv$options[[name]]
}

playwith.options <- function(...)
{
    ## this would have been really simple if only form allowed were
    ## lattice.options("foo", "bar") and
    ## lattice.options(foo=1, bar=2). But it could also be
    ## lattice.options(foo=1, "bar"), which makes some juggling necessary

    new <- list(...)
    if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]])) new <- new[[1]]
    old <- .PlaywithEnv$options

    ## if no args supplied, returns full options list
    if (length(new) == 0) return(old)

    nm <- names(new)
    if (is.null(nm)) return(old[unlist(new)]) ## typically getting options, not setting
    isNamed <- nm != "" ## typically all named when setting, but could have mix
    if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])

    ## so now everything has non-"" names, but only the isNamed ones should be set
    ## everything should be returned, however

    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]

    ## this used to be

    ## modified <- updateList(retVal[nm], new[nm])
    ## .LatticeEnv$lattice.options[names(modified)] <- modified

    ## but then calling lattice.options(foo = NULL) had no effect
    ## because foo would be missing from modified.  So, we now do:

    updateList <- function (x, val) {
        if (is.null(x)) x <- list()
        modifyList(x, val)
    }
    .PlaywithEnv$options <- updateList(old, new[nm])

    ## return changed entries invisibly
    invisible(retVal)
}
