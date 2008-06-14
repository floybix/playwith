## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### Tools

playApplicationTools <-
    list("options",
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
         )

playInteractionTools <-
    list("expand",
         "identify",
         "annotate",
         "arrow",
         "edit.annotations",
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
         "---",
         "coords"
         )

toolConstructors <-
    list(`--` = function(...) gtkSeparatorToolItem(),
         `---` = function(...) {
             foo <- gtkSeparatorToolItem()
             foo$setExpand(TRUE)
             foo$setDraw(FALSE)
             foo
         }
         )

### Convenient constructor function

quickTool <-
    function(playState,
             label = "",
             icon.name = NULL,
             tooltip = NULL,
             f = NULL,
             data = NULL,
             post.plot.action = NULL,
             isToggle = FALSE,
             show = TRUE)
{
    x <- if (isToggle) gtkToggleToolButton(show=show)
    else gtkToolButton(show=show)
    x["label"] <- label
    x["icon-name"] <- icon.name
    if (!is.null(tooltip)) {
        result <- try(x$setTooltipText(tooltip), silent=TRUE)
        if (inherits(result, "try-error"))
            x$setTooltip(gtkTooltips(), tooltip) ## deprecated
    }
    if (!is.null(f)) {
        if (is.null(data)) data <- playState
        else data$playState <- playState
        gSignalConnect(x, "clicked", f, data=data)
    }
    if (!is.null(post.plot.action))
        attr(x, "post.plot.action") <- post.plot.action
    x
}

