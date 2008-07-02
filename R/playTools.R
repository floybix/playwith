## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### Tools

## the big list of built-in tools
## (added to by code in files playTools_*.R)
toolConstructors <-
    list(`--` = function(...) gtkSeparatorToolItem(),
         `~~` = function(...) {
             foo <- gtkSeparatorToolItem()
             foo$setDraw(FALSE)
             foo
         },
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
    x[["label"]] <- label
    x[["icon-name"]] <- icon.name
    if (!is.null(tooltip)) {
        result <- try(x[["tooltip-text"]] <- tooltip, silent=TRUE)
        #result <- try(x$setTooltipText(tooltip), silent=TRUE)
        #if (inherits(result, "try-error"))
        #    x$setTooltip(gtkTooltips(), tooltip) ## deprecated
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

parameterControlTool <-
    function(playState, name, value,
             label = name,
             horizontal = TRUE)
{
    stopifnot(length(value) > 0)
    if (!is.logical(value))
        label <- paste(label, ": ", sep="")
    ## signal handlers
    updateParamValue <- function(widget, playState) {
        newval <- widget[["value"]]
        oldval <- get(name, envir=playState$env)
        if (identical(oldval, newval)) return()
        assign(name, newval, envir=playState$env)
        playReplot(playState)
    }
    updateParamText <- function(widget, playState) {
        newval <- widget[["text"]]
        oldval <- get(name, envir=playState$env)
        if (identical(oldval, newval)) return()
        assign(name, newval, envir=playState$env)
        playReplot(playState)
    }
    updateParamTextNumeric <- function(widget, playState) {
        newval <- try(as.numeric(widget[["text"]]))
        if (inherits(newval, "try-error")) return()
        if (is.na(newval)) return()
        oldval <- get(name, envir=playState$env)
        if (identical(oldval, newval)) return()
        assign(name, newval, envir=playState$env)
        playReplot(playState)
    }
    updateParamCombobox <- function(widget, playState) {
        ## signal also emitted on typing, ignore
        if (widget$getActive() == -1) return()
        newval <- widget$getActiveText()
        oldval <- get(name, envir=playState$env)
        if (identical(oldval, newval)) return()
        assign(name, newval, envir=playState$env)
        playReplot(playState)
    }
    updateParamActive <- function(widget, playState) {
        newval <- widget[["active"]]
        oldval <- get(name, envir=playState$env)
        if (identical(oldval, newval)) return()
        assign(name, newval, envir=playState$env)
        playReplot(playState)
    }
    if (is.integer(value) || inherits(value, "AsIs")) {
        if (inherits(value, "AsIs")) value <- as.vector(value)
        box <- gtkVBox()
        box$packStart(gtkLabel(label))
        if (length(value) == 1) {
            ## only one value given -- make up a range
            range <- 10 * (1 + abs(value)) ^ 2 * c(-1, 1)
        } else {
            ## range given
            range <- range(value)
        }
        step <- min(diff(unique(sort(value))))
        widget <- gtkSpinButton(min=min(range), max=max(range), step=step)
        widget[["digits"]] <- max(0, - floor(log10(step)))
        widget$setValue(get(name, envir=playState$env))
        gSignalConnect(widget, "value-changed",
                       updateParamValue, data=playState)
        box$packStart(widget)
        foo <- gtkToolItem()
        foo$add(box)
        return(foo)
    }
    if (is.numeric(value)) {
        if (length(value) == 1) {
            ## entry coercing to numeric
            box <- gtkVBox()
            box$packStart(gtkLabel(label))
            widget <- gtkEntry()
            widget[["text"]] <- toString(get(name, envir=playState$env))
            widget["width-chars"] <- 6
            gSignalConnect(widget, "activate",
                           updateParamTextNumeric, data=playState)
            box$packStart(widget)
            foo <- gtkToolItem()
            foo$add(box)
            return(foo)
            ## only one value given -- make up a range
#            range <- 10 * (1 + abs(value)) ^ 2 * c(-1, 1)
#            step <- 10^round(log10(abs(value))-1)
#            if (value == 0) step <- 1
        }
        ## scale (slider)
        if (length(value) == 2) {
            ## range given -- make up a step
            range <- range(value)
            step <- 10^round(log10(diff(range))-1)
        } else {
            ## explicit sequence given (but might not be regular)
            range <- range(value)
            step <- median(abs(diff(sort(value))))
        }
        digits <- max(0, - floor(log10(step)))
        box <- if (horizontal) gtkHBox() else gtkVBox()
        box$packStart(gtkLabel(label), expand=FALSE)
        widget <- if (horizontal)
            gtkHScale(min=min(range), max=max(range), step=step)
        else gtkVScale(min=min(range), max=max(range), step=step)
        widget$setValue(get(name, envir=playState$env))
        widget[["digits"]] <- digits
        widget[["update-policy"]] <- GtkUpdateType["discontinuous"]
        gSignalConnect(widget, "value-changed",
                       updateParamValue, data=playState)
        box$packStart(widget)
        foo <- gtkToolItem()
        foo$setExpand(TRUE)
        foo$add(box)
        return(foo)
    }
    if (is.character(value)) {
        box <- gtkVBox()
        box$packStart(gtkLabel(label))
        if (length(value) == 1) {
            ## text entry
            widget <- gtkEntry()
            widget[["text"]] <- get(name, envir=playState$env)
            #widget["width-chars"] <- 30
            gSignalConnect(widget, "activate",
                           updateParamText, data=playState)
        } else {
            ## combo box
            widget <- gtkComboBoxEntryNewText()
            widget$show()
            for (item in value)
                widget$appendText(item)
            #widget$setActiveText(get(name, envir=playState$env))
            index <- match(get(name, envir=playState$env), value)
            if (is.na(index)) index <- 1
            widget[["active"]] <- (index - 1)
            #gSignalConnect(widget, "editing-done",
            gSignalConnect(widget$getChild(), "activate",
                           updateParamText, data=playState)
            gSignalConnect(widget, "changed",
                           updateParamCombobox, data=playState)
        }
        box$packStart(widget)
        foo <- gtkToolItem()
        foo$add(box)
        return(foo)
    }
    if (is.logical(value)) {
        ## toggle button / checkbox
        widget <- gtkCheckButton(label)
        widget[["active"]] <- isTRUE(get(name, envir=playState$env))
        gSignalConnect(widget, "clicked",
                       updateParamActive, data=playState)
        foo <- gtkToolItem()
        foo$add(widget)
        return(foo)
    }
    ## otherwise...
    stop("do not know about ",
         toString(class(value)), " objects")
}
