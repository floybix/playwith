## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

annotationActionGroup <- function(playState)
{
    entries <-
        list( ## : name, stock icon, label, accelerator, tooltip, callback
             list("Annotation", "gtk-italic", "Annotate", NULL, "Add custom labels to the plot", annotate_handler),
             list("Arrow", "gtk-connect", "Arrow", NULL, "Add an arrow to the plot", arrow_handler),
             list("UndoAnnotation", "gtk-undo", "Undo ann.", "<Ctrl>Z", "Remove last annotation", undo.annotation_handler),
             list("Clear", "gtk-clear", "Clear", "<Shift>Delete", "Remove labels and annotations", clear_handler),
             list("EditAnnotations", "gtk-edit", "Edit ann.", "<Ctrl><Shift>E", "Edit annotations (including arrows) code", edit.annotations_handler)
             )

    ## construct action group with playState passed to callbacks
    aGroup <- gtkActionGroupNew("AnnotationActions")
    aGroup$addActions(entries, playState)
    aGroup
}


### STORE PIXBUF FOR FAST REDRAWS:
###
###    da.w <- da$getAllocation()$width
###    da.h <- da$getAllocation()$height
###    buf <- gdkPixbufGetFromDrawable(src=da$window, src.x=0, src.y=0,
###                                    dest.x=0, dest.y=0, width=da.w, height=da.h)






updateAnnotationActions <- function(playState)
{
    updateAnnotationActionStates(playState)
}

updateAnnotationActionStates <- function(playState)
{
    aGroup <- playState$actionGroups[["AnnotationActions"]]
    ## Clear
    ## in basic device mode do not know the call; it cannot be redrawn (TODO?)
    showClear <- !isBasicDeviceMode(playState)
    ## TODO: allow new tools to specify more items to clear
    showClear <- showClear && ((length(playState$ids) > 0) ||
                               (length(playState$annotations) > 0) ||
                               (length(playState$brushed) > 0))
    aGroup$getAction("Clear")$setVisible(showClear)
    ## EditAnnotations
    showEdit <- !isBasicDeviceMode(playState)
    showEdit <- showEdit && (length(playState$annotations) > 0)
    aGroup$getAction("EditAnnotations")$setVisible(showEdit)
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
        clear.types <- NULL
        widg <- gcheckboxgroup(types, checked=TRUE)
        result <- gbasicdialog(title="Clear what?", widget=widg,
                               handler=function(...)
                               clear.types <<- svalue(widg) )
        playState$win$present()
        if (!isTRUE(result)) return()
    }
    for (x in clear.types)
        playState[[x]] <- list()
    ## update other tool states
    updateAnnotationActionStates(playState)
    playReplot(playState)
}

edit.annotations_handler <- function(widget, playState)
{
    annotSpaces <- names(playState$annotations)
    if (length(annotSpaces) == 0) return()
    if (length(annotSpaces) == 1) {
        space <- annotSpaces[1]
    }
    else if (length(annotSpaces) > 1) {
        space <- select.list(annotSpaces,
                             multiple = FALSE, title = "Choose annotation space")
        playState$win$present()
        if (space == "") return()
    }
    annots <- playState$annotations[[space]]
    callTxt <- paste(unlist(lapply(annots, deparse, control="showAttributes")), collapse="\n")
    repeat {
        newTxt <- guiTextInput(callTxt, title="Edit annotations",
                               prompt="", accepts.tab=F)
        if (is.null(newTxt)) break
        callTxt <- newTxt
        tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
        ## check whether there was a syntax error
        if (inherits(tmp, "error")) {
            gmessage.error(conditionMessage(tmp))
        } else {
            playState$annotations[[space]] <- tmp
            playReplot(playState)
            break
        }
    }
    playState$win$present()
}


arrow_handler <- function(widget, playState)
{
    pageAnnotation <- identical(playState$annotation.mode, "page")
    foo <- playLineInput(playState, prompt=
                         "Click and drag to draw an arrow. (Right-click to cancel)")
    if (is.null(foo)) return()
    if (is.null(foo$coords)) pageAnnotation <- TRUE
    if (foo$is.click) return()
    space <- foo$space
    if (pageAnnotation) space <- "page"
    myXY <- if (space == "page") foo$ndc else foo$coords
    myXY$x <- signif(myXY$x, 8)
    myXY$y <- signif(myXY$y, 8)
    annot <- call("grid.lines", x=myXY$x, y=myXY$y)
    if (space != "page") annot$default.units <- "native"
    annot$arrow <- playState$arrow.arrow
    style <- eval(playState$arrow.style)
    if (inherits(style, "gpar"))
        style <- as.call(c(quote(gpar), style))
    if (is.null(playState$arrow.style)) {
        ## default style is taken (at plot time) from lattice settings
        style <- quote(do.call(gpar, trellis.par.get("add.line")))
    }
    if (!is.null(style)) annot$gp <- style
    originalPlot <- if (isBasicDeviceMode(playState))
        try(recordPlot())
    ## draw it
    playDo(playState, eval(annot), space=space,
           clip.off=identical(playState$clip.annotations, FALSE))
    ## store it
    playState$annotations[[space]] <-
        c(playState$annotations[[space]], annot)
    if (isBasicDeviceMode(playState))
        playState$.recorded.plot <- originalPlot
    ## update other tool states
    with(playState$tools, {
        if (exists("clear", inherits=F))
            clear["visible"] <- TRUE
        if (exists("edit.annotations", inherits=F))
            edit.annotations["visible"] <- TRUE
        if (exists("undo.annotation", inherits=F))
            undo.annotation["visible"] <- TRUE
    })
}
