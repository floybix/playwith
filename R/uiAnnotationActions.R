## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

annotationActionGroup <- function(playState)
{
    entries <-
        list( ## : name, stock icon, label, accelerator, tooltip, callback
             list("Annotation", "gtk-italic", "Annotate", "<Ctrl>A", "Add custom labels to the plot", annotate_handler),
             list("Arrow", "gtk-connect", "Arrow", "<Ctrl><Shift>A", "Add an arrow to the plot", arrow_handler),
             list("Legend", "gtk-sort-ascending", "Legend", NULL, "Place a legend", legend_handler),
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
    ## draw annotations
    for (space in names(playState$annotations)) {
        playDo(playState,
               lapply(playState$annotations[[space]], eval),
               space = space)
    }
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

undo.annotation_handler <- function(widget, playState)
{
    ## TODO
    stop("not yet implemented")
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
    pageAnnotation <- isTRUE(playState$page.annotation)
    foo <- playLineInput(playState, prompt = paste(
                                    "Click and drag to draw an arrow;",
                                    "Right-click or Esc to cancel."))
    if (is.null(foo)) return()
    if (is.null(foo$coords)) pageAnnotation <- TRUE
    if (foo$is.click) return()
    space <- foo$space
    if (pageAnnotation) space <- "page"
    myXY <- if (space == "page") foo$ndc else foo$coords
    myXY$x <- signif(myXY$x, 7)
    myXY$y <- signif(myXY$y, 7)
    annot <- call("panel.arrows", myXY$x[1], myXY$y[1],
                  myXY$x[2], myXY$y[2])
    arrow <- playState$arrow
    ## each of these may be NULL
    annot$angle <- arrow$angle
    annot$length <- arrow$length
    annot$unit <- arrow$unit
    annot$type <- arrow$type
    annot$ends <- arrow$ends
    annot$code <- arrow$code

    ## TODO: delete this?
    originalPlot <- if (isBasicDeviceMode(playState))
        try(recordPlot())
    ## draw it
    playDo(playState, eval(annot), space=space)
    ## store it
    playState$annotations[[space]] <-
        c(playState$annotations[[space]], annot)
    if (isBasicDeviceMode(playState))
        playState$.recorded.plot <- originalPlot
    ## update other tool states
    updateAnnotationActionStates(playState)
}

annotate_handler <- function(widget, playState)
{
    pageAnnotation <- isTRUE(playState$page.annotation)
    foo <- playRectInput(playState, prompt = paste(
                                    "Click or drag to place text annotation;",
                                    "Right-click or Esc to cancel."))
    if (is.null(foo)) return()
    if (is.null(foo$coords)) pageAnnotation <- TRUE
    space <- foo$space
    if (pageAnnotation) space <- "page"
    absXY <- foo$ndc
    myXY <- if (space == "page") foo$dc else foo$coords
    myXY$x <- signif(myXY$x, 7)
    myXY$y <- signif(myXY$y, 7)
    if (foo$is.click) {
        myX <- myXY$x[1]
        myY <- myXY$y[1]
    }

    playFreezeGUI(playState)
    on.exit(playThawGUI(playState))

    ## pop up dialog to create label
    dialog <- gwindow(title = "New annotation")
    wingroup <- ggroup(horizontal = FALSE, container = dialog)
    wid <- list()

    ## TEXT AND JUST
    labgroup <- gframe("Label text and position",
                       horizontal = FALSE, container = wingroup)
    wid$label <- gtext(width = 200, height = 50, container = labgroup)
    wid$label.expr <- gcheckbox("plotmath", container = labgroup)
    just.hgroup <- ggroup(horizontal = TRUE, container = labgroup)
    glabel(if (foo$is.click) "Position of text \n relative to click:" else
           "Justification of \n text inside box:", container = just.hgroup)
    ## this grid of checkboxes is conceptually a radio button group
    justw <- list()
    just_handler <- function(h, ...) {
        if (svalue(h$obj) == FALSE) return()
        ## turn all the others off
        for (j in names(justw)) {
            if (j != h$action)
                svalue(justw[[j]]) <- FALSE
        }
    }
    lay <- glayout(container=just.hgroup, spacing=2)
    lay[1,1] <- justw$lt <- gcheckbox("topleft",  handler = just_handler, action = "lt")
    lay[1,2] <- justw$ct <- gcheckbox("top    ",  handler = just_handler, action = "ct")
    lay[1,3] <- justw$rt <- gcheckbox("topright", handler = just_handler, action = "rt")
    lay[2,1] <- justw$lc <- gcheckbox("left   ",  handler = just_handler, action = "lc")
    lay[2,2] <- justw$cc <- gcheckbox("centre ",  handler = just_handler, action = "cc")
    lay[2,3] <- justw$rc <- gcheckbox("right  ",  handler = just_handler, action = "rc")
    lay[3,1] <- justw$lb <- gcheckbox("botleft",  handler = just_handler, action = "lb")
    lay[3,2] <- justw$cb <- gcheckbox("bottom ",  handler = just_handler, action = "cb")
    lay[3,3] <- justw$rb <- gcheckbox("botright", handler = just_handler, action = "rb")
    svalue(justw$cc) <- TRUE
    visible(lay) <- TRUE

    offsetgroup <- ggroup(horizontal = TRUE, container = labgroup)
    glabel(if (foo$is.click) "Offset from point (chars):"
    else "Offset from box edge", container = offsetgroup)
    wid$offset <- gedit(toString(playState$label.offset), width = 4,
                        coerce.with = as.numeric, container = offsetgroup)
    if (!foo$is.click) {
        ## it was a drag, defining a rectangle
        ## option to draw box border
        wid$drawbox <- gcheckbox("Draw box", container = offsetgroup)
        ## TODO: fit to box?
    }
    focus(wid$label) <- TRUE

    ## STYLE
    user.text <- trellis.par.get("user.text")
    if (is.null(user.text))
        user.text <- trellis.par.get("add.text")
    stylegroup <- gframe("Style",
                         horizontal=FALSE, container=wingroup)
    lay <- glayout(container=stylegroup, spacing=2)
    ## style settings widgets
    wid$col <- gdroplist(palette(), selected = 0, editable = TRUE)
    size(wid$col) <- c(100, -1)
    wid$cex <- gedit("1.0", width = 4, coerce.with = as.numeric)
    wid$lineheight <- gedit("1.0", width = 4, coerce.with = as.numeric)
    wid$rot <- gdroplist(c("-90","-45","-30","0","30","45","90"), selected = 4,
                         editable = TRUE, coerce.with = as.numeric)
    size(wid$rot) <- c(60, -1)
    if (!is.null(user.text$col)) svalue(wid$col) <- user.text$col
    if (!is.null(user.text$cex)) svalue(wid$cex) <- user.text$cex
    if (!is.null(user.text$lineheight))
        svalue(wid$lineheight) <- user.text$lineheight
    lay[1,1] <- "Text color:"
    lay[1,2] <- wid$col
    lay[1,3] <- " Scale:"
    lay[1,4] <- wid$cex
    lay[2,1] <- "Lineheight:"
    lay[2,2] <- wid$lineheight
    lay[2,3] <- " Rotation:"
    lay[2,4] <- wid$rot
    visible(lay) <- TRUE
    wid$set.defaults <- gcheckbox("Set as default label style",
                                  container = stylegroup)
    glabel("For more control, try Customise Style from the Style menu.",
           container = stylegroup)

    originalPlot <- try(recordPlot())
    showingPreview <- FALSE

    annot_handler <- function(h, ...)
    {
        ## note: playState is accessed from the function environment

        ## TEXT AND JUST
        argExpr <- function(wid, expr.wid) {
            newVal <- svalue(wid)
            if (newVal == "") return(NULL)
            if (svalue(expr.wid))
                newVal <- try(parse(text=newVal, srcfile=NULL))
            if (inherits(newVal, "try-error")) return(NULL)
            newVal
        }
        isExpr <- svalue(wid$label.expr)
        labelVal <- argExpr(wid$label, wid$label.expr)
        if (!isExpr) {
            labelVal <- gsub("\\", "\\\\", labelVal, fixed=TRUE)
        }
        ## find which checkbox was selected
        just <- c("c", "c")
        for (j in names(justw))
            if (svalue(justw[[j]])) just <- strsplit(j, NULL)[[1]]
        just[1] <- switch(just[1], l="left", r="right", c="centre")
        just[2] <- switch(just[2], t="top", b="bottom", c="centre")

        ## COORDINATES
        if (foo$is.click == FALSE) {
            ## it was a drag, defining a rectangle
            ## choose side of rect to align to
            x.leftright <- myXY$x[order(absXY$x)]
            y.bottop <- myXY$y[rev(order(absXY$y))]
            myX <- switch(just[1],
                          left = x.leftright[1],
                          right = x.leftright[2],
                          mean(x.leftright))
            myY <- switch(just[2],
                          bottom = y.bottop[1],
                          top = y.bottop[2],
                          mean(y.bottop))
            ## justification flipped (inside rect, not around point)
            just[1] <- switch(just[1],
                              left="right", right="left", centre="centre")
            just[2] <- switch(just[2],
                              top="bottom", bottom="top", centre="centre")
        }
        ## have to add offset manually if text is placed on a corner
        if ((svalue(wid$offset) != 0) && all(just != "centre")) {
            pad <- svalue(wid$offset)
            pad <- playDo(playState,
                          convertWidth(unit(pad, "char"), "native", TRUE),
                          space = space)
            if (just[1] == "left") pad <- 0 - pad
            myX <- myX + pad
            pad <- svalue(wid$offset)
            pad <- playDo(playState,
                          convertHeight(unit(pad, "char"), "native", TRUE),
                          space = space)
            if (just[2] == "bottom") pad <- 0 - pad
            myY <- myY + pad
        }

        myX <- signif(myX, 7)
        myY <- signif(myY, 7)

        ## CREATE THE CALL
        annot <- call("panel.usertext", myX, myY, labelVal)
        if (!all(just == "centre")) {
            if (any(just == "centre")) {
                if ("bottom" %in% just) annot$pos <- 1
                if ("left" %in% just) annot$pos <- 2
                if ("top" %in% just) annot$pos <- 3
                if ("right" %in% just) annot$pos <- 4
                if (svalue(wid$offset) != 0.5)
                    annot$offset <- svalue(wid$offset)
            } else {
                adj <- c(0.5, 0.5)
                if ("bottom" %in% just) adj[2] <- 1
                if ("left" %in% just) adj[1] <- 1
                if ("top" %in% just) adj[2] <- 0
                if ("right" %in% just) adj[1] <- 0
                annot$adj <- adj
            }
        }
        if (svalue(wid$set.defaults)) {
            user.text$col <- svalue(wid$col)
            user.text$cex <- svalue(wid$cex)
            user.text$lineheight <- svalue(wid$lineheight)
            trellis.par.set(user.text = user.text)
        }
        if (!identical(svalue(wid$col), user.text$col))
            annot$col <- svalue(wid$col)
        if (!identical(svalue(wid$cex), user.text$cex))
            annot$cex <- svalue(wid$cex)
        if (!identical(svalue(wid$lineheight), user.text$lineheight))
            annot$lineheight <- svalue(wid$lineheight)
        if (!identical(svalue(wid$rot), 0))
            annot$srt <- svalue(wid$rot)

        ## draw box?
        if (!foo$is.click && svalue(wid$drawbox)) {
            dobox <- call("panel.rect", x = mean(myXY$x), y = mean(myXY$y),
                          width = abs(diff(myXY$x)), height = abs(diff(myXY$y)))
            annot <- call("{", dobox, annot)
        }

        ## need to redraw all annotations if style changed
        if (svalue(wid$set.defaults) &&
            (length(playState$annotations) > 0))
        {
            playReplot(playState)
            originalPlot <- try(recordPlot())
            showingPreview <<- FALSE
        }

        if (showingPreview) {
            ## remove preview (i.e. redraw original plot)
            result <- try(replayPlot(originalPlot))
            ## may fail if engine.display.list / grid.display.list off
            if (inherits(result, "try-error")) {
                playReplot(playState)
            } else {
                generateSpaces(playState)
            }
        }

        if (h$action == "preview") {
            ## echo annotation code to console
            message(paste(deparse(annot), collapse="\n"))
            ## draw it
            playDo(playState, eval(annot), space = space)
            showingPreview <<- TRUE
            return()
        }
        ## draw it
        playDo(playState, eval(annot), space=space)
        ## store it
        playState$annotations[[space]] <-
            c(playState$annotations[[space]], annot)
        if (isBasicDeviceMode(playState))
            playState$.recorded.plot <- originalPlot
        ## update other tool states
        updateAnnotationActionStates(playState)

        dispose(h$obj)
        playState$win$present()
    }

    buttgroup <- ggroup(container=wingroup)
    addSpring(buttgroup)
    okbutt <- gbutton("OK", handler=annot_handler,
                      action="ok", container=buttgroup)
    prebutt <- gbutton("Preview", handler=annot_handler,
                       action="preview", container=buttgroup)
    canbutt <- gbutton("Cancel", handler=function(h, ...) {
        if (showingPreview) {
            ## remove preview (i.e. redraw original plot)
            result <- try(replayPlot(originalPlot))
            if (inherits(result, "try-error")) {
                playReplot(playState)
            } else {
                generateSpaces(playState)
            }
        }
        dispose(h$obj)
    }, container=buttgroup)
    size(okbutt) <- size(prebutt) <- size(canbutt) <- c(80, 30)
    #defaultWidget(okbutt) <- TRUE
}


legend_handler <- function(widget, playState)
{
    ## TODO
}
