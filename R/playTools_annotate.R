## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### ANNOTATE

toolConstructors$annotate <- function(playState)
{
    quickTool(playState,
              label = "Annotate",
              icon = "gtk-italic",
              tooltip = "Add your own labels to the plot",
              f = annotate_handler,
              post.plot.action = annotate_postplot_action)
}

annotate_handler <- function(widget, playState)
{
    pageAnnotation <- identical(playState$annotation.mode, "page")
    foo <- playRectInput(playState, prompt=
                         "Click or drag to place an annotation. (Right-click to cancel)")
    if (is.null(foo)) return()
    if (is.null(foo$coords)) pageAnnotation <- TRUE
    space <- foo$space
    if (pageAnnotation) space <- "page"
    myXY <- if (space == "page") foo$ndc else foo$coords
    myXY$x <- signif(myXY$x, 8)
    myXY$y <- signif(myXY$y, 8)
    if (foo$is.click) {
        myX <- myXY$x[1]
        myY <- myXY$y[1]
    }

    playFreezeGUI(playState)
    on.exit(playThawGUI(playState))

    ## pop up dialog to create label
    dialog <- gwindow(title="New annotation")
    wingroup <- ggroup(horizontal=FALSE, container=dialog)
    wid <- list()

    ## TEXT AND JUST
    labgroup <- gframe("Label text",
                       horizontal=FALSE, container=wingroup)
    lay <- glayout(container=labgroup, spacing=2)
    wid$label <- gtext(width=200, height=50)
    wid$label.expr <- gcheckbox("plotmath")
    lay[1,1:2] <- wid$label
    lay[2,1] <- wid$label.expr
    lay[3,1] <- if (foo$is.click) "Position of text \nrelative to click:"
    else "Justification of \ntext inside box:"
    justgroup <- ggroup(horizontal=FALSE)
    wid$hjust <- gradio(c("left", "centre", "right"), selected=2, horizontal=TRUE)
    wid$vjust <- gradio(c("top", "centre", "bottom"), selected=2, horizontal=TRUE)
    add(justgroup, wid$hjust)
    add(justgroup, wid$vjust)
    lay[3,2] <- justgroup
    lay[4,1] <- if (foo$is.click) "Offset from point (chars):"
    else "Offset from box edge"
    wid$offset <- gedit("0", width=5, coerce.with=as.numeric)
    lay[4,2] <- wid$offset
    if (!foo$is.click) {
        ## it was a drag, defining a rectangle
        ## option to draw box border
        wid$drawbox <- gcheckbox("Draw box")
        lay[5,1:2] <- wid$drawbox
        ## TODO: fit to box?
    }
    visible(lay) <- TRUE
    focus(wid$label) <- TRUE

    ## STYLE
    stylegroup <- gframe("Style",
                         horizontal=FALSE, container=wingroup)
    lay <- glayout(container=stylegroup, spacing=2)
    refStyle <- playState$label.style
    if (is.null(playState$label.style)) {
        ## default style is taken from lattice settings
        refStyle <- do.call(gpar, trellis.par.get("add.text"))
    }
    ## col
    colList <- c(palette(), trellis.par.get("superpose.symbol")$col)
    wid$col <- gdroplist(colList, selected=0, editable=TRUE)
    wid$alpha <- gspinbutton(value=1, from=0, to=1, by=0.05, digits=2)
    if (!is.null(refStyle$col)) svalue(wid$col) <- refStyle$col
    if (!is.null(refStyle$alpha)) svalue(wid$alpha) <- refStyle$alpha
    lay[1,1] <- "Text color:"
    lay[1,2] <- wid$col
    lay[2,1] <- "Alpha (opacity):"
    lay[2,2] <- wid$alpha
    ## cex, fontsize
    wid$cex <- gedit("1.0", width=5, coerce.with=as.numeric)
    wid$fontsize <- gedit("", width=5, coerce.with=as.numeric)
    if (!is.null(refStyle$cex)) svalue(wid$cex) <- refStyle$cex
    if (!is.null(refStyle$fontsize)) svalue(wid$fontsize) <- refStyle$fontsize
    lay[3,1] <- "Expansion factor:"
    lay[3,2] <- wid$cex
    lay[4,1] <- "Font size, points:"
    lay[4,2] <- wid$fontsize
    ## fontfamily, fontface
    familyList <- c("serif", "sans", "mono", "symbol",
                    "HersheySerif", "HersheySans", "HersheyScript",
                    "HersheyGothicEnglish", "HersheyGothicGerman", "HersheyGothicItalian",
                    "HersheySymbol", "HersheySansSymbol")
    faceList <- c("plain", "bold", "italic", "bold.italic",
                  "cyrillic", "cyrillic.oblique", "EUC")
    wid$fontfamily <- gdroplist(familyList, selected=0)
    wid$fontface <- gdroplist(faceList, selected=0)
    if (!is.null(refStyle$fontfamily)) svalue(wid$fontfamily) <- refStyle$fontfamily
    if (!is.null(refStyle$fontface)) svalue(wid$fontface) <- refStyle$fontface
    lay[5,1] <- "Font family:"
    lay[5,2] <- wid$fontfamily
    lay[6,1] <- "Font face:"
    lay[6,2] <- wid$fontface
    ## lineheight (as multiple of text height)
    wid$lineheight <- gedit("1.0", width=5, coerce.with=as.numeric)
    if (!is.null(refStyle$lineheight)) svalue(wid$lineheight) <- refStyle$lineheight
    lay[7,1] <- "Line height factor:"
    lay[7,2] <- wid$lineheight
    ## rot (rotation angle)
    wid$rot <- gdroplist(c("-90","-45","-30","0","30","45","90"), selected=4,
                         editable=TRUE, coerce.with=as.numeric)
    lay[8,1] <- "Rotation angle:"
    lay[8,2] <- wid$rot
    visible(lay) <- TRUE

    ## OPTIONS
                                        #optionsgroup <- gframe("Options", horizontal=FALSE, container=wingroup)
    ## TODO: option to set as default style

    showingPreview <- FALSE

    annot_handler <- function(h, ...)
    {
        ## note: playState is accessed from the function environment!

        ## TEXT AND JUST
        argExpr <- function(wid, expr.wid) {
            newVal <- svalue(wid)
            if (newVal == "") return(NULL)
            if (svalue(expr.wid))
                newVal <- parse(text=newVal, srcfile=NULL)
            newVal
        }
        isExpr <- svalue(wid$label.expr)
        labelVal <- argExpr(wid$label, wid$label.expr)
        if (!isExpr) {
            labelVal <- gsub("\\", "\\\\", labelVal, fixed=TRUE)
        }
        just <- c(svalue(wid$hjust), svalue(wid$vjust))

        ## COORDINATES
        if (foo$is.click) {
            ## justification flipped -- at point rather than inside rect
            just[1] <- switch(just[1],
                              left="right", right="left", centre="centre")
            just[2] <- switch(just[2],
                              top="bottom", bottom="top", centre="centre")
        }
        else {
            ## it was a drag, defining a rectangle
            ## choose side of rect to align to
            x.leftright <- sort(myXY$x)
            if (is.unsorted(rawXLim(playState, space=space)))
                x.leftright <- rev(x.leftright)
            y.bottop <- sort(myXY$y)
            if (is.unsorted(rawYLim(playState, space=space)))
                y.bottop <- rev(y.bottop)
            myX <- switch(just[1],
                          left=x.leftright[1],
                          right=x.leftright[2],
                          centre=mean(x.leftright))
            myY <- switch(just[2],
                          bottom=y.bottop[1],
                          top=y.bottop[2],
                          centre=mean(y.bottop))
        }
        if ((svalue(wid$offset) != 0) && any(just != "centre")) {
            if (just[1] != "centre") {
                myPad <- svalue(wid$offset)
                if (just[1] == "right") myPad <- 0 - myPad
                myX <- substitute(unit(x, "native") + unit(pad, "char"),
                                  list(x=myX, pad=myPad))
            }
            if (just[2] != "centre") {
                myPad <- svalue(wid$offset)
                if (just[2] == "top") myPad <- 0 - myPad
                myY <- substitute(unit(y, "native") + unit(pad, "char"),
                                  list(y=myY, pad=myPad))
            }
        }

        ## STYLE
                                        #print("cex")
                                        #str(svalue(wid$cex))
                                        #print("col")
                                        #str(svalue(wid$col))
                                        #print("family")
                                        #str(svalue(wid$fontfamily))
                                        #str(svalue(wid$fontfamily, index=TRUE))
        hasCol <- (svalue(wid$col) != "")
        hasAlpha <- (svalue(wid$alpha) != 1)
        hasCex <- (svalue(wid$cex) != 1.0)
        hasSize <- (!is.na(svalue(wid$fontsize)))
        hasFamily <- !is.null(svalue(wid$fontfamily)) && !isExpr
        hasFace <- !is.null(svalue(wid$fontface)) && !isExpr
        hasHeight <- (svalue(wid$lineheight) != 1.0)
        hasRot <- (svalue(wid$rot) != 0)
                                        #gpc <- refStyle
                                        #if (inherits(gpc, "gpar"))
                                        #	gpc <- as.call(c(quote(gpar), gpc))
        gpc <- call("gpar")
        if (hasCol) gpc$col <- svalue(wid$col)
        if (hasAlpha) gpc$alpha <- svalue(wid$alpha)
        if (hasCex) gpc$cex <- svalue(wid$cex)
        if (hasSize) gpc$fontsize <- svalue(wid$fontsize)
        if (hasFamily) gpc$fontfamily <- svalue(wid$fontfamily)
        if (hasFace) gpc$fontface <- svalue(wid$fontface)
        if (hasHeight) gpc$lineheight <- svalue(wid$lineheight)

        ## CREATE THE CALL
        annot <- call("grid.text", labelVal, x=myX, y=myY)
        if (!all(just == "centre")) annot$just <- just
        if (space != "page") annot$default.units <- "native"
        annot$gp <- if (length(gpc) > 1) gpc
        if (hasRot) annot$rot <- svalue(wid$rot)

        ## add box?
        if (!foo$is.click && svalue(wid$drawbox)) {
            dobox <- call("grid.rect", x=mean(myXY$x), y=mean(myXY$y),
                          width=abs(diff(myXY$x)), height=abs(diff(myXY$y)))
            if (space != "page") dobox$default.units <- "native"
            annot <- call("{", dobox, annot)
        }

        if (showingPreview) {
            ## handle basic device mode: can't replot
            if ((length(playState$call) == 1) &&
                identical(playState$call[[1]], quote(`{`))) {
                ## do not know the call; it cannot be redrawn
                playRefresh(playState)
            }
            else playReplot(playState)
        }

        if (h$action == "preview") {
            print(annot)
                                        # preview only (refresh, grid.draw)
                                        #annot$name <- "tmp.preview"
                                        # refresh to remove any old preview
                                        #grid.refresh()
                                        #playRefresh(playState)
                                        # draw it
                                        # engine.display.list / grid.display.list
                                        #foo <- grid.grabExpr(eval(annot))
                                        #foo2 <- quote(grid.draw(foo, recording=FALSE))
                                        #foo2 <- quote(grid:::drawGrob(foo))
            playDo(playState, eval(annot), space=space,
                   clip.off=identical(playState$clip.annotations, FALSE))
                                        # and remove without redraw
                                        #grid.gremove("tmp.preview", redraw=FALSE)
            showingPreview <<- TRUE
            return()
        }
        ## draw it
        playDo(playState, eval(annot), space=space,
               clip.off=identical(playState$clip.annotations, FALSE))
        ## store it
        playState$annotations[[space]] <-
            c(playState$annotations[[space]], annot)
        ## update other tool states
        with(playState$tools, {
            if (exists("edit.annotations", inherits=F))
                edit.annotations["visible"] <- TRUE
            if (exists("clear", inherits=F))
                clear["visible"] <- TRUE
        })

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
        if (showingPreview) playReplot(playState); dispose(h$obj) },
                       container=buttgroup)
    size(okbutt) <- size(prebutt) <- size(canbutt) <- c(80, 30)

    return()
}

annotate_postplot_action <- function(widget, playState)
{
    ## draw annotations
    for (space in names(playState$annotations)) {
        playDo(playState,
               lapply(playState$annotations[[space]], eval),
               space=space,
               clip.off=identical(playState$clip.annotations, FALSE))
    }
}
