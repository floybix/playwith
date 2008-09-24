## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

initClickActions <- function(playState)
{
    ## add click handler to device
    if (is.null(playState$widgets$buttonPressSignal)) {
        playState$widgets$buttonPressSignal <-
            gSignalConnect(playState$widgets$drawingArea,
                           "button-press-event",
                           device.click_handler, data=playState)
    }
    if (!is.null(playState$click.mode)) {
        ## set initial click.mode
        vals <- clickModeValues()
        val <- vals[[playState$click.mode]]
        if (!is.null(val)) {
            aGroup <- playState$actionGroups[["PlotActions"]]
            aGroup$getAction("Zoom")$setCurrentValue(val)
        }
        playState$click.mode <- NULL
    }
}

clickmode.change_handler <- function(action, current, playState)
{
    if (!current["active"]) return()
    playState$tmp$click.mode <- gtkActionGetName(current)
    updateClickActions(playState)
}

updateClickActions <- function(playState)
{
    if (is.null(playState$tmp$click.mode))
        playState$tmp$click.mode <- "Zoom"
    curs <- switch(playState$tmp$click.mode,
                   Zoom = "crosshair",
                   Identify = "hand1",
                   Brush = "circle",
                   Annotation = "xterm",
                   Arrow = "left_ptr")
    cursor <- gdkCursorNew(GdkCursorType[curs])
    playState$widgets$drawingArea$getWindow()$setCursor(cursor)
    ## work out which actions are possible on current plot
    hasArgs <- playState$accepts.arguments
    isLatt <- playState$is.lattice
    isSplom <- (playState$callName %in% c("splom"))
    isLatt3D <- isLatt && !is.null(playState$trellis$panel.args.common$scales.3d)
    hasPanels <- isLatt && (length(trellis.currentLayout()) > 1)
    isBase <- !isLatt && is.null(playState$viewport)
    isBaseMulti <- isBase && any(par("mfrow") > 1)
    canIdent <- playState$tmp$identify.ok
    actions <- list()
    actions$nav2D <- (hasArgs && !isLatt3D && !isSplom)
    actions$nav3D <- (isLatt3D)
    actions$ident <- (canIdent)
    playState$tmp$ok.actions <- actions
    ## set default statusbar message
    modeOK <- playState$tmp$click.mode
    if ((modeOK == "Zoom") && actions$nav3D)
        modeOK <- "Nav3D"
    if ((modeOK == "Zoom") && !actions$nav2D)
        modeOK <- "Coords"
    if ((modeOK == "Identify") && !actions$ident)
        modeOK <- "Coords"
    msg <- switch(modeOK,
                  Zoom = paste("Click for coordinates",
                  "Drag to zoom (hold Shift to constrain)", sep = ", "),
                  Nav3D = "Drag to rotate (hold Shift to constrain)",
                  Identify = "Click or drag to identify points",
                  Brush = paste("Click or drag to brush points",
                  "(hold Shift to constrain)"),
                  Annotation = "Click or drag to place text",
                  Arrow = paste("Drag to draw an arrow",
                  "(hold Shift to constrain)"),
                  Coords = "Click for coordinates") ## fallback
    ## Zoom actions are always accessible, if possible:
    if (actions$nav2D || actions$nav3D) {
        if (modeOK != "Zoom")
            msg <- paste(msg, "Alt-drag to zoom", sep = ", ")
        msg <- paste(msg, "Alt-click to zoom out", sep = ", ")
    }
    msg <- paste(msg, "Right-click for more", sep = ", ")
    playState$widgets$statusbar$pop(1)
    playState$widgets$statusbar$push(1, msg)
}

device.click_handler <- function(widget, event, playState)
{
    if (!isTRUE(playState$tmp$plot.ready)) return(FALSE)
    ## bail out if another tool is handling the click (this ok?)
    if (isTRUE(playState$tmp$now.interacting)) return(FALSE)
    if (playState$tmp$need.reconfig) generateSpaces(playState)
    x <- event$x
    y <- event$y
    ## work out which actions are relevant to the plot
    actions <- playState$tmp$ok.actions
    modeOK <- playState$tmp$click.mode
    if ((modeOK == "Zoom") && !actions$nav2D && !actions$nav3D)
        modeOK <- "Coords"
    if ((modeOK == "Identify") && !actions$ident)
        modeOK <- "Coords"
    pageOK <- (modeOK %in% c("Annotation", "Arrow"))
    isCtrlClick <- (as.flag(event$state) & GdkModifierType["control-mask"])
    isAltClick <- ((as.flag(event$state) & GdkModifierType["mod1-mask"]) ||
                   (as.flag(event$state) & GdkModifierType["mod2-mask"]))
    isShiftClick <- (as.flag(event$state) & GdkModifierType["shift-mask"])
    isPlainClick <- !isCtrlClick && !isAltClick && !isShiftClick
    ## take actions
    if ((event$button == 1) && !isCtrlClick) {
        ## standard (left) mouse button
        dragShape <- "rect"
        if (!isAltClick) {
            if ((modeOK == "Zoom") && actions$nav3D)
                dragShape <- "line"
            if (modeOK == "Arrow")
                dragShape <- "line"
        }
        ## handle click or drag
        foo <- playClickOrDrag(playState, x0=x, y0=y,
                               shape=dragShape)
        if (is.null(foo)) {
            ## drag went off device
            coordsCore(playState, NULL)
            return(FALSE)
        }
        if (is.null(foo$coords) && !pageOK) {
            ## click/drag outside of a defined space
            coordsCore(playState, NULL)
            return(FALSE)
        }
        if (foo$is.click) {
            coordsCore(playState, foo)
        } else {
            coordsCore(playState, NULL)
        }
        ## standard alt-click actions
        if (isAltClick) {
            if (actions$nav2D) {
                ## 2D Zoom
                if (foo$is.click) {
                    zoomoutCore(playState, foo)
                } else {
                    ## drag
                    zoomCore(playState, foo)
                }
            }
            if (actions$nav3D) {
                ## 3D Zoom
                if (foo$is.click) {
                    zoomout3DCore(playState, foo)
                } else {
                    ## drag
                    zoom3DCore(playState, foo)
                }
            }
        } else {
            ## plain click: normal actions
            if (modeOK == "Zoom") {
                if (actions$nav3D) {
                    ## Nav3D
                    if (!foo$is.click)
                        rotate3DCore(playState, foo)
                } else {
                    ## Zoom (2D)
                    if (!foo$is.click)
                        zoomCore(playState, foo)
                }
            }
            if (modeOK == "Identify") {
                identifyCore(playState, foo, remove = isShiftClick)
            }
            if (modeOK == "Brush") {
                brushCore(playState, foo, remove = isShiftClick)
            }
            if (modeOK == "Annotation") {
                annotateCore(playState, foo)
            }
            if (modeOK == "Arrow") {
                arrowCore(playState, foo)
            }
        }
    } else {
        coordsCore(playState, NULL)
    }
    if (event$button == 2) {
        ## middle mouse button click: zoom to fit
        if (actions$nav2D || actions$nav3D)
            zoomfit_handler(NULL, playState)
    }
    if ((event$button == 3) || isCtrlClick) {
        ## right mouse button or control-click
        foo <- playClickOrDrag(playState, x0=x, y0=y,
                               shape="rect")
        if (is.null(foo))
            return(FALSE)
        ## pop up context menu
        contextCore(playState, foo, event = event)
    }
    return(FALSE)
}

coordsCore <- function(playState, foo) {
    coords <- foo$coords
    ## convert from log scale if necessary
    coords <- spaceCoordsToDataCoords(playState, coords)
    if (!is.null(coords)) {
        x <- format(coords$x, digits=3)
        y <- format(coords$y, digits=3)
        coordsTxt <- paste("<tt>x:", x, ", y:", y, " </tt>", sep="")
        playState$widgets$coordsLabel$setMarkup(coordsTxt)
        playState$widgets$coordsLabel["visible"] <- TRUE
    } else {
        playState$widgets$coordsLabel["visible"] <- FALSE
    }
}

zoomCore <- function(playState, foo)
{
    xlim <- range(foo$coords$x)
    ylim <- range(foo$coords$y)
    ## reverse axis scales if needed
    if (!foo$yOnly) {
        if (is.unsorted(rawXLim(playState, space=foo$space)))
            xlim <- rev(xlim)
        rawXLim(playState) <- xlim
    }
    if (!foo$xOnly) {
        if (is.unsorted(rawYLim(playState, space=foo$space)))
            ylim <- rev(ylim)
        rawYLim(playState) <- ylim
    }
    playReplot(playState)
}

zoomoutCore <- function(playState, foo)
{
    nav.x <- !isTRUE(foo$yOnly)
    nav.y <- !isTRUE(foo$xOnly)
    ## find existing scales
    xlim <- rawXLim(playState, space=foo$space)
    ylim <- rawYLim(playState, space=foo$space)
    ## centre on click location
    xlim <- (xlim - mean(xlim)) + mean(foo$coords$x)
    ylim <- (ylim - mean(ylim)) + mean(foo$coords$y)
    ## zoom out: make range twice the size
    if (nav.x) xlim <- xlim + diff(xlim) * c(-0.5, 0.5)
    if (nav.y) ylim <- ylim + diff(ylim) * c(-0.5, 0.5)
    ## this converts from raw numeric to original format (including unlog)
    if (nav.x) rawXLim(playState) <- xlim
    if (nav.y) rawYLim(playState) <- ylim
    playReplot(playState)
}

zoom3DCore <- function(playState, foo)
{
    ## work out zoom factor by size of drag
    ## ideally this would set xlim/ylim/zlim to drag region?
    xlim <- rawXLim(playState)
    ylim <- rawYLim(playState)
    xfactor <- abs(diff(foo$coords$x)) / abs(diff(xlim))
    yfactor <- abs(diff(foo$coords$y)) / abs(diff(ylim))
    zoomfactor <- 1 / max(xfactor, yfactor)
    zoom <- callArg(playState, "zoom")
    if (is.null(zoom)) zoom <- 0.8
    callArg(playState, "zoom") <- signif(zoom * zoomfactor, 3)
    playReplot(playState)
}

zoomout3DCore <- function(playState, foo)
{
    zoom <- callArg(playState, "zoom")
    if (is.null(zoom)) zoom <- 0.8
    callArg(playState, "zoom") <- signif(zoom / 1.25, 3)
    playReplot(playState)
}

rotate3DCore <- function(playState, foo)
{
    ## work out current viewpoint (rotation)
    screen <- callArg(playState, "screen")
    if (is.null(screen)) screen <- list(z=40, x=-60)
    R.mat <- callArg(playState, "R.mat")
    if (is.null(R.mat)) R.mat <- diag(4)
    ## incorporate existing 'screen' into existing 'R.mat'
    R.mat <- ltransform3dMatrix(screen, R.mat)
    ## apply rotation defined by drag (direction and length)
    ## drag down corresponds to a positive 'x' arg
    ## drag right corresponds to a positive 'y' arg
    ## drag anticlockwise (in corner) corresponds to a positive 'z' arg
    pan.x <- rawXLim(playState)
    pan.y <- rawYLim(playState)
    x <- foo$coords$x
    y <- foo$coords$y
    xdelta <- diff(x)
    ydelta <- diff(y)
    angle <- atan2(y[2], x[2]) - atan2(y[1], x[1])
    dist <- c(max(abs(x/pan.x)[1], abs(y/pan.y)[1]),
              max(abs(x/pan.x)[2], abs(y/pan.y)[2]))
    ## TODO: avoid threshold for changing behaviour -- should be gradual
    if ((abs(angle) < pi/2) && all(dist > 0.7)) {
        rot <- list(z = 180 * angle / (2*pi))
    } else {
        ## TODO: should normalise by panel limits?
        rot <- list(y = xdelta * 180,
                    x = -ydelta * 180)
    }
    R.mat <- ltransform3dMatrix(rot, R.mat)
    R.mat <- round(R.mat, digits = 3)
    callArg(playState, "R.mat") <- call("matrix", c(R.mat), nc = 4)
    callArg(playState, "screen") <- list() ## replace default
    ## keep 3D scales on the same axes
    ## (it is confusing if they switch while rotating)
    ## ## no, bad for long axis labels
    #callArg(playState, "scpos") <- list(x = 1, y = 8, z = 4)
    playReplot(playState)
}

contextCore <- function(playState, foo, event)
{
    ## pop up context menu
    cMenu <- gtkMenu()
    cMenu$popup(button = event$button, activate.time = event$time)
    cMenu["visible"] <- FALSE
    ## fill in menu items
    showGeneralStuff <- TRUE
    if (foo$space != "page") {
        foo$is.click <- TRUE
        foo <- playSelectData(playState, foo = foo)
        id <- foo$subscripts
        if (length(id) > 0) {
            ## clicked on a data point, don't show general stuff
            showGeneralStuff <- FALSE
            x <- toString(foo$x, width = 30)
            y <- toString(foo$y, width = 30)
            item <- gtkMenuItem(paste("x:", x))
            item["sensitive"] <- FALSE
            cMenu$append(item)
            item <- gtkMenuItem(paste("y:", y))
            item["sensitive"] <- FALSE
            cMenu$append(item)
            dat <- getDataArg(playState)
            if (!is.null(dat)) {
                rn <- if (.row_names_info(dat) <= 0)
                    NULL else row.names(dat)
                if (!is.null(rn)) {
                    txt <- toString(rn[id], width = 30)
                    item <- gtkMenuItem(paste("row:", txt))
                    item["sensitive"] <- FALSE
                    cMenu$append(item)
                }
                cn <- colnames(dat)
                for (i in seq_along(cn)) {
                    txt <- toString(paste(cn[i], dat[id, i]),
                                    width = 30)
                    item <- gtkMenuItem(txt)
                    item["sensitive"] <- FALSE
                    cMenu$append(item)
                }
            }
            cMenu$append(gtkSeparatorMenuItem())
            aGroup <- playState$actionGroups[["PlotActions"]]
            cMenu$append(aGroup$getAction("SetLabelsTo")$createMenuItem())
        }
    }
    if (showGeneralStuff) {
        cMenu$destroy()
        cMenu <- playState$uiManager$getWidget("/ContextMenu")
        cMenu$popup(button = event$button, activate.time = event$time)
    }
    cMenu["visible"] <- TRUE
    while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
}
