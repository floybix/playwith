## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

initClickActions <- function(playState)
{
    if (is.null(playState$widgets$buttonPressSignal)) {
        playState$widgets$buttonPressSignal <-
            gSignalConnect(playState$widgets$drawingArea,
                           "button-press-event",
                           device.click_handler, data=playState)
    }
}

updateClickActions <- function(playState)
{
    ## work out which actions are relevant to the plot
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
    playState$tmp$click.actions <- actions
    ## set default statusbar message
    msg <- with(actions,
                paste(c(if (ident) c("Click to identify points"),
                        if (nav2D) c("Drag to zoom (hold Shift to constrain)",
                                     "Alt-click to zoom out"),
                        if (nav3D) c("Drag to rotate (hold Shift to constrain)",
                                     "Alt-drag to zoom", "Alt-click to zoom out"),
                        "Right-click for more"),
                      collapse = ", "))
    playState$widgets$statusbar$pop(1)
    playState$widgets$statusbar$push(1, msg)
}

device.click_handler <- function(widget, event, playState)
{
    if (!isTRUE(playState$plot.ready)) return(FALSE)
    ## bail out if another tool is handling the click (this ok?)
    if (isTRUE(playState$tmp$now.interacting)) return(FALSE)
    if (playState$tmp$need.reconfig) generateSpaces(playState)
    x <- event$x
    y <- event$y
    ## work out which actions are relevant to the plot
    actions <- playState$tmp$click.actions
    isCtrlClick <- (as.flag(event$state) & GdkModifierType["control-mask"])
    isAltClick <- ((as.flag(event$state) & GdkModifierType["mod1-mask"]) ||
                   (as.flag(event$state) & GdkModifierType["mod2-mask"]))
    isShiftClick <- (as.flag(event$state) & GdkModifierType["shift-mask"])
    isPlainClick <- !isCtrlClick && !isAltClick && !isShiftClick
    ## take actions
    if ((event$button == 1) && !isCtrlClick) {
        ## standard (left) mouse button
        dragShape <- "rect"
        if (actions$nav3D && !isAltClick)
            dragShape <- "line"
        ## handle click or drag
        foo <- playClickOrDrag(playState, x0=x, y0=y,
                               shape=dragShape)
        if (is.null(foo)) {
            ## drag went off device
            coordsCore(playState, NULL)
            return(FALSE)
        }
        if (is.null(foo$coords)) {
            ## click/drag outside of a defined space
            coordsCore(playState, NULL)
            return(FALSE)
        }
        if (foo$is.click) {
            ## click, not drag
            coordsCore(playState, foo)
            if (actions$nav2D) {
                if (isAltClick)
                    zoomoutCore(playState, foo)
            }
            if (actions$nav3D) {
                if (isAltClick)
                    zoomout3DCore(playState, foo)
            }
            if (actions$ident) {
                if (isShiftClick)
                    identifyCore(playState, foo, deidentify=TRUE)
                if (isPlainClick)
                    identifyCore(playState, foo)
            }
        }
        else {
            ## drag
            coordsCore(playState, NULL)
            if (actions$nav2D) {
                zoomCore(playState, foo)
            }
            if (actions$nav3D) {
                if (!isAltClick)
                    rotate3DCore(playState, foo)
                if (isAltClick)
                    zoom3DCore(playState, foo)
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
        if (is.null(foo)) {
            return(FALSE)
        }
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
    space <- foo$space
    showGeneralStuff <- TRUE
    if (space != "page") {
        foo$is.click <- TRUE
        foo <- playSelectData(playState, foo = foo)
        id <- foo$subscripts
        if (length(id) > 0) {
            ## clicked on a data point, don't show general stuff
            showGeneralStuff <- FALSE
            x <- foo$x
            y <- foo$y
            ## TODO: actions for these?
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
                    item <- gtkMenuItem(rn[id])
                    item["sensitive"] <- FALSE
                    cMenu$append(item)
                }
                cn <- colnames(dat)
                for (i in seq_along(cn)) {
                    item <- gtkMenuItem(paste(cn[i], dat[id, i]))
                    item["sensitive"] <- FALSE
                    cMenu$append(item)
                }
            }
        }
    }
    if (showGeneralStuff) {
        aGroup <- playState$actionGroups[["PlotActions"]]
        item <- aGroup$getAction("PlotSettings")$createMenuItem()
        cMenu$append(item)
        aGroup <- playState$actionGroups[["GlobalActions"]]
        for (actionName in c("CustomStyle", "SetSize")) {
            item <- aGroup$getAction(actionName)$createMenuItem()
            cMenu$append(item)
        }
        cMenu$append(gtkSeparatorMenuItem())
        for (actionName in c("Back", "Forward", "Redraw")) {
            item <- aGroup$getAction(actionName)$createMenuItem()
            cMenu$append(item)
        }
        cMenu$append(gtkSeparatorMenuItem())
        for (actionName in c("ViewSource")) {
            item <- aGroup$getAction(actionName)$createMenuItem()
            cMenu$append(item)
        }
    }
    ## show the menu
    cMenu$popup(button = event$button, activate.time = event$time)
    while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
}
