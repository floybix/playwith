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
                                     "Alt-click to zoom out",
                                     "Middle-click to reset"),
                        if (nav3D) c("Drag to rotate (hold Shift to constrain)",
                                        # "Alt-drag to zoom", "Alt-click to zoom out"
                                     "Middle-click to reset")
                        ), collapse = ", "))
    ## TODO: "Right-click for more" (context menu)
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
    isCtrlClick <- as.flag(event$state) & GdkModifierType["control-mask"]
    if ((event$button == 1) && !isCtrlClick) {
        ## standard (left) mouse button
        foo <- playClickOrDrag(playState, x0=x, y0=y, shape="rect")
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
            if (foo$modifiers & GdkModifierType["mod1-mask"]) {
                ## alt-click
                if (actions$nav2D)
                    zoomoutCore(playState, foo)
            } else {
                ## click
                if (actions$ident)
                    identifyCore(playState, foo)
            }
        }
        else {
            ## drag
            coordsCore(playState, NULL)
            if (actions$nav2D)
                zoomCore(playState, foo)
            if (actions$nav3D)
                NA ## TODO
        }
    } else {
        coordsCore(playState, NULL)
    }
    if (event$button == 2) {
        ## middle mouse button
        if (actions$nav2D || actions$nav3D)
            zoomfit_handler(NULL, playState)
    }
    if ((event$button == 3) || isCtrlClick) {
        ## right mouse button or control-click
        ## TODO: context menu
        #space <- whichSpace(playState, x, y)
        #if (space != "page") {
        #    coords <- deviceCoordsToSpace(playState, x, y, space=space)
        #    foo <- list(coords=coords, space=space)
        #    zoomoutCore(playState, foo)
        #}
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
