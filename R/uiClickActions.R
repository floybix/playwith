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

device.click_handler <- function(widget, event, playState)
{
    if (!isTRUE(playState$plot.ready)) return(FALSE)
    ## bail out if another tool is handling the click
    ## TODO: is this a race condition? is there a better way?
    if (isTRUE(playState$now.interacting)) return(FALSE)
    if (playState$.need.reconfig) generateSpaces(playState)
    x <- event$x
    y <- event$y
    ## work out which actions are relevant to the plot
    hasArgs <- playState$accepts.arguments
    isLatt <- playState$is.lattice
    isSplom <- (playState$callName %in% c("splom"))
    isLatt3D <- (playState$callName %in% c("cloud", "wireframe"))
    hasPanels <- isLatt && (length(trellis.currentLayout()) > 1)
    isBase <- !isLatt && is.null(playState$viewport)
    isBaseMulti <- isBase && any(par("mfrow") > 1)
    ## clear the coords label
    coordsCore(playState, NULL)
    if (event$button == 1) {
        ## standard (left) mouse button
        xonly <- playState$time.mode && is.null(playState$time.vector)
        nav.x <- TRUE
        nav.y <- !xonly
        ## TODO: in playClickOrDrag:
        ##       hold <Shift> to constrain to xonly or yonly (for rect or line)
        ##       and check return value for dimensions (of rect)
        scales <- c( if (nav.x) "x", if (nav.y) "y" )
        foo <- playClickOrDrag(playState, x0=x, y0=y, shape="rect", scales=scales)
        if (is.null(foo)) {
            ## drag went off device
            return(FALSE)
        }
        if (is.null(foo$coords)) {
            ## click/drag outside of a defined space
            return(FALSE)
        }
        if (foo$is.click) {
            ## single click
            coordsCore(playState, foo)
            if (foo$modifiers & GdkModifierType["control-mask"]) {
                ## control-click
                zoomoutCore(playState, foo)
            } else {
                identifyCore(playState, foo)
            }
        }
        else {
            ## drag
            zoomCore(playState, foo)
        }
    }
    if (event$button == 2) {
        ## middle mouse button
        zoomfit_handler(NULL, playState)
    }
    if (event$button == 3) {
        ## right mouse button
        ## TODO: context menu
        space <- whichSpace(playState, x, y)
        if (space != "page") {
            coords <- deviceCoordsToSpace(playState, x, y, space=space)
            foo <- list(coords=coords, space=space)
            zoomoutCore(playState, foo)
        }
    }
    return(FALSE)
}


coordsCore <- function(playState, foo) {
    coords <- foo$coords
    ## convert from log scale if necessary
    coords <- spaceCoordsToDataCoords(playState, coords)
    coordsTxt <- "<tt>        </tt>"
    if (!is.null(coords)) {
        x <- format(coords$x, digits=4)
        y <- format(coords$y, digits=4)
        coordsTxt <- paste("<tt>x:", x, ", y:", y, "</tt>", sep="")
    }
    playState$widgets$coordsLabel$setMarkup(coordsTxt)
}

zoomCore <- function(playState, foo)
{
    xonly <- playState$time.mode && is.null(playState$time.vector)
    nav.x <- TRUE
    nav.y <- !xonly
    scales <- c( if (nav.x) "x", if (nav.y) "y" )
    xlim <- range(foo$coords$x)
    ylim <- range(foo$coords$y)
    ## reverse axis scales if needed
    if (is.unsorted(rawXLim(playState, space=foo$space))) xlim <- rev(xlim)
    if (is.unsorted(rawYLim(playState, space=foo$space))) ylim <- rev(ylim)
    ## this converts from raw numeric to original format (including unlog)
    if (nav.x) rawXLim(playState) <- xlim
    if (nav.y) rawYLim(playState) <- ylim
    playReplot(playState)
}

zoomoutCore <- function(playState, foo)
{
    xonly <- playState$time.mode && is.null(playState$time.vector)
    nav.x <- TRUE
    nav.y <- !xonly
    scales <- c( if (nav.x) "x", if (nav.y) "y" )
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
