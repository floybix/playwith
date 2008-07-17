## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### COORDS

toolConstructors$clickhandler <- function(playState)
{
    ## add click event handler to plot -- always active
    if (is.null(playState$widgets$clickEventSignal)) {
        playState$widgets$clickEventSignal <-
            gSignalConnect(playState$widgets$drawingArea,
                           "button-press-event", device_click_handler,
                           data=playState)
    }
    return(NA)
}

device_click_handler <- function(widget, event, playState)
{
    if (!isTRUE(playState$plot.ready)) return(FALSE)
    ## bail out if another tool is handling the click
    ## TODO: is this a race condition? is there a better way?
    if (isTRUE(playState$now.interacting)) return(FALSE)
    if (playState$.need.reconfig) generateSpaces(playState)
    x <- event$x
    y <- event$y
    coordsCore(playState, NULL)
    if (event$button == 1) {
        ## standard (left) mouse button
        xonly <- playState$time.mode && is.null(playState$time.vector)
        nav.x <- TRUE
        nav.y <- !xonly
        ## TODO: if holding left/right arrow, constrain to x
        ##       if holding up/down arrow, constrain to y
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
            identifyCore(playState, foo)
        }
        else {
            ## drag
            zoomCore(playState, foo)
        }
    }
    if (event$button == 3) {
        ## right mouse button
        space <- whichSpace(playState, x, y)
        if (space != "page") {
            coords <- deviceCoordsToSpace(playState, x, y, space=space)
            foo <- list(coords=coords, space=space)
            zoomoutCore(playState, foo)
        }
    }
    return(FALSE)
}
