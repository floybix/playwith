## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### ZOOM

toolConstructors$zoom <- function(playState)
{
    if (playState$accepts.arguments == FALSE) return(NA)
    ## this tool does not work with multiple base graphics plots
    if (!playState$is.lattice && is.null(playState$viewport)) {
        if (any(par("mfrow") > 1)) return(NA)
    }
    ## this tool does not work with "splom" or 3D plots
    callName <- deparseOneLine(mainCall(playState)[[1]])
    if (callName %in% c("splom", "cloud", "wireframe"))
        return(NA)

    quickTool(playState,
              label = "Zoom...",
              icon = "gtk-zoom-in",
              tooltip = "Select a new plot region with the mouse",
              f = zoom_handler)
}

zoomCore <- function(playState, foo)
{
    if (!("zoom" %in% names(playState$tools))) return()
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

zoom_handler <- function(widget, playState)
{
    xonly <- playState$time.mode && is.null(playState$time.vector)
    nav.x <- TRUE
    nav.y <- !xonly
    scales <- c( if (nav.x) "x", if (nav.y) "y" )
    foo <- playRectInput(playState, prompt=paste(
                                    "Click and drag to define the new plot region.",
                                    "(Right-click to cancel)"), scales=scales)
    if (is.null(foo)) return()
    if (is.null(foo$coords)) return()
    if (foo$is.click) return()
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

### ZOOMOUT

toolConstructors$zoomout <- function(playState)
{
    if (playState$accepts.arguments == FALSE) return(NA)
    ## this tool does not work with multiple base graphics plots
    if (!playState$is.lattice && is.null(playState$viewport)) {
        if (any(par("mfrow") > 1)) return(NA)
    }
    ## this tool does not work with "splom" or 3D plots
    callName <- deparseOneLine(mainCall(playState)[[1]])
    if (callName %in% c("splom", "cloud", "wireframe"))
        return(NA)

    quickTool(playState,
              label = "Zoom out",
              tooltip = "Zoom out to show 4x plot area",
              icon = "gtk-zoom-out",
              f = zoomout_handler)
}

zoomoutCore <- function(playState, foo)
{
    if (!("zoom" %in% names(playState$tools))) return()
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

zoomout_handler <- function(widget, playState)
{
    xonly <- playState$time.mode && is.null(playState$time.vector)
    nav.x <- TRUE
    nav.y <- !xonly
    ## find existing scales
    xlim <- rawXLim(playState)
    ylim <- rawYLim(playState)
    ## zoom out: make range twice the size
    if (nav.x) xlim <- xlim + diff(xlim) * c(-0.5, 0.5)
    if (nav.y) ylim <- ylim + diff(ylim) * c(-0.5, 0.5)
    ## this converts from raw numeric to original format (including unlog)
    if (nav.x) rawXLim(playState) <- xlim
    if (nav.y) rawYLim(playState) <- ylim
    playReplot(playState)
}

### ZOOMFIT

toolConstructors$zoomfit <- function(playState)
{
    if (playState$accepts.arguments == FALSE) return(NA)
    quickTool(playState,
              label = "Fit data",
              icon = "gtk-zoom-fit",
              f = zoomfit_handler,
              post.plot.action = zoomfit_postplot_action)
}

zoomfit_handler <- function(widget, playState)
{
    xonly <- playState$time.mode && is.null(playState$time.vector)
    nav.x <- TRUE
    nav.y <- !xonly
    callName <- deparseOneLine(mainCall(playState)[[1]])
    nav.z <- (callName %in% c("cloud", "wireframe"))
    ## update scales
    if (nav.x) callArg(playState, "xlim") <- NULL
    if (nav.y) callArg(playState, "ylim") <- NULL
    if (nav.z) callArg(playState, "zlim") <- NULL
    playReplot(playState)
}

zoomfit_postplot_action <- function(widget, playState)
{
    xonly <- playState$time.mode && is.null(playState$time.vector)
    nav.x <- TRUE
    nav.y <- !xonly
    callName <- deparseOneLine(mainCall(playState)[[1]])
    nav.z <- (callName %in% c("cloud", "wireframe"))
    nonfit <- FALSE
    if (nav.x && !is.null(callArg(playState, "xlim"))) nonfit <- TRUE
    if (nav.y && !is.null(callArg(playState, "ylim"))) nonfit <- TRUE
    if (nav.z && !is.null(callArg(playState, "zlim"))) nonfit <- TRUE
    widget["visible"] <- nonfit
}
