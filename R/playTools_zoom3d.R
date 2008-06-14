## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### NAV 3D

toolConstructors$zoomin.3d <- function(playState)
{
    if (playState$accepts.arguments == FALSE) return(NA)
    ## this tool only works with 3D lattice plots
    callName <- deparseOneLine(playState$call[[1]])
    if ((callName %in% c("cloud", "wireframe")) == FALSE)
        return(NA)

    quickTool(playState,
              label = "Zoom in",
              icon = "gtk-zoom-in",
              f = zoomin3d_handler)
}

toolConstructors$zoomout.3d <- function(playState)
{
    if (playState$accepts.arguments == FALSE) return(NA)
    ## this tool only works with 3D lattice plots
    callName <- deparseOneLine(playState$call[[1]])
    if ((callName %in% c("cloud", "wireframe")) == FALSE)
        return(NA)

    quickTool(playState,
              label = "Zoom out",
              icon = "gtk-zoom-out",
              f = zoomout3d_handler)
}

toolConstructors$fly.left.3d <- function(playState)
{
    if (playState$accepts.arguments == FALSE) return(NA)
    ## this tool only works with 3D lattice plots
    callName <- deparseOneLine(playState$call[[1]])
    if ((callName %in% c("cloud", "wireframe")) == FALSE)
        return(NA)

    quickTool(playState,
              label = "Fly left",
              icon = "gtk-media-rewind-ltr",
              f = flyleft3d_handler)
}

toolConstructors$fly.right.3d <- function(playState)
{
    if (playState$accepts.arguments == FALSE) return(NA)
    ## this tool only works with 3D lattice plots
    callName <- deparseOneLine(playState$call[[1]])
    if ((callName %in% c("cloud", "wireframe")) == FALSE)
        return(NA)

    quickTool(playState,
              label = "Fly right",
              icon = "gtk-media-rewind-rtl",
              f = flyright3d_handler)
}

zoomin3d_handler <- function(widget, playState)
{
    zoom <- callArg(playState, zoom)
    if (is.null(zoom)) zoom <- 1
    callArg(playState, zoom) <- signif(zoom * 1.5, 4)
    playReplot(playState)
}

zoomout3d_handler <- function(widget, playState)
{
    zoom <- callArg(playState, zoom)
    if (is.null(zoom)) zoom <- 1
    callArg(playState, zoom) <- signif(zoom / 1.5, 4)
    playReplot(playState)
}

flyleft3d_handler <- function(widget, playState)
{
    screen <- callArg(playState, screen)
    if (is.null(screen)) screen <- list(z=40, x=-60)
    if (names(screen)[1] == 'z') screen[[1]] <- screen[[1]] + 45
    else screen <- c(z = 45, screen)
    ## convert list to call so that deparse is pretty
    callArg(playState, screen) <- as.call(c(quote(list), screen))
    playReplot(playState)
}

flyright3d_handler <- function(widget, playState)
{
    screen <- callArg(playState, screen)
    if (is.null(screen)) screen <- list(z=40, x=-60)
    if (names(screen)[1] == 'z') screen[[1]] <- screen[[1]] - 45
    else screen <- c(z = -45, screen)
    ## convert list to call so that deparse is pretty
    callArg(playState, screen) <- as.call(c(quote(list), screen))
    playReplot(playState)
}

