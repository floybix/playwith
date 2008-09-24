# playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

playSelectData <-
    function(playState = playDevCur(),
             prompt = paste(
             "Click or drag to select data points;",
             "Right-click or Esc to cancel."),
             foo = playRectInput(playState, prompt = prompt))
{
    force(foo)
    if (is.null(foo)) return(NULL)
    if (is.null(foo$coords)) return(NULL)
    coords <- foo$coords
    data <- xyCoords(playState, space = foo$space)
    ## convert to log scale if necessary
    data <- dataCoordsToSpaceCoords(playState, data)

    if (length(data$x) == 0) {
        gmessage.error(paste("Sorry, can not guess the data point coordinates.",
                             "Please contact the maintainer with suggestions."))
        return(NULL)
    }
    which <- NULL
    pos <- NULL
    if (foo$is.click) {
        x <- coords$x[1]
        y <- coords$y[1]
        ppxy <- playDo(playState,
                       quote(list(
                            lx=convertX(unit(x, "native"), "points", TRUE),
                            ly=convertY(unit(y, "native"), "points", TRUE),
                            px=convertX(unit(data$x, "native"), "points", TRUE),
                            py=convertY(unit(data$y, "native"), "points", TRUE))),
                       space=foo$space)
        pdists <- with(ppxy, sqrt((px - lx)^2 + (py - ly)^2))
        if (min(pdists, na.rm = TRUE) > 18)
            ##warning("no observations within ", 18, " points")
            which <- integer(0)
        else {
            which <- which.min(pdists)
            pos <- with(ppxy, lattice:::getTextPosition(x = lx - px[which],
                                                        y = ly - py[which]))
        }
    }
    else {
        ## drag
        ok <- TRUE
        if (!foo$yOnly)
            ok <- (min(coords$x) <= data$x) & (data$x <= max(coords$x))
        if (!foo$xOnly)
            ok <- ok & (min(coords$y) <= data$y) & (data$y <= max(coords$y))
        which <- which(ok)
    }
    subscripts <- data$subscripts[which]
    if (is.null(subscripts)) subscripts <- which
    c(list(subscripts = subscripts, which = which,
           x = data$x[which], y = data$y[which],
           pos = pos, is.click = foo$is.click),
      foo)
}

playPointInput <-
    function(playState = playDevCur(),
             prompt = paste(
             "Click on the plot;",
             "Right-click or Esc to cancel."))
{
    playDevSet(playState)
    playState$win$present()
    playPrompt(playState, prompt)
    on.exit(playPrompt(playState, NULL))
    cur.vp <- current.vpPath()
    upViewport(0)
    if (!is.null(cur.vp)) on.exit(downViewport(cur.vp), add=TRUE)
    dc <- grid.locator()
    if (is.null(dc)) return(NULL)
    ## check for modifier keys
    ptrInfo <- playState$widgets$drawingArea$window$getPointer()
    modifiers <- as.flag(0)
    if (!is.null(ptrInfo$retval))
        modifiers <- as.flag(ptrInfo$mask)
    ## convert coordinates
    ndc <- list(x=convertX(dc$x, "npc"), y=convertY(dc$y, "npc"))
    dc <- lapply(dc, as.numeric)
    ndc <- lapply(ndc, as.numeric)
    coords <- NULL
    space <- whichSpace(playState, dc$x, dc$y)
    if (space != "page") {
        coords <-
            playDo(playState,
                   quote(convertFromDevicePixels(dc$x, dc$y, valueOnly = TRUE)),
                   space = space)
    }
    list(coords=coords, space=space, dc=dc, ndc=ndc, modifiers=modifiers)
}

playLineInput <-
    function(playState = playDevCur(),
             prompt = paste(
             "Click and drag to define a line",
             "(hold Shift to constrain to x or y scales);",
             "Right-click or Esc to cancel."))
{
    playDevSet(playState)
    playState$win$present()
    playPrompt(playState, prompt)
    on.exit(playPrompt(playState, NULL))
    vp <- current.vpPath()
    upViewport(0)
    on.exit(if (!is.null(vp)) downViewport(vp), add=TRUE)
    ## wait for click
    xy0 <- grid.locator()
    if (is.null(xy0)) return(NULL)
    xy0 <- lapply(xy0, as.numeric)
    playClickOrDrag(playState, x0=xy0$x, y=xy0$y, shape="line")
}

playRectInput <-
    function(playState = playDevCur(),
             prompt = paste(
             "Click and drag to define a rectangular region",
             "(hold Shift to constrain to x or y scales);",
             "Right-click or Esc to cancel."))
{
    playDevSet(playState)
    playState$win$present()
    playPrompt(playState, prompt)
    on.exit(playPrompt(playState, NULL))
    vp <- current.vpPath()
    upViewport(0)
    on.exit(if (!is.null(vp)) downViewport(vp), add=TRUE)
    ## wait for click
    xy0 <- grid.locator()
    if (is.null(xy0)) return(NULL)
    xy0 <- lapply(xy0, as.numeric)
    playClickOrDrag(playState, x0=xy0$x, y=xy0$y, shape="rect")
}

## assumes that the mouse button has already been pressed
## converts into user coordinates
playClickOrDrag <-
    function(playState, x0, y0,
             shape=c("rect", "line"))
{
    playDevSet(playState)
    foo <- handleClickOrDrag(playState$widgets$drawingArea,
                             x0=x0, y0=y0, shape=shape)
    if (is.null(foo)) return(NULL)
    dc <- foo$dc
    coords <- NULL
    ## work out which space the drag was in: try the mid-point first
    space <- whichSpace(playState, mean(dc$x), mean(dc$y))
    ## otherwise, try start of drag
    if (space == "page") space <- whichSpace(playState, dc$x[1], dc$y[1])
    ## otherwise, try end of drag
    if (space == "page") space <- whichSpace(playState, dc$x[2], dc$y[2])
    if (space != "page") {
        coords <-
            playDo(playState,
                   quote(convertFromDevicePixels(dc$x, dc$y, valueOnly = TRUE)),
                   space = space)
    }
    foo$coords <- coords
    foo$space <- space
    foo
}

## assumes that the mouse button has already been pressed
handleClickOrDrag <-
    function(da, x0, y0,
             shape = c("rect", "line"))
{
    CLICKDUR <- 0.25 ## seconds
    shape <- match.arg(shape)
    ## xyInit is the original click location
    xyInit <- list(x=x0, y=y0)
    da.w <- da$getAllocation()$width
    da.h <- da$getAllocation()$height
    buf <- gdkPixbufGetFromDrawable(src=da$window, src.x=0, src.y=0,
                                    dest.x=0, dest.y=0, width=da.w, height=da.h)
    if (is.null(buf)) stop("Could not make pixbuf")
    gc <- gdkGCNew(da$window)
    gc$copy(da["style"]$blackGc)
    gc$setRgbFgColor(gdkColorParse("black")$color)
    gc$setRgbBgColor(gdkColorParse("white")$color)
    gc$setLineAttributes(line.width=1, line.style=GdkLineStyle["double-dash"],
                         cap.style=GdkCapStyle["butt"], join.style=GdkJoinStyle["miter"])
    gc$setDashes(c(8, 4))
    ## xyDrag is the drag-to location while dragging
    xyDrag <- xyInit
    ## these are used to constrain the drag to x or y scales
    xOnly <- FALSE
    yOnly <- FALSE
    ## xyEnd is the final drag-to location
    release_handler <- function(widget, event, env) {
        ## mouse button was released
        env$xyEnd <- list(x=event$x, y=event$y)
        return(TRUE)
    }
    expose_handler <- function(widget, event) {
        area <- event$area
        gdkDrawPixbuf(event$window, pixbuf=buf,
                      src.x=area$x, src.y=area$y, dest.x=area$x, dest.y=area$y,
                      width=area$width, height=area$height)
        xx <- c(xyInit$x, xyDrag$x)
        yy <- c(xyInit$y, xyDrag$y)
        ## constrain drag along x or y scales
        if (shape == "rect") {
            if (xOnly) yy <- c(-1, da.h)
            if (yOnly) xx <- c(-1, da.w)
        }
        if (shape == "line") {
            if (xOnly) yy[2] <- yy[1]
            if (yOnly) xx[2] <- xx[1]
        }
         switch(shape,
               line = gdkDrawLine(event$window, gc=gc,
                   x1=xx[1], y1=yy[1],
                   x2=xx[2], y2=yy[2]),
               rect = gdkDrawRectangle(event$window, gc=gc,
                   filled=FALSE, x=min(xx), min(yy),
                   width=abs(diff(xx)), height=abs(diff(yy)))
               )
        return(TRUE) ## stop event here
    }
    tmpSigE <- gSignalConnect(da, "expose-event", expose_handler)
    tmpSigR <- gSignalConnect(da, "button-release-event", release_handler,
                              data=environment())
    rectx <- xyInit$x
    recty <- xyInit$y
    init_time <- proc.time()[3]
    repeat {
        ## xyEnd is the final drag location, set by event handler
        if (exists("xyEnd", inherits=FALSE)) break
        xyDrag <- da$window$getPointer()
        ## check that pointer is inside the window
        ## -- fails on linux?
        #if (is.null(xyDrag$retval)) break
        if ((as.flag(xyDrag$mask) & GdkModifierType["button1-mask"]) == 0) {
            ## mouse button was released
            xyEnd <- xyDrag
            break
        }
        ## (if it is a drag, not a click)
        if ((proc.time()[3] - init_time) > CLICKDUR) {
            ## constrain to x or y scales if holding Shift
            if ((as.flag(xyDrag$mask) & GdkModifierType["shift-mask"])) {
                ## decide which scale to constrain by direction of drag
                dragHoriz <- (abs(xyInit$x - xyDrag$x) >
                              abs(xyInit$y - xyDrag$y))
                xOnly <- dragHoriz
                yOnly <- !dragHoriz
            } else {
                xOnly <- yOnly <- FALSE
            }
        }
        ## work out the region that needs to be redrawn
        rectx <- range(c(xyInit$x, xyDrag$x, rectx))
        recty <- range(c(xyInit$y, xyDrag$y, recty))
        ## constrain rectangle along x or y scales
        if (shape == "rect") {
            if (xOnly) recty <- c(-1, da.h)
            if (yOnly) rectx <- c(-1, da.w)
        }
        wd <- rectx[2] - rectx[1] + 2
        ht <- recty[2] - recty[1] + 2
        da$window$invalidateRect(list(x=rectx[1], y=recty[1],
                                      width=wd, height=ht),
                                 invalidate.children=FALSE)
        ## try to force redraw
        gdkWindowProcessAllUpdates()
        while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
    }
    end_time <- proc.time()[3]
    gSignalHandlerDisconnect(da, tmpSigR)
    gSignalHandlerDisconnect(da, tmpSigE)
    ## check for modifier keys
    ptrInfo <- da$window$getPointer()
    modifiers <- as.flag(0)
    if (!is.null(ptrInfo$retval))
        modifiers <- as.flag(ptrInfo$mask)
    ## clean up
    da$window$invalidateRect(invalidate.children=FALSE)
    if (!exists("xyEnd", inherits=FALSE)) return(NULL)
    ## device coordinates
    ## note origin is at top-left (same as ROOT viewport)
    dc <- list(x = c(xyInit$x, xyEnd$x),
               y = c(xyInit$y, xyEnd$y))
    if (shape == "line") {
        if (xOnly) dc$y[2] <- dc$y[1]
        if (yOnly) dc$x[2] <- dc$x[1]
    }
    ## normalised device coordinates
    ndc <- list(x = dc$x / da.w, y = dc$y / da.h)
    ## was it a click or drag? (click = no slower than 1/4 second)
    is.click <- (end_time - init_time) <= CLICKDUR
    ## alternative criteria for click: moved less than 10 pixels
    is.click <- is.click ||
                ((abs(diff(dc$x)) < 10) && (abs(diff(dc$y)) < 10))
    list(dc = dc, ndc = ndc, xOnly = xOnly, yOnly = yOnly,
         is.click = is.click, modifiers = modifiers)
}

whichSpace <- function(playState, x.px, y.px)
{
    ## assumes spaces do not overlap
    for (space in names(playState$tmp$spaceLimDevice)) {
        lims <- playState$tmp$spaceLimDevice[[space]]
        ## test for point inside bounds
        x <- lims$x
        y <- lims$y
        if ((min(x) <= x.px) && (x.px <= max(x)) &&
            (min(y) <= y.px) && (y.px <= max(y)))
            return(space)
    }
    ## return "page" if not inside any of the viewports
    return("page")
}
