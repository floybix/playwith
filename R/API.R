# playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### API functions

playDevCur <- function() StateEnv$.current ## may be NULL

playDevList <- function()
{
    foo <- as.list(StateEnv)
    names(foo) <- lapply(foo, function(x) toString(x$win["title"]))
    foo
}

playDevSet <- function(playState)
{
    stopifnot(inherits(playState, "playState"))
    StateEnv$.current <- playState
    playState$old.dev <- dev.cur()
    dev.set(playState$dev)
    playState$win$present()
}

playDevOff <- function(playState = playDevCur())
{
    ## TODO: should this run the close action?
    if (inherits(playState$win, "GtkWindow"))
        try(playState$win$destroy())#, silent=TRUE)
    ## it seems that memory is not freed! (R2.7.1)
    rm(list=ls(playState), envir=playState)
    cleanupStateEnv()
}

playGetIDs <- function(playState = playDevCur(), labels = FALSE)
{
    ids <- do.call(rbind, playState$ids)$which
    if (labels) playState$labels[ids] else ids
}

print.playState <- function(x, ...)
{
    stopifnot(inherits(x, "playState"))
    cat(paste("<playState: ",
              toString(x$win["title"]), ">\n", sep=""))
}

cleanupStateEnv <- function()
{
    for (ID in ls(StateEnv)) {
        if (!inherits(StateEnv[[ID]], "playState")) next
        if (!inherits(StateEnv[[ID]]$win, "GtkWindow")) {
            ## window is defunct
            rm(list=ID, envir=StateEnv)
        }
    }
    ## select a new 'current' if it is invalid
    if (!inherits(StateEnv$.current$win, "GtkWindow")) {
        StateEnv$.current <- if (length(ls(StateEnv)))
            StateEnv[[ ls(StateEnv)[1] ]] else NULL
    }
}

callArg <- function(playState, arg, eval = TRUE, data = NULL)
{
    if (is.symbol(arg)) arg <- as.character(arg)
    ## work-around since the `exact` argument only appeared in R 2.6
    exactbit <- if (getRversion() <= "2.6") '"]]' else '", exact=TRUE]]'
    getx <- if (is.numeric(arg)) paste('[[', arg+1, ']]', sep="")
    else if (is.character(arg)) paste('[["', arg, exactbit, sep="")
    else paste("$", deparseOneLine(arg), sep="")
    mainCall <- mainCall(playState)
    zap <- eval(parse(text=paste("mainCall", getx, sep="")))
    if (eval == FALSE) return(zap)
    if (mode(zap) == "expression") return(zap)
    if (is.null(data))
        eval(zap, envir=playState$env, enclos=parent.frame())
    else
        eval(zap, envir=data, enclos=playState$env)
}

"callArg<-" <- function(playState, arg, value)
{
    if (is.symbol(arg)) arg <- as.character(arg)
    if (is.null(arg)) return()
    getx <- if (is.numeric(arg)) paste('[[', arg+1, ']]', sep="")
    else if (is.character(arg)) paste("$", arg, sep="")
    else paste("$", deparseOneLine(arg), sep="")
    mainCall <- mainCall(playState)
    zap <- parse(text=paste("mainCall", getx, sep=""))[[1]]
    zap <- call("<-", zap, quote(value))
    eval(zap, enclos=parent.frame())
    ## instantiate implicit lists as language objects
    ## this is required for e.g. lattice's scales$x$at <- quote(qnorm(...))
    ## easiest way is just to deparse without showAttributes and then parse
    if (is.language(arg)) {
        tmp <- try( parse(text=deparseOneLine(mainCall,
                           control=playwith.getOption("deparse.options")
                           ))[[1]] )
        if (!inherits(tmp, "try-error"))
            mainCall <- tmp
    }
    mainCall(playState) <- mainCall
    playState
}

mainCall <- function(playState) {
    recursiveIndex(playState$call, playState$main.call.index)
}

"mainCall<-" <- function(playState, value) {
    recursiveIndex(playState$call, playState$main.call.index) <- value
    playState
}

updateMainCall <- function(playState) {
    ## find which component of the call takes arguments (xlim etc)
    main.function <- playState$main.function
    tmpCall <- playState$call
    okCallPath <- function(tmpCall, main.function) {
        tmpFun <- eval(tmpCall[[1]])
        if (!is.null(main.function)) {
            ok <- identical(tmpFun, main.function)
        } else {
            ok <- any(c("xlim", "...") %in% names(formals(tmpFun)))
            ok <- ok && !identical(tmpFun, with) ## skip `with` function
        }
        if (ok) return(TRUE)
        if (length(tmpCall) > 1)
            for (i in seq(2, length(tmpCall)))
                if (is.call(tmpCall[[i]])) {
                    tmpPath <- okCallPath(tmpCall[[i]], main.function)
                    if (isTRUE(tmpPath)) return(i)
                    if (!is.null(tmpPath)) return(c(i, tmpPath))
                }
        return(NULL)
    }
    main.call.index <- okCallPath(tmpCall, main.function)
    if (is.null(main.function)) {
        ## look for a call to "plot"
        main.call.index.plot <- okCallPath(tmpCall, plot)
        if (!is.null(main.call.index.plot)) {
            ## found "plot" call
            main.call.index <- main.call.index.plot
        }
    }
    if (isTRUE(main.call.index)) main.call.index <- NA ## top-level
    ## check whether the called function accepts arguments
    playState$accepts.arguments <- !is.null(main.call.index)
    ## set index to top-level even if looks invalid, so callArg() works
    if (is.null(main.call.index)) main.call.index <- NA ## top-level
    playState$main.call.index <- main.call.index
    playState$callName <- ""
    ## put call into canonical form, but with first argument un-named
    if (playState$accepts.arguments) {
        mainCall <- mainCall(playState)
        playState$callName <- toString(deparse(mainCall[[1]]))
        ## apply match.call()
        callFun <- eval(mainCall[[1]])
        firstArgName <- names(mainCall)[2]
        mainCall <- match.call(callFun, mainCall)
        if (is.null(firstArgName) || (firstArgName == ""))
            if (!is.null(names(mainCall))) names(mainCall)[2] <- ""
        mainCall(playState) <- mainCall
    }
}

playFreezeGUI <- function(playState = playDevCur())
    playSetFreezeGUI(playState, TRUE)

playThawGUI <- function(playState = playDevCur())
    playSetFreezeGUI(playState, FALSE)

playSetFreezeGUI <- function(playState, frozen)
{
    playState$tmp$now.interacting <- frozen
    with(playState$widgets, {
        ## TODO: freeze GlobalActions etc?
        topToolbar["sensitive"] <- !frozen
        leftToolbar["sensitive"] <- !frozen
        rightToolbar["sensitive"] <- !frozen
        ## leave bottom toolbar alone as this is where parameter
        ## control tools go (otherwise long slider drags interrupted).
        ## these tools check plot.ready before redrawing (threads...).
        ## similarly, leave page and time scrollbars as sensitive.
        if (!is.null(playState$widgets$latticist))
            latticist["sensitive"] <- !frozen
    })
    playState$win$getWindow()$setCursor(
      if (frozen) gdkCursorNew("watch") else NULL)
}

blockRedraws <- function(expr, playState = playDevCur())
{
    oval <- playState$tmp$skip.redraws
    playState$tmp$skip.redraws <- TRUE
    da <- playState$widgets$drawingArea
    da$setSizeRequest(da$getAllocation()$width, da$getAllocation()$height)
                                        #playState$win$setGeometryHints(da, list(max.width=myW, min.width=myW,
                                        #	max.height=myH, min.height=myH))
                                        #da$window$freezeUpdates() # hmm
    foo <- try(eval.parent(substitute(expr)))
                                        #da$window$thawUpdates()
                                        #playState$win$setGeometryHints(da, list())
    da$setSizeRequest(-1, -1)
    playState$tmp$skip.redraws <- oval
    foo
}

hideWidgetNoRedraw <- function(playState, widget, horiz)
{
    whichDim <- if (horiz) "height" else "width"
    if (widget["visible"]) blockRedraws({
        widgSize <- widget$getAllocation()
        winSize <- playState$win$getSize()
        widget["visible"] <- FALSE
        winSize[[whichDim]] <- winSize[[whichDim]] - widgSize[[whichDim]]
        playState$win$resize(winSize$width, winSize$height)
    })
}

playPrompt <- function(playState, text = NULL)
{
    if (is.null(text)) {
        playThawGUI(playState)
        playState$widgets$statusbar$pop(0)
    } else {
        playFreezeGUI(playState)
        playState$widgets$statusbar$push(0, toString(text))
    }
    invisible()
}

rawXLim <- function(playState = playDevCur(), space="plot")
    rawXYLim(playState, space=space)$x

rawYLim <- function(playState = playDevCur(), space="plot")
    rawXYLim(playState, space=space)$y

rawXYLim <- function(playState, space="plot")
{
    playDevSet(playState)
    if (playState$is.lattice && (space == "plot")) {
        ## if space does not specify a panel, just pick one
        space <- packet.number()
        if (length(space) == 0) {
            packets <- trellis.currentLayout(which="packet")
            space <- packets[packets > 0][1]
        }
        space <- paste("packet", space)
    }
    playDo(playState,
           list(x=convertX(unit(0:1, "npc"), "native", valueOnly=TRUE),
                y=convertY(unit(0:1, "npc"), "native", valueOnly=TRUE)),
           space=space)
}

"rawXLim<-" <- function(playState = playDevCur(), value)
{
    setRawXYLim(playState, value, "x")
    playState
}

"rawYLim<-" <- function(playState = playDevCur(), value)
{
    setRawXYLim(playState, value, "y")
    playState
}

setRawXYLim <- function(playState, x, x.or.y=c("x", "y"))
{
    playDevSet(playState)
    x.or.y <- match.arg(x.or.y)
     if (playState$is.lattice) {
        ## TODO: packet 1 may not exist?
        x.panel <- xyData(playState, space="packet 1")[[x.or.y]]
        ## set factor labels explicitly, otherwise they are coerced to numeric
        if (is.factor(x.panel)) {
            scales.labels <- substitute(scales[[x.or.y]]$labels,
                                        list(x.or.y=x.or.y))
            scales.at <- substitute(scales[[x.or.y]]$at,
                                        list(x.or.y=x.or.y))
            if (is.null(callArg(playState, scales.labels))) {
                callArg(playState, scales.labels) <- levels(x.panel)
                callArg(playState, scales.at) <- 1:nlevels(x.panel)
            }
        }
        else if (is.somesortoftime(x.panel)) {
          class(x) <- class(x.panel)
          if (inherits(x.panel, "Date"))
            x <- call("as.Date", format(x))
          if (inherits(x.panel, "POSIXct"))
            x <- call("as.POSIXct", format(x))
          if (inherits(x.panel, "yearmon"))
            x <- call("as.yearmon", format(as.Date(x)))
          if (inherits(x.panel, "yearqtr"))
            x <- call("as.yearqtr", format(as.Date(x)))
        }
        else {
          ## numeric
          ## it seems now (lattice 0.17-12) that xlim/ylim are used directly
          ## so we don't need this:
          #isExtended <- switch(x.or.y,
          #                     x = playState$trellis$x.scales$axs == "r",
          #                     y = playState$trellis$y.scales$axs == "r")
          #f <- lattice.getOption("axis.padding")$numeric
          #if (isExtended) x <- shrinkrange(x, f=f)
        }
    }
    else if (!is.null(playState$viewport)) {
      ## non-lattice grid plot
      ## (do not know if the range is extended or not).
    }
    else {
      ## base graphics plot
      isExtended <- switch(x.or.y,
                           x = (par("xaxs") == "r"),
                           y = (par("yaxs") == "r"))
      if (isExtended) x <- shrinkrange(x, f=0.04)
    }
    ## convert back from log scale if required
    x <- spaceCoordsToDataCoordsXY(playState, x, x.or.y=x.or.y)
    ## round such that approximation error is within 1/1000 of x/y range
    if (is.numeric(x)) {
        digits <- max(3 - floor(log10(abs(diff(x)))), 0)
        x <- round(x, digits = digits)
    }
    if (x.or.y == "x") callArg(playState, "xlim") <- x
    if (x.or.y == "y") callArg(playState, "ylim") <- x
}

shrinkrange <- function(r, f = 0.1)
{
  stopifnot(length(r) == 2)
  orig.d <- diff(r) / (1 + 2*f)
  orig.r <- r - c(-f, f) * orig.d
  orig.r
}

is.somesortoftime <- function(x) {
  inherits(x, "Date") ||
  inherits(x, "POSIXt") ||
  inherits(x, "yearmon") ||
  inherits(x, "yearqtr")
}

playDo <- function(playState, expr, space = "plot",
                   clip.off = !isTRUE(playState$clip.annotations))
{
    playDevSet(playState)
    ## store current viewport and restore it when finished
    cur.vp <- current.vpPath()
    upViewport(0) ## go to root viewport
    on.exit({
        upViewport(0)
        if (!is.null(cur.vp)) downViewport(cur.vp)
    })
    if (space == "page") {
        ## normalised device coordinates
        downViewport("pageAnnotationVp")
    } else {
        ## user / plot coordinates
        if (!is.null(playState$viewport)) {
            ## grid graphics plot
            downViewport(playState$viewport[[space]])
        }
        else if (playState$is.lattice) {
            ## lattice plot
            packets <- trellis.currentLayout(which="packet")
            if (space == "plot") {
                space <- packet.number()
                if (length(space) == 0) {
                    if (sum(packets > 0) > 1)
                        stop("space not well specified")
                    space <- packets[packets > 0][1]
                }
                space <- paste("packet", space)
            }
            packet <- as.numeric(sub("packet ", "", space))
            whichOne <- which(packets == packet)
            if (length(whichOne) == 0) return()
            myCol <- col(packets)[whichOne]
            myRow <- row(packets)[whichOne]
            myVp <- trellis.vpname("panel", myCol, myRow, clip.off=clip.off)
            downViewport(myVp)
            ## NOTE: a panel is not in focus here (as in trellis.focus)
            ## because that would destroy any previous focus state
            ## -- if focus is required, do that before calling playDo.
        }
        else {
            ## base graphics
            space <- "plot"
            if (clip.off) space <- "plot.clip.off"
            downViewport(playState$tmp$baseVps[[space]])
        }
    }
    ## do the stuff and return the result
    eval(substitute(expr), parent.frame(), playState$env)
}

playSelectData <-
    function(playState = playDevCur(),
             prompt = paste(
             "Click or drag to select data points;",
             "Right-click or Esc to cancel."))
{
    foo <- playRectInput(playState, prompt=prompt)
    if (is.null(foo)) return(NULL)
    if (is.null(foo$coords)) return(NULL)
    coords <- foo$coords
    data <- xyCoords(playState, space=foo$space)
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
                       list(lx=convertX(unit(x, "native"), "points", TRUE),
                            ly=convertY(unit(y, "native"), "points", TRUE),
                            px=convertX(unit(data$x, "native"), "points", TRUE),
                            py=convertY(unit(data$y, "native"), "points", TRUE)),
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
    c(list(which=which, x=data$x[which], y=data$y[which],
           subscripts=data$subscripts[which],
           pos=pos, is.click=foo$is.click),
      foo)
}

playPointInput <-
    function(playState = playDevCur(),
             prompt = paste(
             "Click on the plot;",
             "Right-click or Esc to cancel."))
{
    playDevSet(playState)
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
                   convertFromDevicePixels(dc$x, dc$y, valueOnly = TRUE),
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
                   convertFromDevicePixels(dc$x, dc$y, valueOnly = TRUE),
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

getDataArg <- function(playState, eval = TRUE)
{
    if (is.null(playState$data.points)) {
        mainCall <- mainCall(playState)
        if (length(mainCall > 1)) {
            ## check for named "data" argument
            tmp.data <- callArg(playState, "data", eval=eval)
            if (is.null(tmp.data)) {
                ## look at first argument
                tmp.x <- callArg(playState, 1, eval=TRUE)
                if (inherits(tmp.x, "formula")) {
                    ## if 1st arg is formula, 2nd is `data` (by convention)
                    if (is.null(tmp.data) &&
                        (length(mainCall) >= 3) &&
                        (is.null(names(mainCall)) ||
                         identical(names(mainCall)[[3]], ""))
                        )
                        tmp.data <- callArg(playState, 2, eval=eval)
                }
            }
        }
        if (is.null(tmp.data)) {
            ## objects may also come from a with() block
            if (identical(playState$call[[1]], as.symbol("with"))) {
                tmp.data <- playState$call[[2]]
                if (eval) tmp.data <- eval(tmp.data, playState$env)
            }
        }
    } else {
        ## data.points were supplied
        tmp.data <- if (eval) playState$data.points
        else quote(playDevCur()$data.points)
    }
    tmp.data
}

xyCoords <- function(playState = playDevCur(), space="plot")
{
    foo <- xyData(playState, space=space)
    foo$x <- as.numeric(foo$x)
    foo$y <- as.numeric(foo$y)
    foo
}

xyData <- function(playState = playDevCur(), space="plot")
{
    if (!is.null(playState$data.points)) {
        return(xy.coords_with_class(playState, playState$data.points))
    }
    if (playState$is.lattice) {
        if (space == "page") {
            ## data from all packets
            tmp <- try(do.call(rbind,
                               lapply(playState$trellis$panel.args, as.data.frame)
                               ), silent=TRUE)
            if (inherits(tmp, "try-error"))
                return(NULL)
            return(tmp)
        }
        if (space == "plot") {
            space <- packet.number()
            if (length(space) == 0) {
                packets <- trellis.currentLayout(which="packet")
                if (sum(packets > 0) > 1) stop("space not well specified")
                space <- packets[packets > 0][1]
            }
            space <- paste("packet", space)
        }
        packet <- as.numeric(sub("packet ", "", space))
        foo <- trellis.panelArgs(playState$trellis, packet.number=packet)
        if (is.null(foo$y) && !is.null(foo$distribution)) {
            ## probably `qqmath`
            return(xy.coords.qqmath(panel.args=foo))
        }
        if (length(foo$x) != length(foo$y)) {
            if ((nx <- length(foo$x)) < (ny <- length(foo$y)))
                foo$x <- rep(foo$x, length.out = ny)
            else foo$y <- rep(foo$y, length.out = nx)
        }
        return(foo)
    }
    ## otherwise...
    ## hard-coded exceptions
    callName <- deparseOneLine(mainCall(playState)[[1]])
    if (callName %in% c("qqnorm", "qqplot")) {
        ## these return plotted coordinates in a list
        modCall <- mainCall(playState)
        modCall$plot <- FALSE
        foo <- eval(modCall, playState$env)
        return(foo)
    }
    ## check for named "data" argument
    tmp.data <- callArg(playState, "data") ## may be NULL
    ## look at first argument (tmp.data may be NULL)
    tmp.x <- callArg(playState, 1, data=tmp.data)
    if (inherits(tmp.x, "formula")) {
        ## if 1st arg is formula, 2nd is `data` (by convention)
        if (is.null(tmp.data) && (length(mainCall) > 2) &&
            (is.null(names(mainCall)) ||
             identical(names(mainCall)[[3]], "")))
            tmp.data <- callArg(playState, 2)
    }
    if (is.null(tmp.data)) {
        ## objects may also come from a with() block
        if (identical(playState$call[[1]], as.symbol("with")))
            tmp.data <- eval(playState$call[[2]], playState$env)
    }
    tmp.y <- callArg(playState, "y", data=tmp.data)
    xy.coords_with_class(playState, tmp.x, tmp.y, data=tmp.data)
}

## adapted from grDevices::xy.coords
xy.coords_with_class <- function(playState, x, y=NULL, recycle=TRUE, data=NULL)
{
    if (is.null(y)) {
        if (is.language(x)) {
            if (inherits(x, "formula") && length(x) == 3) {
                if (!is.null(data)) {
                    y <- eval(x[[2]], data, playState$env)
                    x <- eval(x[[3]], data, playState$env)
                } else {
                    y <- eval(x[[2]], environment(x), playState$env)
                    x <- eval(x[[3]], environment(x), playState$env)
                }
            }
            else stop("invalid first argument")
        }
        else if (inherits(x, "zoo")) {
            y <- if (is.matrix(x)) x[, 1] else x
            x <- stats::time(x)
        }
        else if (inherits(x, "ts")) {
            y <- if (is.matrix(x)) x[, 1] else x
            x <- stats::time(x)
        }
        else if (is.complex(x)) {
            y <- Im(x)
            x <- Re(x)
        }
        else if (is.matrix(x) || is.data.frame(x)) {
            if (ncol(x) == 1) {
                y <- x[, 1]
                x <- seq_along(y)
            }
            else {
                y <- x[, 2]
                x <- x[, 1]
            }
        }
        else if (is.list(x)) {
            y <- x[["y"]]
            x <- x[["x"]]
        }
        else {
            y <- x
            x <- seq_along(x)
        }
    }
    if (inherits(x, "zoo")) {
        ## and y is not null
        x <- as.numeric(x)
        y <- as.numeric(y)
    }
    if (inherits(x, "POSIXt")) x <- as.POSIXct(x)
    if (length(x) != length(y)) {
        if (recycle) {
            if ((nx <- length(x)) < (ny <- length(y)))
                x <- rep(x, length.out = ny)
            else y <- rep(y, length.out = nx)
        } else stop("'x' and 'y' lengths differ")
    }
    list(x=x, y=y)
}

## copied from panel.identify.qqmath
xy.coords.qqmath <-
    function(x = panel.args$x,
             distribution = panel.args$distribution,
             groups = panel.args$groups,
             subscripts = panel.args$subscripts,
             labels = subscripts,
             panel.args = trellis.panelArgs(),
             ...)
{
    x <- as.numeric(x)
    if (is.null(subscripts)) subscripts <- seq_along(x)
    if (!is.null(panel.args$f.value)) warning("'f.value' not supported; ignoring")
    ## TODO: just return NULL if f.value is defined
    distribution <-
        if (is.function(distribution)) distribution
        else if (is.character(distribution)) get(distribution)
        else eval(distribution)
    ## compute quantiles corresponding to given vector, possibly
    ## containing NA's.  The return value must correspond to the
    ## original order
    getq <- function(x)
    {
        ans <- x
        id <- !is.na(x)
        ord <- order(x[id])
        if (any(id)) ans[id] <- distribution(ppoints(sum(id)))[order(ord)]
        ans
    }
    if (is.null(groups))
    {
        return(list(x = getq(x), y = x, subscripts = subscripts))
    }
    else
    {
        allq <- rep(NA_real_, length(x))
        subg <- groups[subscripts]
        vals <- if (is.factor(groups)) levels(groups) else sort(unique(groups))
        for (i in seq_along(vals))
        {
            ok <- !is.na(subg) & (subg == vals[i])
            allq[ok] <- getq(x[ok])
        }
        return(list(x = allq, y = x, subscripts = subscripts))
    }
}

## aka playUnLog
spaceCoordsToDataCoords <- function(playState, xy)
{
    if (!is.null(xy$x)) xy$x <-
        spaceCoordsToDataCoordsXY(playState, xy$x, x.or.y="x")
    if (!is.null(xy$y)) xy$y <-
        spaceCoordsToDataCoordsXY(playState, xy$y, x.or.y="y")
    xy
}

spaceCoordsToDataCoordsXY <- function(playState, x, x.or.y=c("x", "y"))
{
    x.or.y <- match.arg(x.or.y)
    logBase <- playLogBase(playState, x.or.y)
    if (is.na(logBase)) return(x)
    logBase ^ x
}

## aka playReLog
dataCoordsToSpaceCoords <- function(playState, xy)
{
    if (!is.null(xy$x)) xy$x <-
        dataCoordsToSpaceCoordsXY(playState, xy$x, x.or.y="x")
    if (!is.null(xy$y)) xy$y <-
        dataCoordsToSpaceCoordsXY(playState, xy$y, x.or.y="y")
    xy
}

dataCoordsToSpaceCoordsXY <- function(playState, x, x.or.y=c("x", "y"))
{
    x.or.y <- match.arg(x.or.y)
    logBase <- playLogBase(playState, x.or.y)
    if (is.na(logBase)) return(x)
    log(x, base=logBase)
}

playLogBase <- function(playState, x.or.y=c("x", "y"))
{
    x.or.y <- match.arg(x.or.y)
    if (playState$is.lattice) {
        scalesObj <- playState$trellis[[paste(x.or.y, "scales", sep=".")]]
        x <- scalesObj$log
        if (identical(x, FALSE)) return(NA)
        if (isTRUE(x)) return(10)
        if (identical(x, "e")) return(exp(1))
        return(x)
    } else if (playState$is.ggplot) {
        logArg <- callArg(playState, "log")
        if (!is.null(logArg) &&
            (x.or.y %in% strsplit(logArg, split="")[[1]]))
            return(10)
    } else {
        ## traditional graphics plot
        if (par(paste(x.or.y, "log", sep=""))) return(10)
    }
    return(NA)
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

isBasicDeviceMode <- function(playState)
{
    if ((length(playState$call) == 1) &&
        identical(playState$call[[1]], quote(`{`))) {
        ## basic device mode
        ## (do not know the call)
        return(TRUE)
    }
    FALSE
}

gmessage.error <- function(message, title="Error", icon="error", ...)
    gmessage(message, title=title, icon=icon, ...)

Filters <- matrix(c(
                    "R or S files (*.R,*.q,*.ssc,*.S)", "*.R;*.q;*.ssc;*.S",
                    "Postscript files (*.ps)",          "*.ps",
                    "Encapsulated Postscript (*.eps)",  "*.eps",
                    "PDF files (*.pdf)",                "*.pdf",
                    "Png files (*.png)",                "*.png",
                    "Jpeg files (*.jpeg,*.jpg)",        "*.jpeg;*.jpg",
                    "Text files (*.txt)",               "*.txt",
                    "R images (*.RData,*.rda)",         "*.RData;*.rda",
                    "Zip files (*.zip)",                "*.zip",
                    "SVG files (*.svg)",                "*.svg",
                    "Windows Metafiles (*.wmf,*.emf)",  "*.wmf;*.emf",
                    "xfig files (*.fig)",               "*.fig",
                    "All files (*.*)",                  "*.*"), ncol=2, byrow=T,
                  dimnames=list(c('R','ps','eps','pdf','png','jpeg','txt',
                  'RData','zip','svg','wmf','fig','All'),NULL))

get.extension <- function(path)
{
    ## Extract and return the extension part of a filename

    parts <- strsplit(path, "\\.")[[1]]
    if (length(parts) > 1)
        last <- parts[length(parts)]
    else
        last <- ""
    last
}

