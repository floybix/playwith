## playwith: interactive plots in R using GTK+
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
    try(playState$win$destroy(), silent=TRUE)
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
    if (!inherits(StateEnv$.current$win, "GtkWindow")) {
        StateEnv$.current <- if (length(ls(StateEnv)))
            StateEnv[[ ls(StateEnv)[1] ]] else NULL
    }
}

callArg <- function(playState, arg, expr, eval = TRUE, data = NULL)
{
    if (missing(expr) == missing(arg)) stop("give 'arg' or 'expr'")
    if (!missing(expr)) arg <- substitute(expr)
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

"callArg<-" <- function(playState, arg, expr, value)
{
    if (missing(expr) == missing(arg)) stop("give 'arg' or 'expr'")
    if (!missing(expr)) arg <- substitute(expr)
    if (is.symbol(arg)) arg <- as.character(arg)
    if (is.null(arg)) return()
    ## instantiate implicit lists as language objects (so deparse is pretty)
#    if (is.language(arg)) {
#        xbits <- strsplit(deparseOneLine(arg), "$", fixed=TRUE)[[1]]
#        for (i in seq_len(length(xbits) - 1)) {
#            implicitList <- parse(text=paste(xbits[1:i], collapse="$"))[[1]]
#            if (is.null(callArg(playState, implicitList))) {
#                callArg(playState, implicitList) <- quote(list())
#            }
#        }
#    }
    getx <- if (is.numeric(arg)) paste('[[', arg+1, ']]', sep="")
    else if (is.character(arg)) paste("$", arg, sep="")
    else paste("$", deparseOneLine(arg), sep="")
    mainCall <- mainCall(playState)
    zap <- parse(text=paste("mainCall", getx, sep=""))[[1]]
    zap <- call("<-", zap, quote(value))
    eval(zap, enclos=parent.frame())
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
    main.function <- playState$.args$main.function
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
    playState$main.call.index <- main.call.index
    ## check whether the called function accepts arguments
    playState$accepts.arguments <- !is.null(playState$main.call.index)
        #((typeof(callFun) == "closure") && !is.null(formals(callFun)))
}

playFreezeGUI <- function(playState)
    playSetFreezeGUI(playState, TRUE)

playThawGUI <- function(playState)
    playSetFreezeGUI(playState, FALSE)

playSetFreezeGUI <- function(playState, frozen)
{
    playState$now.interacting <- if (frozen) TRUE
    with(playState$widgets, {
        topToolbar["sensitive"] <- !frozen
        leftToolbar["sensitive"] <- !frozen
        bottomToolbar["sensitive"] <- !frozen
        rightToolbar["sensitive"] <- !frozen
        pageScrollBox["sensitive"] <- !frozen
        timeScrollBox["sensitive"] <- !frozen
                                        #callToolbar["sensitive"] <- !frozen
    })
    playState$win$getWindow()$setCursor(
      if (frozen) gdkCursorNew("watch") else NULL)
}

blockRedraws <- function(expr, playState = playDevCur())
{
    oval <- playState$skip.redraws
    playState$skip.redraws <- TRUE
    da <- playState$widgets$drawingArea
    da$setSizeRequest(da$getAllocation()$width, da$getAllocation()$height)
                                        #playState$win$setGeometryHints(da, list(max.width=myW, min.width=myW,
                                        #	max.height=myH, min.height=myH))
                                        #da$window$freezeUpdates() # hmm
    foo <- try(eval.parent(substitute(expr)))
                                        #da$window$thawUpdates()
                                        #playState$win$setGeometryHints(da, list())
    da$setSizeRequest(-1, -1)
    playState$skip.redraws <- oval
    foo
}

playPrompt <- function(playState, text=NULL)
{
    with(playState$widgets, {
        if (is.null(text)) {
            ## hide the prompt widget
            promptLabel$setMarkup("")
            promptBox["sensitive"] <- FALSE
            if (playState$show.call) {
                callToolbar$show()
                promptBox$hide()
            }
            playThawGUI(playState)
            return(invisible())
        }
        ## show the prompt widget
        promptBox["sensitive"] <- TRUE
        if (playState$show.call) {
            promptBox$show()
            callToolbar$hide()
        }
        playFreezeGUI(playState)
        ## set the prompt text
        promptLabel$setMarkup(paste("<big><b>",
                                    toString(text), "</b></big>"))
    })
    invisible()
}

rawXLim <- function(playState, space="plot")
    rawXYLim(playState, space=space)$x


rawYLim <- function(playState, space="plot")
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
    playDo(playState, space=space, list(
                      x=convertX(unit(0:1, "npc"), "native", valueOnly=TRUE),
                      y=convertY(unit(0:1, "npc"), "native", valueOnly=TRUE)))
}

"rawXLim<-" <- function(playState, value)
{
    setRawXYLim(playState, value, "x")
    playState
}

"rawYLim<-" <- function(playState, value)
{
    setRawXYLim(playState, value, "y")
    playState
}

is.somesortoftime <- function(x) {
  inherits(x, "Date") ||
  inherits(x, "POSIXt") ||
  inherits(x, "yearmon") ||
  inherits(x, "yearqtr")
}

setRawXYLim <- function(playState, x, x.or.y=c("x", "y"))
{
    playDevSet(playState)
    x.or.y <- match.arg(x.or.y)
    ## round digits conservatively
    x <- signif(x, digits=8)
    if (playState$is.lattice) {
        ## TODO: this really sucks
        x.panel <- xyData(playState, space="page")[[x.or.y]]
        ## set factor labels explicitly, othewise is coerced to numeric
        if (is.factor(x.panel)) {
            if (is.null(callArg(playState, expr=scales[[x.or.y]]$labels))) {
                callArg(playState, expr=scales[[x.or.y]]$labels) <- levels(x.panel)
                callArg(playState, expr=scales[[x.or.y]]$at) <- 1:nlevels(x.panel)
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
          isExtended <- switch(x.or.y,
                               x = playState$trellis$x.scales$axs == "r",
                               y = playState$trellis$y.scales$axs == "r")
          f <- lattice.getOption("axis.padding")$numeric
          if (isExtended) x <- shrinkrange(x, f=f)
          ## round digits conservatively
          x <- signif(x, digits=8)
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
      ## round digits conservatively
      x <- signif(x, digits=8)
    }
    ## convert back from log scale if required
    x <- spaceCoordsToDataCoordsXY(playState, x, x.or.y=x.or.y)
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

## note space="page" means the root viewport
playDo <- function(playState, expr, space="plot", clip.off=FALSE)
{
    playDevSet(playState)
    cur.vp <- current.vpPath()
    upViewport(0) ## go to root viewport
    on.exit(expression()) ## placeholder
    if (space != "page") {
        ## user / plot coordinates
        if (!is.null(playState$viewport)) {
            ## grid graphics plot
            depth <- try(downViewport(playState$viewport[[space]]))
            if (inherits(depth, "try-error")) {
                stop(paste("Viewport", playState$viewport, "not found"))
            }
            on.exit(upViewport(depth), add=TRUE)
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
            depth <- try(downViewport(myVp))
            on.exit(upViewport(depth), add=TRUE)
            ## TODO: should focus panel or just go to viewport?
            ## (if focus, will destroy any previous focus)
            ##trellis.focus("panel", myCol, myRow, highlight=FALSE)
            ##on.exit(trellis.unfocus())
        }
        else {
            ## base graphics
            space <- "plot"
            if (clip.off) space <- "plot.clip.off"
            depth <- downViewport(playState$baseViewports[[space]])
            on.exit(upViewport(depth), add=TRUE)
        }
    }
    if (inherits(depth, "try-error")) return()
    if (!is.null(cur.vp)) on.exit(downViewport(cur.vp), add=TRUE)
    ## do the stuff and return the result
    ##if (is.list(stuff)) lapply(stuff, eval, parent.frame(), playState$env)
    ##else eval(stuff, parent.frame(), playState$env)
    eval(substitute(expr), parent.frame(), playState$env)
}

playSelectData <- function(playState, prompt="Click or drag to select data points.")
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
        ok <- ((min(coords$x) <= data$x) & (data$x <= max(coords$x))
               & (min(coords$y) <= data$y) & (data$y <= max(coords$y)))
        which <- which(ok)
    }
    c(list(which=which, x=data$x[which], y=data$y[which],
           subscripts=data$subscripts[which],
           pos=pos, is.click=foo$is.click),
      foo)
}

playPointInput <- function(playState, prompt="Click on the plot")
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
        coords <- deviceCoordsToSpace(playState, dc$x, dc$y, space=space)
    }
    list(coords=coords, space=space, dc=dc, ndc=ndc, modifiers=modifiers)
}

playPolyInput <- function(playState, prompt="Click points to define a region; right-click to end")
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
    ## TODO...
    repeat {
    }
}

playLineInput <- function(playState, prompt="Click and drag to define a line")
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
    function(playState,
             prompt="Click and drag to define a rectangular region",
             scales=c("x", "y"))
{
    scales <- match.arg(scales, several.ok=TRUE)
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
    playClickOrDrag(playState, x0=xy0$x, y=xy0$y, shape="rect", scales=scales)
}

## assumes that the mouse button has already been pressed
## converts into user coordinates
playClickOrDrag <-
    function(playState, x0, y0,
             shape=c("rect", "line"),
             scales=c("x", "y"))
{
    playDevSet(playState)
    foo <- handleClickOrDrag(playState$widgets$drawingArea,
                             x0=x0, y0=y0, shape=shape, scales=scales)
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
        xy0 <- deviceCoordsToSpace(playState, dc$x[1], dc$y[1], space=space)
        xy1 <- deviceCoordsToSpace(playState, dc$x[2], dc$y[2], space=space)
        coords <- list(x=c(xy0$x, xy1$x), y=c(xy0$y, xy1$y))
    }
    foo$coords <- coords
    foo$space <- space
    foo
}

## assumes that the mouse button has already been pressed
handleClickOrDrag <-
    function(da, x0, y0,
             shape=c("rect", "line"),
             scales=c("x", "y"))
{
    shape <- match.arg(shape)
    scales <- match.arg(scales, several.ok=TRUE)
    ## px0 is the original click location
    px0 <- list(x=x0, y=y0)
    da.w <- da$getAllocation()$width
    da.h <- da$getAllocation()$height
    buf <- gdkPixbufGetFromDrawable(src=da$window, src.x=0, src.y=0,
                                    dest.x=0, dest.y=0, width=da.w, height=da.h)
    if (is.null(buf)) stop("Could not make pixbuf")
    gc <- gdkGCNew(da$window)
    gc$copy(da[["style"]][["blackGc"]])
    gc$setRgbFgColor(gdkColorParse("black")$color)
    gc$setRgbBgColor(gdkColorParse("white")$color)
    gc$setLineAttributes(line.width=1, line.style=GdkLineStyle["double-dash"],
                         cap.style=GdkCapStyle["butt"], join.style=GdkJoinStyle["miter"])
    gc$setDashes(c(8, 4))
    px00 <- px0
    px00.prev <- px0
    release_handler <- function(widget, event, env) {
        ## mouse button was released
        env$px1 <- list(x=event$x, y=event$y)
        return(TRUE)
    }
    expose_handler <- function(widget, event, env) {
        area <- event$area
        gdkDrawPixbuf(event$window, pixbuf=env$buf,
                      src.x=area$x, src.y=area$y, dest.x=area$x, dest.y=area$y,
                      width=area$width, height=area$height)
        xx <- range(c(env$px0$x, env$px00$x))
        yy <- range(c(env$px0$y, env$px00$y))
        ## restrict rectangle to x or y scales, according to `scales`
        if (!("x" %in% scales)) xx <- c(-1, da.w)
        if (!("y" %in% scales)) yy <- c(-1, da.h)
        wd <- xx[2] - xx[1]
        ht <- yy[2] - yy[1]
        switch(env$shape,
               line = gdkDrawLine(event$window, gc=env$gc,
                 x1=env$px0$x, y1=env$px0$y,
                 x2=env$px00$x, y2=env$px00$y),
               rect = gdkDrawRectangle(event$window, gc=env$gc, filled=FALSE,
                 x=xx[1], yy[1], width=wd, height=ht)
               )
        return(TRUE) ## stop event here
    }
    tmpSigE <- gSignalConnect(da, "expose-event", expose_handler, data=environment())
    tmpSigR <- gSignalConnect(da, "button-release-event", release_handler, data=environment())
    init_time <- proc.time()[3]
    repeat {
        ## px1 is the final drag location, set by event handler
        if (exists("px1", inherits=FALSE)) break
        px00.prev <- px00
        px00 <- da$window$getPointer()
        ## check that pointer is inside the window
        ## TODO -- fails on linux?
#        if (is.null(px00$retval)) break
        if ((as.flag(px00$mask) & GdkModifierType["button1-mask"]) == 0) {
            ## mouse button was released
            px1 <- px00
            break
        }
        xx <- range(c(px0$x, px00$x, px00.prev$x))
        yy <- range(c(px0$y, px00$y, px00.prev$y))
        ## restrict rectangle to x or y scales, according to `scales`
        if (!("x" %in% scales)) xx <- c(-1, da.w)
        if (!("y" %in% scales)) yy <- c(-1, da.h)
        wd <- xx[2] - xx[1] + 2
        ht <- yy[2] - yy[1] + 2
        da$window$invalidateRect(list(x=xx[1], y=yy[1], width=wd, height=ht),
                                 invalidate.children=FALSE)
        ## try to force redraw
        gdkWindowProcessAllUpdates()
        while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)

    }
    gSignalHandlerDisconnect(da, tmpSigR)
    gSignalHandlerDisconnect(da, tmpSigE)
    ## check for modifier keys
    ptrInfo <- da$window$getPointer()
    modifiers <- as.flag(0)
    if (!is.null(ptrInfo$retval))
        modifiers <- as.flag(ptrInfo$mask)
    ## clean up
    da$window$invalidateRect(invalidate.children=FALSE)
    if (!exists("px1", inherits=FALSE)) return(NULL)
    dc <- list(x=c(px0$x, px1$x), y=c(px0$y, px1$y))
    ## was it a click or drag? (click = no slower than 1/4 second)
    is.click <- (proc.time()[3] - init_time) <= 0.25
    ## alternative criteria for click: moved less than 10 pixels
    is.click <- is.click ||
                ((abs(diff(dc$x)) < 10) && (abs(diff(dc$y)) < 10))
    ndc <- list(x=dc$x / da.w, y=(da.h - dc$y) / da.h)
    list(dc=dc, ndc=ndc, is.click=is.click, modifiers=modifiers)
}

xyCoords <- function(playState, space="plot")
{
    foo <- xyData(playState, space=space)
    foo$x <- as.numeric(foo$x)
    foo$y <- as.numeric(foo$y)
    foo
}

xyData <- function(playState, space="plot")
{
    if (!is.null(playState$data.points)) {
        return(xy.coords_with_class(playState, playState$data.points))
    }
    if (playState$is.lattice) {
        if (space == "page") {
            ## data from all packets
            return(do.call(rbind,
                           lapply(playState$trellis$panel.args, as.data.frame)
                           ))
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
        ## if 1st arg is formula, 2nd is `data`
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
            x <- data.matrix(x)
            if (ncol(x) == 1) {
                y <- x[, 1]
                x <- seq_along(y)
            }
            else {
                colnames <- dimnames(x)[[2]]
                y <- x[, 2]
                x <- x[, 1]
            }
        }
        else if (is.list(x)) {
            y <- x[["y"]]
            x <- x[["x"]]
        }
        else {
            if (is.factor(x)) x <- as.numeric(x)
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
        scalesArg <- callArg(playState, "scales")
        if (!is.null(scalesArg[[x.or.y]]$log)) {
            logBase <- latticeLogBase(scalesArg[[x.or.y]]$log)
            if (!is.na(logBase)) return(logBase)
        } else {
            logBase <- latticeLogBase(scalesArg$log)
            if (!is.na(logBase)) return(logBase)
        }
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

latticeLogBase <- function(x)
{
    x <- eval(x)
    if (is.null(x) || identical(x, FALSE)) return(NA)
    if (isTRUE(x)) return(10)
    if (identical(x, "e")) return(exp(1))
    x
}

whichSpace <- function(playState, x.device, y.device)
{
    for (space in names(playState$deviceToSpace)) {
        spaceFun <- playState$deviceToSpace[[space]]
        xy <- spaceFun(x.device, y.device)
        if (xy$inside) return(space)
    }
    ## return "page" if not inside any of the viewports
    return("page")
}

deviceCoordsToSpace <- function(playState, x.device, y.device, space = "plot")
{
    if (space == "page") return(list(x=x.device, y=y.device))
    if (playState$is.lattice && (space == "plot")) {
        space <- packet.number()
        if (length(space) == 0) {
            packets <- trellis.currentLayout(which="packet")
            if (sum(packets > 0) > 1) stop("space not well specified")
            space <- packets[packets > 0][1]
        }
        space <- paste("packet", space)
    }
    spaceFun <- playState$deviceToSpace[[space]]
    spaceFun(x.device, y.device)
}

## this is called by playReplot
## makes a function to transform device native x, y (i.e. pixels)
## into plot coordinates (current viewport native coords if is.grid=T)
deviceToUserCoordsFunction <- function(is.grid = TRUE)
{
    ## get device size in pixels
    vp <- current.vpPath()
    upViewport(0) ## go to root viewport
    dpx <- abs(c(diff(convertX(unit(0:1, "npc"), "native", valueOnly=TRUE)),
                 diff(convertY(unit(0:1, "npc"), "native", valueOnly=TRUE))))
    ## pixels per inch
    dpi <- convertX(unit(1, "inches"), "native", valueOnly=TRUE)
    if (length(vp) > 0) downViewport(vp)
    ## device size in inches
    din <- dpx / dpi
    if (is.grid) {
        ## using current viewport
        xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
        ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
        plot.in <-    convertX(unit(1, "npc"), "inches", valueOnly=T)
        plot.in[2] <- convertY(unit(1, "npc"), "inches", valueOnly=T)
        transform <- solve(grid::current.transform())
        function(x.px, y.px) {
            n <- max(length(x.px), length(y.px))
            x.px <- rep(x.px, length=n)
            y.px <- rep(y.px, length=n)
            y.px <- dpx[2] - y.px ## y scale origin at bottom
            ## TODO: handle vectors directly, this loop is a hack
            x_npc <- y_npc <- numeric(0)
            for (i in seq_along(x.px)) {
                pos.ndc <- c(x.px[i], y.px[i]) / dpx
                pos.din <- c(din * pos.ndc, 1)
                pos.in <- (pos.din %*% transform)
                pos.in <- (pos.in / pos.in[3])
                x_npc[i] <- pos.in[1] / plot.in[1]
                y_npc[i] <- pos.in[2] / plot.in[2]
            }
            x <- xlim[1] + x_npc * diff(xlim)
            y <- ylim[1] + y_npc * diff(ylim)
            inside <- ((min(xlim) <= x) & (x <= max(xlim))
                       & (min(ylim) <= y) & (y <= max(ylim)))
            ## note: do not unlog here, might want raw coords
            list(x=x, y=y, inside=inside)
        }
    } else {
        xlim <- par("usr")[1:2]
        ylim <- par("usr")[3:4]
        xmar <- (par("mai") + par("omi"))[c(2,4)]
        ymar <- (par("mai") + par("omi"))[c(1,3)]
        plot.px <- par("pin") * dpi
        xmar.px <- xmar * dpi
        ymar.px <- ymar * dpi
        xlog <- par("xlog")
        ylog <- par("ylog")
        function(x.px, y.px) {
            n <- max(length(x.px), length(y.px))
            x.px <- rep(x.px, length=n)
            y.px <- rep(y.px, length=n)
            y.px <- dpx[2] - y.px ## y scale origin at bottom
            x_npc <- (x.px - xmar.px[1]) / plot.px[1]
            y_npc <- (y.px - ymar.px[1]) / plot.px[2]
            x <- xlim[1] + x_npc * diff(xlim)
            y <- ylim[1] + y_npc * diff(ylim)
            inside <- ((min(xlim) <= x) & (x <= max(xlim))
                       & (min(ylim) <= y) & (y <= max(ylim)))
            ## note: do not unlog here, might want raw coords
            list(x=x, y=y, inside=inside)
        }
    }
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

