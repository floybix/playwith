## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### TIME.MODE

toolConstructors$time.mode <- function(playState)
{
    with (playState$widgets, {
        timeScrollbar["sensitive"] <- playState$time.mode
        timeEntry["sensitive"] <- playState$time.mode
    })
    if (is.null(playState$time.vector)) {
        if (playState$accepts.arguments == FALSE) return(NA)
    }
    quickTool(playState,
              label = "Time mode",
              icon = "gtk-media-forward-ltr",
              tooltip = "Time mode: scroll along the x-axis",
              f = time.mode_handler,
              post.plot.action = time.mode_postplot_action,
              isToggle = TRUE)
}

time.mode_handler <- function(widget, playState)
{
    playState$time.mode <- widget["active"]
    blockRedraws(with (playState$widgets, {
        timeScrollBox["visible"] <- TRUE
        timeScrollbar["sensitive"] <- playState$time.mode
        timeEntry["sensitive"] <- playState$time.mode
    }))
    ## store data range and class
    if (playState$time.mode) {
        if (is.null(playState$time.vector)) {
            xy <- xyData(playState, space="page")
            playState$time.mode.x.range <- extendrange(as.numeric(xy$x))
            playState$time.mode.x.attr <- attributes(xy$x)
        }
    }
    ## update scrollbar etc
    time.mode_postplot_action(widget, playState)
}

time.mode_postplot_action <- function(widget, playState)
{
    if (playState$time.mode == FALSE) return()
    if (widget["active"] == FALSE) {
        widget["active"] <- TRUE ## triggers update
        return()
    }
    blockRedraws({
        widg <- playState$widgets
        if (!is.null(playState$time.vector)) {
            x.pos <- playState$env$cur.index
            x.max <- length(playState$time.vector)
            x.jump <- playState$time.mode.page.incr
            if (is.null(x.jump)) x.jump <- round(log2(x.max))
            cur.time <- playState$env$cur.time
            widg$timeEntry["text"] <- toString(cur.time)
            widg$timeScrollbar["adjustment"] <-
                gtkAdjustment(value=x.pos, lower=1, upper=x.max+1,
                              step.incr=1, page.incr=x.jump, page.size=1)
            widg$timeScrollbar$setValue(x.pos) ## need this (bug?)
            return()
        }
        x.range <- playState$time.mode.x.range
        x.lim <- rawXLim(playState)
        x.page <- abs(diff(x.lim))
        x.page <- min(x.page, abs(diff(x.range)))
        x.pos <- min(x.lim)
        x.pos <- max(x.pos, min(x.range))
        x.pos <- min(x.pos, max(x.range))
        ## format x limits for text box
        xlim <- signif(x.lim, 6)
        class(x.lim) <- playState$time.mode.x.attr$class
        if ("POSIXt" %in% class(x.lim))
            attr(x.lim, "tz") <- playState$time.mode.x.attr$tz
        if ("factor" %in% class(x.lim))
            attr(x.lim, "levels") <- playState$time.mode.x.attr$levels
                                        #mostattributes(x.lim) <- playState$time.mode.x.attr
        widg$timeEntry["text"] <- paste(format(x.lim), collapse=" to ")
        ## set up scrollbar
        widg$timeScrollbar["adjustment"] <-
            gtkAdjustment(value=x.pos, lower=min(x.range), upper=max(x.range),
                          step.incr=x.page/2, page.incr=x.page, page.size=x.page)
        widg$timeScrollbar$setValue(x.pos) ## need this (bug?)
    })
}

time.mode_scrollbar_handler <- function(widget, playState)
{
    newLim <- widget$getValue()
    if (!is.null(playState$time.vector)) {
        newLim <- round(newLim)
        playState$env$cur.index <- newLim
        playState$env$cur.time <- playState$time.vector[newLim]
        playReplot(playState)
        return()
    }
    newLim[2] <- newLim + widget["adjustment"]["page-size"]
    if (widget["adjustment"]["page-size"] == 0) stop()
                                        #oldLim <- rawXLim(playState)
                                        #if (min(oldLim) == min(newLim)) return()
    newLim <- signif(newLim, 8)
    rawXLim(playState) <- newLim
    playReplot(playState)
}

time.mode_entry_handler <- function(widget, playState)
{
    if (!is.null(playState$time.vector)) {
        newLim <- widget["text"]
        time.vector <- playState$time.vector
        max.x <- length(time.vector)
        cls <- class(time.vector)
        if ("POSIXt" %in% cls) newLim <- try(as.POSIXct(newLim))
        else if ("Date" %in% cls) newLim <- try(as.Date(newLim))
        if (inherits(newLim, "try-error")) {
            ## treat it as an index into time.vector
            cur.index <- try(as.integer(widget["text"]), silent=TRUE)
            if (inherits(cur.index, "try-error")) {
                ## give up
                gmessage.error(conditionMessage(newLim))
                return()
            }
        } else {
            newLim <- as.numeric(newLim)
            cur.index <- findInterval(newLim, time.vector)
        }
        cur.index <- max(1, min(max.x, cur.index))
        playState$env$cur.index <- cur.index
        playState$env$cur.time <- time.vector[cur.index]
        playReplot(playState)
        return()
    }
    newLim <- strsplit(widget["text"], " to ")[[1]]
    if ((length(newLim) != 2)) {
        gmessage.error("Give bounds in form \"LOWER to UPPER\".")
        return()
    }
    x.attr <- playState$time.mode.x.attr
    cls <- x.attr$class
    if ("POSIXt" %in% cls) newLim <- as.POSIXct(newLim)
    else if ("Date" %in% cls) newLim <- as.Date(newLim)
    else if ("integer" %in% cls) newLim <- as.integer(newLim)
    callArg(playState, xlim) <- as.numeric(newLim)
    playReplot(playState)
}
