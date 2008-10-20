# playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer


playGetIDs <-
    function(playState = playDevCur(),
             type = c("labelled", "brushed"),
             labels = FALSE)
{
    type <- match.arg(type, several.ok = TRUE)
    ids.brushed <- unlist(playState$linked$ids)
    ids.lab <- do.call(rbind, playState$ids)$subscripts
    ids <- NULL
    if ("labelled" %in% type) ids <- ids.lab
    if ("brushed" %in% type) ids <- c(ids, ids.brushed)
    ids <- unique(sort(ids))
    if (labels) playState$labels[ids] else ids
}

playSetIDs <-
    function(playState = playDevCur(),
             value,
             type = "brushed",
             space = "plot",
             add = FALSE,
             redraw = NA,
             pos = 1)
{
    playStoreUndo(playState)
    type <- match.arg(type, c("labelled", "brushed"))
    if (is.logical(value))
        value <- which(value)
    if (type == "brushed") {
        if (add == FALSE) { ## replace
            if (length(playState$linked$ids)) ## exists
                if (is.na(redraw)) redraw <- TRUE
            playState$linked$ids <- list()
        }
        i <- length(playState$linked$ids) + 1
        playState$linked$ids[[i]] <- value
        if (is.na(redraw)) {
            ## draw without a full redraw
            drawLinkedLocal(playState)
            updateLinkedSubscribers(playState)
        }
    }
    if (type == "labelled") {
        if (add == FALSE) { ## replace
            if (length(playState$ids)) ## exists
                if (is.na(redraw)) redraw <- TRUE
            playState$ids <- list()
        }
        ids.new <- data.frame(subscripts = value, pos = pos)
        i <- length(playState$ids) + 1
        playState$ids[[i]] <- ids.new
        names(playState$ids)[i] <- space
        if (is.na(redraw)) {
            ## draw without a full redraw
            drawLabelsInSpace(playState, subscripts = value,
                              space = space, pos = pos)
            #drawLabels(playState)
        }
    }
    if (isTRUE(redraw)) {
        ## full redraw
        playReplot(playState)
        ## redraw linked plots
        if (type == "brushed")
            updateLinkedSubscribers(playState, redraw = TRUE)
    }
    ## update other tool states
    updateAnnotationActionStates(playState)
    invisible()
}

playClear <-
    function(playState = playDevCur(),
             type = c("annotations", "labelled", "brushed"),
             redraw = TRUE)
{
    playStoreUndo(playState)
    type <- match.arg(type, several.ok = TRUE)
    ## remove types that are empty
    type <- c(if (("labelled" %in% type) &&
                  length(playState$ids)) "labelled",
              if (("annotations" %in% type) &&
                  length(playState$annotations)) "annotations",
              if (("brushed" %in% type) &&
                  length(playState$linked$ids)) "brushed"
              )
    if (length(type) == 0) return()
    for (x in type) {
        if (x == "labelled") {
            if (length(playState$ids) == 0)
            playState$ids <- list()
        } else if (x == "annotations") {
            playState$annotations <- list()
        } else if (x == "brushed") {
            playState$linked$ids <- list()
        }
    }
    ## redraw
    if (redraw) {
        playReplot(playState)
        ## update linked plots
        if ("brushed" %in% type)
            updateLinkedSubscribers(playState, redraw = TRUE)
    }
    updateAnnotationActionStates(playState)
    invisible()
}

playUndo <- function(playState = playDevCur())
{
    if (isBasicDeviceMode(playState)) {
        ## basic device mode: only one stored display
        redoPlot <- recordPlot()
        try(replayPlot(playState$tmp$recorded.plot))
        generateSpaces(playState)
        playState$tmp$recorded.plot <- redoPlot
        return(invisible())
    }
    i <- length(playState$tmp$undoStack)
    if (i == 0) return()
    state <- playState$tmp$undoStack[[i]]
    anyLinked <- !identical(state$brushed, playState$linked$ids)
    length(playState$tmp$undoStack) <- (i - 1)
    playState$ids <- state$ids
    playState$annotations <- state$annotations
    playState$linked$ids <- state$brushed
    updateAnnotationActionStates(playState)
    ## redraw
    playReplot(playState)
    if (anyLinked)
        updateLinkedSubscribers(playState, redraw = TRUE)
    invisible()
}

playStoreUndo <- function(playState = playDevCur())
{
    if (isBasicDeviceMode(playState)) {
        ## basic device mode: only one stored display
        playState$tmp$recorded.plot <- try(recordPlot())
        return(invisible())
    }
    state <- list()
    state$ids <- playState$ids
    state$annotations <- playState$annotations
    state$brushed <- playState$linked$ids
    i <- length(playState$tmp$undoStack) + 1
    playState$tmp$undoStack[[i]] <- state
    if (i > playwith.getOption("undo.levels"))
        playState$tmp$undoStack <- playState$tmp$undoStack[-1]
    updateAnnotationActionStates(playState)
    invisible()
}

playAnnotate <- function(playState, annot, space = "plot")
{
    playStoreUndo(playState)
    ## store annotation
    i <- length(playState$annotations) + 1
    playState$annotations[[i]] <- as.expression(annot)
    names(playState$annotations)[i] <- space
    ## draw it
    playDo(playState, annot, space = space)
    ## update other tool states
    updateAnnotationActionStates(playState)
    invisible()
}

playDo <- function(playState, expr, space = "plot",
                   clip.off = !isTRUE(playState$clip.annotations),
                   return.code = FALSE)
{
    playDevSet(playState)
    vpName <- NULL
    if (space == "page") {
        ## normalised device coordinates
        vpName <- "pageAnnotationVp"
    } else {
        ## user / plot coordinates
        if (!is.null(playState$viewport)) {
            ## grid graphics plot
            vpName <- playState$viewport[[space]]
            if (inherits(vpName, "viewport") || inherits(vpName, "vpPath"))
                vpName <- vpName$name
        }
        else if (playState$is.lattice) {
            ## lattice plot
            packets <- playState$tmp$currentLayout
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
            vpName <- trellis.vpname("panel", myCol, myRow, clip.off=clip.off)
            ## NOTE: a panel is not in focus here (as in trellis.focus)
            ## because that would destroy any previous focus state
            ## -- if focus is required, do that before calling playDo.
        }
        else {
            ## base graphics
            space <- "plot"
            if (clip.off) space <- "plot.clip.off"
            vpName <- playState$tmp$baseVps[[space]]$name
        }
    }
    if (return.code) {
        return(c(as.expression(call("seekViewport", vpName)),
                 expr))
    }
    ## store current viewport and restore it when finished
    cur.vp <- current.vpPath()
    on.exit({
        upViewport(0)
        if (!is.null(cur.vp)) downViewport(cur.vp)
    })
    ## do the stuff and return the result
    seekViewport(vpName)
    eval(expr, parent.frame(), playState$env)
}

playUnlink <- function(playState = playDevCur())
{
    oldlinked <- playState$linked
    newlinked <- new.env(parent = baseenv())
    newlinked$ids <- playState$linked$ids
    newlinked$subscribers <- list(playState)
    playState$linked <- newlinked
    ## remove self from (old) list of subscribers
    for (i in seq_along(oldlinked$subscribers)) {
        otherPlayState <- oldlinked$subscribers[[i]]
        if (identical(otherPlayState$ID, playState$ID)) {
            oldlinked$subscribers <- oldlinked$subscribers[-i]
            break
        }
    }
    ## update action states
    for (x in oldlinked$subscribers)
        updateGlobalActions(x)
    updateGlobalActions(playState)
}

