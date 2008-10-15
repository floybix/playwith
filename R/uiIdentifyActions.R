## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer


initIdentifyActions <- function(playState)
{
    playState$tmp$identify.ok <- FALSE
    if (isBasicDeviceMode(playState)) return()
    hasArgs <- playState$accepts.arguments
    isLatt <- playState$is.lattice
    isBase <- !isLatt && is.null(playState$viewport)
    isBaseMulti <- isBase && any(par("mfrow") > 1)
    ## detect known plots that this will not work with
    if (is.null(playState$data.points)) {
        if (!hasArgs) return()
        if (isBaseMulti) return()
        ## lattice package:
        if (playState$callName %in%
            c("histogram", "densityplot", "barchart")) return()
        if (playState$callName %in% "marginal.plot") return()
        ## from graphics package:
        if (playState$callName %in%
            c("hist", "barplot", "spineplot", "mosaic",
              "assocplot", "fourfoldplot",
              "coplot", "persp", "pie")) return()
    }
    labels <- playState$.args$labels
    ## try to guess labels if they were not given
    if (is.null(labels)) {
        tmp.data <- getDataArg(playState)
        if (!is.null(tmp.data) &&
            !inherits(tmp.data, "list") &&
            !is.environment(tmp.data))
        {
            ## data arg, probably a data.frame
            labels <- case.names(tmp.data)
        } else {
            ## no useful data arg; take arg 1 instead
            tmp.x <- callArg(playState, 1)
            if (inherits(tmp.x, "formula")) {
                ## get left-most term in RHS of formula
                xObj <- if (length(tmp.x) == 2)
                    tmp.x[[2]] else tmp.x[[3]]
                while (is.call(xObj) && toString(xObj[[1]]) %in%
                       c("|", "*", "+"))
                    xObj <- xObj[[2]]
                xObj <- eval(xObj, tmp.data, playState$env)
                labels <- case.names(xObj)
            } else {
                ## first arg is an object, not a formula
                labels <- case.names(tmp.x)
            }
        }
    }
    playState$labels <- labels
    playState$tmp$identify.ok <- TRUE
}

updateIdentifyActions <- function(playState)
{
    aGroup <- playState$actionGroups[["PlotActions"]]
    ## Identify etc
    canIdent <- playState$tmp$identify.ok
    aGroup$getAction("Identify")$setSensitive(canIdent)
    aGroup$getAction("IdTable")$setSensitive(canIdent)
    aGroup$getAction("FindLabels")$setSensitive(canIdent)
    hasIDs <- ((length(playState$ids) > 0) ||
               (length(playState$linkeds$ids) > 0))
    aGroup$getAction("SaveIDs")$setSensitive(hasIDs)
    ## Brush
    aGroup$getAction("Brush")$setSensitive(canIdent)
    ## draw persistent labels and brushed points
    if (canIdent) {
        drawLabels(playState)
        drawLinkedLocal(playState)
    }
}

drawLabels <- function(playState, return.code = FALSE)
{
    playDevSet(playState)
    theCode <- expression()
    ## group by space
    spaces <- names(playState$ids)
    for (space in unique(spaces)) {
        items <- playState$ids[spaces == space]
        idInfo <- do.call(rbind, items)
        expr <- drawLabelsInSpace(playState, subscripts = idInfo$subscripts,
                           space = space, pos = idInfo$pos,
                           return.code = return.code)
        if (return.code)
            theCode <- c(theCode, expr)
    }
    theCode
}

drawLabelsInSpace <- function(playState, subscripts, space = "plot",
                              pos = 1, return.code = FALSE)
{
    data <- xyCoords(playState, space=space)
    if (length(data$x) == 0) return()
    if (length(data$y) == 0) return()
    ## convert to log scale if necessary
    data <- dataCoordsToSpaceCoords(playState, data)
    if (!is.null(data$subscripts)) {
        ## 'data' is a subset given by data$subscripts,
        ## so need to find which ones match the label subscripts
        #which <- findInterval(subscripts, data$subscripts)
        which <- match(subscripts, data$subscripts, 0)
        x <- data$x[which] ## TODO -- matrix case!
        y <- data$y[which]
    } else {
        ## 'data' (x and y) is the whole dataset
        x <- data$x[subscripts]
        y <- data$y[subscripts]
    }
    labels <- playState$labels[subscripts]
    pos <- rep(pos, length=length(labels))
    offset <- as.numeric(playState$label.offset)
    annots <- expression()
    for (i in seq_along(labels)) {
        annots[[i]] <- call("panel.usertext", x[i], y[i],
                            labels[i], pos = pos[i])
        if (offset != 0.5)
            annots[[i]]$offset <- offset
    }
    playDo(playState, annots, space = space,
           return.code = return.code)
}

drawLinkedLocal <- function(playState, return.code = FALSE)
{
    ## draw linked brushed points
    theCode <- expression()
    subscripts <- unlist(playState$linked$ids)
    if (length(subscripts) == 0) return(theCode)
    subscripts <- unique(sort(subscripts))
    playDevSet(playState)
    for (space in playState$spaces) {
        data <- xyCoords(playState, space = space)
        if (length(data$x) == 0) next
        if (length(data$y) == 0) next
        ## convert to log scale if necessary
        data <- dataCoordsToSpaceCoords(playState, data)
        if (!is.null(data$subscripts)) {
            ## 'data' is a subset given by data$subscripts,
            ## so need to find which ones match the label subscripts
            which <- match(subscripts, data$subscripts, 0)
        } else {
            ## 'data' (x and y) is the whole dataset
            which <- subscripts
        }
        x <- if (is.matrix(data$x)) data$x[,which] else data$x[which]
        y <- if (is.matrix(data$y)) data$y[,which] else data$y[which]
        if (length(x) == 0) next
        ## TODO: parallel -- lines
        annot <- call("panel.brushpoints", x, y)
        expr <- playDo(playState, annot, space = space,
                       return.code = return.code)
        if (return.code)
            theCode <- c(theCode, expr)
    }
    theCode
}

updateLinkedSubscribers <- function(playState, redraw = FALSE)
{
    whichDead <- NULL
    for (i in seq_along(playState$linked$subscribers)) {
        otherPlayState <- playState$linked$subscribers[[i]]
        if (!identical(otherPlayState$ID, playState$ID)) {
            ## first check that this subscriber is still alive
            if (!inherits(otherPlayState$win, "GtkWindow")) {
                whichDead <- c(whichDead, i)
                next
            }
            ## trigger draw / redraw
            if (redraw) playReplot(otherPlayState)
            else drawLinkedLocal(otherPlayState)
        }
    }
    if (length(whichDead))
        playState$linked$subscribers <-
            playState$linked$subscribers[-whichDead]
}

identifyCore <- function(playState, foo, remove = FALSE)
{
    ## TODO: if (remove)
    if (!isTRUE(playState$tmp$identify.ok)) return()
    if (is.null(playState$labels)) return()
    if (is.null(foo$coords)) return()
    space <- foo$space
    data <- xyCoords(playState, space=foo$space)
    ## convert to log scale if necessary
    data <- dataCoordsToSpaceCoords(playState, data)
    if (length(data$x) == 0) return(FALSE)
    if (length(data$y) == 0) return(FALSE)
    coords <- foo$coords
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
        ## all data points within 11 points
        which <- which(pdists < 11)
        if (length(which) == 0) return()
        ## order by distance from click
        which <- which[order(pdists[which])]
        ## account for multiple points (matrix values of data$x, data$y)
        n <- NROW(data$x)
        which <- unique(which %% n)
        which[which == 0] <- n

        idMenu <- gtkMenu()
        idMenu$popup(button=0, activate.time=gtkGetCurrentEventTime())
        item <- gtkMenuItem("Add label to plot:")
        item["sensitive"] <- FALSE
        idMenu$append(item)
        for (w in which) {
            datx <- data$x[[w]]
            daty <- data$y[[w]]
            pos <- with(ppxy, lattice:::getTextPosition(x = lx - px[w],
                                                        y = ly - py[w]))
            ss <- data$subscripts[[w]]
            if (is.null(ss)) ss <- w
            label <- toString(playState$labels[[ss]])
            item <- gtkMenuItem(label)
            idMenu$append(item)
            gSignalConnect(item, "activate",
                           function(widget, user.data) {
                               ss <- user.data$ss
                               pos <- user.data$pos
                               ## store newly identified points in playState
                               ids.new <- data.frame(subscripts = ss, pos = pos)
                               i <- length(playState$ids) + 1
                               playState$ids[[i]] <- ids.new
                               names(playState$ids)[i] <- space
                               playState$undoStack <- c(playState$undoStack, "ids")
                               ## draw them
                               drawLabelsInSpace(playState, subscripts = ss,
                                                 space = space, pos = pos)
                               ## update other tool states
                               updateAnnotationActionStates(playState)
                           }, data = list(ss = ss, pos = pos))
        }
        idMenu$append(gtkSeparatorMenuItem())
        item <- gtkMenuItem("Right-click on point for detail")
        item["sensitive"] <- FALSE
        idMenu$append(item)
        ## show the menu
        while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
    } else {
        ## drag
        foo <- playSelectData(playState, foo = foo)
        if (is.null(foo)) return()
        if (length(foo$which) == 0) return()
        with(foo, {
            if (!is.click) pos <- 1
            ## store newly identified points in playState
            ids.new <- data.frame(subscripts = subscripts, pos = pos)
            i <- length(playState$ids) + 1
            playState$ids[[i]] <- ids.new
            names(playState$ids)[i] <- space
            playState$undoStack <- c(playState$undoStack, "ids")
            ## draw them
            drawLabelsInSpace(playState, subscripts = subscripts,
                              space = space, pos = pos)
        })
        ## update other tool states
        updateAnnotationActionStates(playState)
    }
}

id.table_handler <- function(widget, playState) {
    ## TODO
    gmessage.error("not yet implemented")
}

id.find_handler <- function(widget, playState) {
    ## TODO
    gmessage.error("not yet implemented")
}

set.labels_handler <- function(widget, playState)
{
    playFreezeGUI(playState)
    on.exit(playThawGUI(playState))
    ## widgets
    box <- ggroup(horizontal = FALSE)
    datArg <- getDataArg(playState, eval = FALSE)
    dat <- try(eval(datArg, playState$env))
    labcode <- NULL
    labdesc <- NULL
    if (!is.null(dat)) {
        labcode <- colnames(dat)
        labdesc <- colnames(dat)
    }
    rnCode <- deparseOneLine(call("rownames", datArg))
    labcode <- c(labcode,
                 "xyData()$x",
                 "xyData()$y",
                 'with(xyData(), paste(y, x, sep="@"))',
                 "NULL",
                 rnCode)
    labdesc <- c(labdesc,
                 "data x values",
                 "data y values",
                 "data y@x values",
                 "data subscripts",
                 rnCode)
    labradio <- gradio(labdesc, selected = length(labcode), container = box,
                       handler = function(h, ...) {
                           idx <- max(1, svalue(labradio, index=TRUE))
                           svalue(labedit) <- labcode[idx]
                       })
    labedit <- gedit(rnCode, container = box)
    ## show dialog
    gbasicdialog("Set labels to...", widget = box,
                 handler = function(h, ...) {
                     playDevSet(playState)
                     expr <- parse(text=svalue(labedit))
                     tmp <- tryCatch(
                             eval(expr, dat, playState$env),
                                     error=function(e)e)
                     ## check whether there was an error
                     if (inherits(tmp, "error")) {
                         gmessage.error(conditionMessage(tmp))
                     } else {
                         ## set labels
                         playState$labels <- tmp
                         ## and set default for playNewPlot
                         playState$.args$labels <- tmp
                         ## redraw
                         if (length(playState$ids))
                             playReplot(playState)
                     }
                 })
}

save.ids_handler <- function(widget, playState) {
    name <- ginput("Save subscripts of labelled / brushed points to variable:",
                   title = "Save IDs", text = "myIds")
    if ((length(name) == 0) || (nchar(name) == 0))
        return()
    playDevSet(playState)
    assign(name, playGetIDs(playState), globalenv())
}

brushCore <- function(playState, foo, remove = FALSE)
{
    foo <- playSelectData(playState, foo = foo)
    if (is.null(foo)) return()
    if (length(foo$which) == 0) return()
    ids <- foo$subscripts
    ## TODO: if (remove)
    i <- length(playState$linked$ids) + 1
    playState$linked$ids[[i]] <- ids
    playState$undoStack <- c(playState$undoStack, "linked")
    updateAnnotationActionStates(playState)
    drawLinkedLocal(playState)
    updateLinkedSubscribers(playState)
}

