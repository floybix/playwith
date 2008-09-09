## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer


initIdentifyActions <- function(playState)
{
    playState$tmp$identify.ok <- FALSE
    hasArgs <- playState$accepts.arguments
    isLatt <- playState$is.lattice
    isLatt3D <- isLatt && !is.null(playState$trellis$panel.args.common$scales.3d)
    isBase <- !isLatt && is.null(playState$viewport)
    isBaseMulti <- isBase && any(par("mfrow") > 1)
    ## detect known plots that this will not work with
    ## TODO: check that xyData() has x and y coords
    if (is.null(playState$data.points)) {
        if (!hasArgs) return()
        if (isBaseMulti) return()
        ## lattice package:
        if (isLatt3D) return()
        if (playState$callName %in%
            c("splom", "contourplot",
              "histogram", "densityplot", "barchart")) return()
        if (playState$callName %in% "marginal.plot") return()
        ## what about parallel?
        ## from graphics package:
        if (playState$callName %in%
            c("hist", "barplot", "spineplot", "mosaic",
              "assocplot", "fourfoldplot",
              "coplot", "image", "contour", "persp",
              "pie", "pairs")) return()
    }
    mainCall <- mainCall(playState)
    labels <- playState$.args$labels
    ## try to guess labels if they were not given
    if (is.null(labels)) {
        if (is.null(playState$data.points)) {
            ## try to construct labels from the plot call

            tmp.data <- getDataArg(playState)

            if (length(mainCall > 1)) {
                ## check for named "data" argument
                tmp.data <- callArg(playState, "data")
                if (!is.null(tmp.data))
                    labels <- makeLabels(tmp.data)
                ## hard-coded exceptions...
                if (playState$callName == "qqplot") {
                    tmp.x <- callArg(playState, 1)
                    tmp.y <- callArg(playState, 2)
                    x.lab <- makeLabels(tmp.x, orSeq=T)
                    y.lab <- makeLabels(tmp.y, orSeq=T)
                    labels <- paste(sep="",
                                    x.lab[order(tmp.x)], ",",
                                    y.lab[order(tmp.y)])
                }
                ## otherwise: default handler...
                if (is.null(labels)) {
                    ## look at first argument (tmp.data may be NULL)
                    tmp.x <- callArg(playState, 1, data=tmp.data)
                    if (inherits(tmp.x, "formula")) {
                        ## if 1st arg is formula, 2nd is `data`
                        if (is.null(tmp.data) &&
                            (length(mainCall) >= 3) &&
                            (is.null(names(mainCall)) ||
                             identical(names(mainCall)[[3]], ""))
                            )
                            tmp.data <- callArg(playState, 2)
                        xObj <- if (length(tmp.x) == 2)
                            tmp.x[[2]] else tmp.x[[3]]
                        ## get left-most term in formula
                        while (is.call(xObj) && toString(xObj[[1]]) %in%
                               c("|", "*", "+"))
                            xObj <- xObj[[2]]
                        xObj <- if (!is.null(tmp.data))
                            eval(xObj, tmp.data, playState$env)
                        else eval(xObj, environment(tmp.x), playState$env)
                        labels <- makeLabels(xObj, orSeq=T)
                    } else {
                        if (is.null(row.names(tmp.x)) &&
                            is.list(tmp.x) &&
                            all(c("x","y") %in% names(tmp.x)))
                            tmp.x <- tmp.x$x
                        labels <- makeLabels(tmp.x, orSeq=T)
                    }
                }
            }
        } else {
            ## data.points were supplied
            tmp.x <- playState$data.points
            if (is.null(row.names(tmp.x)) &&
                is.list(tmp.x) &&
                all(c("x","y") %in% names(tmp.x)))
                tmp.x <- tmp.x$x
            labels <- makeLabels(tmp.x, orSeq=T)
        }
    }
    playState$labels <- labels
    playState$tmp$identify.ok <- TRUE
}

makeLabels <- function(x, orSeq=FALSE)
{
    labels <- row.names(x)
    if (is.factor(x) || is.character(x))
        labels <- as.character(x)
    if (inherits(x, "POSIXt") ||
        inherits(x, "Date") ||
        inherits(x, "yearmon") ||
        inherits(x, "yearqtr"))
        labels <- format(x)
    if (inherits(x, "ts") || inherits(x, "zoo"))
        labels <- rep(format(stats::time(x)), NCOL(x))
    if (is.null(labels) && is.numeric(x))
        labels <- names(x)
    if (is.null(labels) && orSeq) labels <- seq_along(x)
    labels
}

updateIdentifyActions <- function(playState)
{
    aGroup <- playState$actionGroups[["PlotActions"]]
    ## Identify etc
    canIdent <- playState$tmp$identify.ok
    aGroup$getAction("Identify")$setSensitive(canIdent)
    aGroup$getAction("IdTable")$setSensitive(canIdent)
    aGroup$getAction("SaveIDs")$setSensitive(canIdent)
    ## draw persistent labels
    if (canIdent)
        drawLabels(playState)
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
        x <- data$x[which]
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

set.labels_handler <- function(widget, playState)
{
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
                 "xyData(playDevCur())$x",
                 "xyData(playDevCur())$y",
                 'with(xyData(playDevCur()), paste(y, x, sep="@"))',
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
                     expr <- parse(text=svalue(labedit))
                     tmp <- tryCatch(
                             eval(expr, dat, playState$env),
                                     error=function(e)e)
                     ## check whether there was an error
                     if (inherits(tmp, "error")) {
                         gmessage.error(conditionMessage(tmp))
                     } else {
                         playState$labels <- tmp
                     }
                 })
}

set.label.offset_handler <- function(widget, playState)
{
    ## TODO
    gmessage.error("not yet implemented")
}

## TODO: get rid of this -- set label style now means changing lattice settings
set.label.style_handler <- function(widget, playState)
{
    style <- playState$label.style
    if (is.null(playState$label.style)) {
        style <- do.call(gpar, trellis.par.get("add.text"))
    }
    if (inherits(style, "gpar"))
        style <- as.call(c(quote(gpar), style))

    callTxt <- deparseOneLine(style)

    repeat {
        newTxt <- NA
        editbox <- gedit(callTxt, width=120)
        gbasicdialog("Edit label style", widget=editbox, action=environment(),
                     handler=function(h, ...) {
                         h$action$newTxt <- svalue(editbox)
                     })
        if (is.na(newTxt)) break
        if (newTxt == "") break
        if (identical(newTxt, callTxt)) break
        callTxt <- newTxt
        tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
        ## check whether there was a syntax error
        if (inherits(tmp, "error")) {
            gmessage.error(conditionMessage(tmp))
        } else {
            playState$label.style <- eval(tmp)
            break
        }
    }
    playState$win$present()
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
        idMenu <- gtkMenu()
        idMenu$popup(button=0, activate.time=gtkGetCurrentEventTime())
        ## order by distance from click
        which <- which[order(pdists[which])]
        for (w in which) {
            datx <- data$x[[w]]
            daty <- data$y[[w]]
            pos <- with(ppxy, lattice:::getTextPosition(x = lx - px[w],
                                                        y = ly - py[w]))
            ss <- data$subscripts[[w]]
            if (is.null(ss)) ss <- w
            label <- toString(playState$labels[[ss]])
            label <- paste(label, " (x: ", format(signif(datx, 4)),
                           ", y: ", format(signif(daty, 4)), ")", sep="")
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
                                        #ids.old <- playState$ids[[space]] ## may be NULL
                                        #if (is.null(ids.old)) ids.old <- ids.new
                                        #else ids.new <- rbind(ids.old, ids.new)
                                        #playState$ids[[space]] <- ids.new
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
    gmessage.error("not yet implemented")
}

save.ids_handler <- function(widget, playState) {
    gmessage.error("not yet implemented")
}
