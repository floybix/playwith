## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

identifyActionGroup <- function(playState)
{
    entries <-
        list( ## : name, stock icon, label, accelerator, tooltip, callback
             list("Identify", "gtk-info", "_Identify...", NULL, "Identify all points in a selected region", identify_handler),
             list("IdTable", "gtk-info", "Select from _table...", NULL, "Select points from a table", id.table_handler),
             list("FindLabels", "gtk-find", "_Find...", "<Ctrl>F", "Find points with labels matching...", id.find_handler),
             list("SaveIDs", NULL, "_Save IDs...", NULL, "_Save current IDs to an object", save.ids_handler)
             )

    ## construct action group with playState passed to callbacks
    aGroup <- gtkActionGroupNew("IdentifyActions")
    aGroup$addActions(entries, playState)
    aGroup
}

initIdentifyActions <- function(playState)
{
    playState$tmp$identify.ok <- FALSE
    hasArgs <- playState$accepts.arguments
    isLatt <- playState$is.lattice
    isBase <- !isLatt && is.null(playState$viewport)
    isBaseMulti <- isBase && any(par("mfrow") > 1)
    ## detect known plots that this will not work with
    ## TODO: check that xyData() has x and y coords
    if (is.null(playState$data.points)) {
        if (!hasArgs) return()
        if (isBaseMulti) return()
        ## lattice package:
        if (playState$callName %in%
            c("splom", "cloud", "wireframe",
              "contourplot", "levelplot",
              "histogram", "densityplot", "barchart")) return()
        ## what about parellel?
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

updateIdentifyActions <- function(playState)
{
    aGroup <- playState$actionGroups[["IdentifyActions"]]
    ## Identify etc
    canIdent <- playState$tmp$identify.ok
    aGroup$getAction("Identify")$setVisible(canIdent)
    aGroup$getAction("IdTable")$setVisible(canIdent)
    aGroup$getAction("SaveIDs")$setVisible(canIdent)
    ## draw persistent labels
    if (!canIdent) return()
    for (space in names(playState$ids)) {
        idInfo <- playState$ids[[space]]
        drawLabels(playState, which=idInfo$which, space=space,
                   pos=idInfo$pos)
    }
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

drawLabels <- function(playState, which, space="plot", pos=1)
{
    playDevSet(playState)
    data <- xyCoords(playState, space=space)
    if (length(data$x) == 0) return(FALSE)
    if (length(data$y) == 0) return(FALSE)
    ## convert to log scale if necessary
    data <- dataCoordsToSpaceCoords(playState, data)
    x <- data$x[which]
    y <- data$y[which]
    if (playState$is.lattice && !is.null(data$subscripts)) {
        subwhich <- findInterval(which, data$subscripts)
        x <- data$x[subwhich]
        y <- data$y[subwhich]
    }
    labels <- playState$labels[which]
    style <- eval(playState$label.style)
    if (is.null(playState$label.style)) {
        ## default style is taken (at plot time) from lattice settings
        style <- do.call(gpar, trellis.par.get("add.text"))
    }
    annots <- expression()
    pos <- rep(pos, length=length(labels))
    offset <- playState$label.offset
    if (!inherits(offset, "unit"))
        offset <- unit(offset, "char")
    ## TODO: do this without a loop
    for (i in seq_along(labels)) {
        ux <- unit(x[i], "native")
        uy <- unit(y[i], "native")
        if (pos[i] == 1) {
            uy <- uy - offset
            adj <- c(0.5, 1)
        }
        else if (pos[i] == 2) {
            ux <- ux - offset
            adj <- c(1, 0.5)
        }
        else if (pos[i] == 3) {
            uy <- uy + offset
            adj <- c(0.5, 0)
        }
        else if (pos[i] == 4) {
            ux <- ux + offset
            adj <- c(0, 0.5)
        }
        annots[[i]] <- call("grid.text", labels[i], x=ux, y=uy,
                            just=adj, gp=style)
    }
    playDo(playState, eval(annots), space=space,
           clip.off=identical(playState$clip.annotations, FALSE))
}

identify_handler <- function(widget, playState)
{
    foo <- playSelectData(playState,
                          "Click or drag to identify points. Right-click to cancel.")
    if (is.null(foo)) return()
    if (length(foo$which) == 0) return()
    if (!is.null(foo$subscripts))
        foo$which <- foo$subscripts
    with(foo, {
        if (!is.click) pos <- 1
        ## store newly identified points in playState
        ids.new <- data.frame(which=which, pos=pos)
        ids.old <- playState$ids[[space]] ## may be NULL
        if (is.null(ids.old)) ids.old <- ids.new
        else ids.new <- rbind(ids.old, ids.new)
        playState$ids[[space]] <- ids.new
        ## draw them
        drawLabels(playState, which=which, space=space, pos=pos)
    })
    ## update other tool states
    updateAnnotationActionStates(playState)
}

identifyCore <- function(playState, foo)
{
    if (!isTRUE(playState$tmp$identify.ok)) return()
    if (is.null(playState$labels)) return()
    space <- foo$space
    data <- xyCoords(playState, space=foo$space)
    ## convert to log scale if necessary
    data <- dataCoordsToSpaceCoords(playState, data)
    if (length(data$x) == 0) return(FALSE)
    if (length(data$y) == 0) return(FALSE)
    coords <- foo$coords

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
    idMenu <- gtkMenu()
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
        idInfo <- list(which=ss, pos=pos)
        gSignalConnect(item, "activate",
                       function(widget, user.data) {
                           which <- user.data$which
                           pos <- user.data$pos
                           ## store newly identified points in playState
                           ids.new <- data.frame(which=which, pos=pos)
                           ids.old <- playState$ids[[space]] ## may be NULL
                           if (is.null(ids.old)) ids.old <- ids.new
                           else ids.new <- rbind(ids.old, ids.new)
                           playState$ids[[space]] <- ids.new
                           ## draw them
                           drawLabels(playState, which=which, space=space, pos=pos)
                           ## update other tool states
                           updateAnnotationActionStates(playState)
                       }, data=idInfo)
    }
    ## show the menu
    idMenu$popup(button=0, activate.time=gtkGetCurrentEventTime())
    while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
}


id.table_handler <- function(widget, playState) {
}

id.find_handler <- function(widget, playState) {
}

save.ids_handler <- function(widget, playState) {

}
