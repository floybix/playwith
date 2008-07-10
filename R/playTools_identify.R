## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### IDENTIFY

toolConstructors$identify <- function(playState)
{
    ## this tool does not work with multiple base graphics plots
    if (!playState$is.lattice && is.null(playState$viewport)) {
        if (any(par("mfrow") > 1)) return(NA)
    }
    if (is.null(playState$data.points)) {
        if (playState$accepts.arguments == FALSE) return(NA)
        callName <- deparseOneLine(mainCall(playState)[[1]])
        ## detect plots that this will not work with
        ## TODO: check that xyData() has x and y coords!
        ## lattice package:
        if (callName %in%
            c("splom", "cloud", "wireframe",
              "contourplot", "levelplot",
              "histogram", "densityplot", "barchart")) return(NA)
        ## what about parellel?
        ## from graphics package:
        if (callName %in%
            c("hist", "barplot", "spineplot", "mosaic",
              "assocplot", "fourfoldplot",
              "coplot", "image", "contour", "persp",
              "pie", "pairs")) return(NA)

    }
    mainCall <- mainCall(playState)
    labels <- playState$.args$labels
    if (is.null(labels)) {
        if (is.null(playState$data.points)) {
            ## try to construct labels from the plot call










            ## try to construct labels from the plot call
            if (length(mainCall > 1)) {
                ## check for named "data" argument
                tmp.data <- callArg(playState, "data")
                if (!is.null(tmp.data))
                    labels <- makeLabels(tmp.data)
                ## hard-coded exceptions...
                callName <- deparseOneLine(mainCall(playState)[[1]])
                if (callName %in% c("qqplot")) {
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
                              (is.null(names(mainCall)) ||
                               identical(names(mainCall)[[3]], "")))
                            tmp.data <- callArg(playState, 2)
                        xObj <- if (length(tmp.x) == 2)
                            tmp.x[[2]] else tmp.x[[3]]
                        ## get left-most term in formula
                        while (is.call(xObj) && toString(xObj[[1]]) %in%
                               c("|", "*", "+"))
                            xObj <- xObj[[2]]
                        xObj <- if (!is.null(tmp.data))
                            eval(xObj, tmp.data, playState$env) #environment(tmp.x))
                        else eval(xObj, environment(tmp.x), playState$env)
                        ## TODO: make playState$env inherit from .GlobalEnv?
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
    ## add click event handler to plot -- always active
    if (is.null(playState$widgets$plotIDEventSig)) {
        playState$widgets$plotIDEventSig <-
            gSignalConnect(playState$widgets$drawingArea,
                           "button-press-event", id_click_handler, data=playState)
    }
    if (is.null(playState$widgets$plotUnIDEventSig)) {
        playState$widgets$plotUnIDEventSig <-
            gSignalConnect(playState$widgets$drawingArea,
                           "button-release-event", id_unclick_handler, data=playState)
    }
    ## make the widget
    quickTool(playState,
              label = "Identify",
              icon = "gtk-info",
              tooltip = "Identify data points by clicking on them",
              f = identify_handler,
              post.plot.action = identify_postplot_action)
}

makeLabels <- function(x, orSeq=FALSE)
{
    labels <- row.names(x)
    if (inherits(x, "POSIXt"))
        labels <- format(x)
    if (inherits(x, "Date"))
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
    data <- dataCoordsToSpaceCoords(playState, data)
    x <- data$x[which]
    y <- data$y[which]
    if (playState$is.lattice && !is.null(data$subscripts)) {
        subwhich <- findInterval(which, data$subscripts)
        x <- data$x[subwhich]
        y <- data$y[subwhich]
    }
    labels <- playState$labels
    #if (playState$is.lattice && !is.null(data$subscripts) &&
    #    (length(labels) > length(data$subscripts)))
    #    labels <- labels[ data$subscripts ]
    labels <- labels[which]
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
    repeat {
        foo <- playSelectData(playState,
                              "Click or drag to identify points. Right-click to end.")
        if (is.null(foo)) break
        if (length(foo$which) == 0) next
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
    }
    ## update other tool states
    with(playState$tools, {
        if (exists("clear", inherits=F))
            clear["visible"] <- TRUE
    })
}

identify_postplot_action <- function(widget, playState)
{
    ## draw persistent labels
    for (space in names(playState$ids)) {
        idInfo <- playState$ids[[space]]
        drawLabels(playState, which=idInfo$which, space=space,
                   pos=idInfo$pos)
    }
}

id_click_handler <- function(widget, event, playState)
{
    if (!isTRUE(playState$plot.ready)) return(FALSE)
    ## coords handler already does this:
    #if (playState$.need.reconfig) generateSpaces(playState)
    da <- playState$widgets$drawingArea
    result <- try(da["tooltip-text"] <- NULL, silent=TRUE)
    if (inherits(result, "try-error")) return(FALSE)
    x <- event$x
    y <- event$y
    space <- whichSpace(playState, x, y)
    if (space == "page") return(FALSE)
    data <- xyCoords(playState, space=space)
    if (length(data$x) == 0) return(FALSE)
    if (length(data$y) == 0) return(FALSE)
    click.xy <- deviceCoordsToSpace(playState, x, y, space=space)
    ## convert from log scale if necessary
    click.xy <- spaceCoordsToDataCoords(playState, click.xy)
    x <- click.xy$x
    y <- click.xy$y
    ppxy <- playDo(playState,
                   list(lx=convertX(unit(x, "native"), "points", TRUE),
                        ly=convertY(unit(y, "native"), "points", TRUE),
                        px=convertX(unit(data$x, "native"), "points", TRUE),
                        py=convertY(unit(data$y, "native"), "points", TRUE)),
                   space=space)
    pdists <- with(ppxy, sqrt((px - lx)^2 + (py - ly)^2))
    if (min(pdists, na.rm = TRUE) > 30) ## needs to be quite tolerant -- bugs lurking?
        return(FALSE)
    which <- which.min(pdists)
    labels <- playState$labels
    if (playState$is.lattice && !is.null(data$subscripts) &&
        (length(labels) > length(data$subscripts)))
        labels <- labels[ data$subscripts ]
    lab <- labels[which]
    da["tooltip-text"] <- lab
    ## try to force update
    da$window$processUpdates(FALSE)
    da$window$invalidateRect(invalidate.children=FALSE)
    while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
    return(FALSE)
}

id_unclick_handler <- function(widget, event, playState)
{
    if (!isTRUE(playState$plot.ready)) return(FALSE)
    da <- playState$widgets$drawingArea
    result <- try(da["tooltip-text"] <- NULL, silent=TRUE)
    ## try to force update
    gdkWindowProcessAllUpdates()
    while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
    return(FALSE)
}
