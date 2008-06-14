## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### IDENTIFY

toolConstructors$identify <- function(playState)
{
    if (is.null(playState$data.points)) {
        if (playState$accepts.arguments == FALSE) return(NA)
        ## does not currently work with "splom" or 3D plots (TODO)
        callName <- deparseOneLine(playState$call[[1]])
        if (callName %in% c("splom", "cloud", "wireframe"))
            return(NA)
    }
    labels <- playState$.args$labels
    if (is.null(labels)) {
        if (is.null(playState$data.points)) {
            ## try to construct labels from the plot call
            if (playState$is.lattice) {
                tmp.data <- NULL
                if ("data" %in% names(playState$call)) {
                    tmp.data <- callArg(playState, data)
                    labels <- makeLabels(tmp.data)
                }
                if (is.null(labels)) {
                    ## try to make labels from first argument
                    tmp.x <- callArg(playState, 1)
                    if (inherits(tmp.x, "formula")) {
                        xObj <- if (length(tmp.x) == 2)
                            tmp.x[[2]] else tmp.x[[3]]
                        ## get left-most term in formula
                        while (is.call(xObj) && toString(xObj[[1]]) %in%
                               c("|", "*", "+"))
                            xObj <- xObj[[2]]
                        xObj <- eval(xObj, tmp.data,
                                     environment(tmp.x))
                        labels <- makeLabels(xObj, orSeq=T)
                    } else {
                        labels <- makeLabels(tmp.x, orSeq=T)
                    }
                }
            } else {
                ## base graphics
                if (length(playState$call) >= 2) {
                    tmp.x <- callArg(playState, 1)
                    if (inherits(tmp.x, "formula")) {
                        xObj <- if (length(tmp.x) == 2)
                            tmp.x[[2]] else tmp.x[[3]]
                        xObj <- eval(xObj, environment(tmp.x),
                                     playState$env)
                        labels <- makeLabels(xObj, orSeq=T)
                    } else {
                        if (is.null(row.names(tmp.x)) &&
                            is.list(tmp.x) &&
                            all(c("x","y") %in% names(tmp.x)))
                            tmp.x <- tmp.x$x
                        labels <- makeLabels(tmp.x, orSeq=T)
                    }
                    ## exceptions...
                    callName <- deparseOneLine(playState$call[[1]])
                    if (callName %in% c("qqplot")) {
                        tmp.y <- callArg(playState, 2)
                        x.lab <- makeLabels(tmp.x, orSeq=T)
                        y.lab <- makeLabels(tmp.y, orSeq=T)
                        labels <- paste(sep="",
                                        x.lab[order(tmp.x)], ",",
                                        y.lab[order(tmp.y)])
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
    xy <- xyCoords(playState, space=space)
    xy <- dataCoordsToSpaceCoords(playState, xy)
    x <- xy$x[which]
    y <- xy$y[which]
    labels <- playState$labels
    if (playState$is.lattice && !is.null(xy$subscripts) &&
        (length(labels) > length(xy$subscripts)))
        labels <- labels[ xy$subscripts ]
    labels <- labels[which]
    style <- eval(playState$label.style)
    if (is.null(playState$label.style)) {
        ## default style is taken (at plot time) from lattice settings
        style <- do.call(gpar, trellis.par.get("add.text"))
    }
    annots <- expression()
    pos <- rep(pos, length=length(labels))
    offset <- unit(0.5, "char")
    if (!is.null(playState$label.offset)) {
        offset <- playState$label.offset
        if (!inherits(offset, "unit"))
            offset <- unit(offset, "char")
    }
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
