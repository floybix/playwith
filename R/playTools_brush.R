## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### BRUSH

toolConstructors$brush <- function(playState)
{
    if (playState$accepts.arguments == FALSE) return(NA)
    ## this tool only works with "splom" lattice plots
    callName <- deparseOneLine(mainCall(playState)[[1]])
    if ((callName %in% "splom") == FALSE)
        return(NA)

    quickTool(playState,
              label = "Brush",
              icon = "gtk-media-record",
              tooltip = "Brush (highlight) data points",
              f = brush_handler,
              post.plot.action = brush_postplot_action)
}

brush_handler <- function(widget, playState)
{
    ## do brushing
    repeat {
        foo <- playRectInput(playState, prompt=paste("Brushing data points...",
                                        "Click the right mouse button to finish."))
        if (is.null(foo)) return()
        if (is.null(foo$coords)) return()
        if (foo$is.click) {
            ## make it a 15x15 pixel box
            foo$dc$x <- foo$dc$x[1] + c(-7, 7)
            foo$dc$y <- foo$dc$y[1] + c(-7, 7)
            foo$coords <- with(foo, deviceCoordsToSpace(playState,
                                                        dc$x, dc$y, space=space))
        }
        space <- foo$space
        x.npc <- playDo(playState,
			convertX(unit(foo$coords$x, "native"), "npc", valueOnly=TRUE),
			space=space)
        y.npc <- playDo(playState,
			convertY(unit(foo$coords$y, "native"), "npc", valueOnly=TRUE),
			space=space)
        pdata <- xyCoords(playState, space=space)
        nvars <- length(pdata$z)
        ## which subpanel
        colpos <- ceiling(x.npc[1] * nvars)
        rowpos <- ceiling(y.npc[1] * nvars)
        if ((rowpos == colpos) ||
            (colpos < 1) || (colpos > nvars) ||
            (rowpos < 1) || (rowpos > nvars)) {
            ## try next point
            colpos <- ceiling(x.npc[2] * nvars)
            rowpos <- ceiling(y.npc[2] * nvars)
        }
        if ((rowpos == colpos) ||
            (colpos < 1) || (colpos > nvars) ||
            (rowpos < 1) || (rowpos > nvars)) {
            ## give up
            return()
        }
        subpanel.name <- paste("subpanel", colpos, rowpos, sep = ".")
        ## coordinates of rect in subpanel
        x.subp <- unit(nvars * (x.npc - (colpos-1) / nvars), "npc")
        y.subp <- unit(nvars * (y.npc - (rowpos-1) / nvars), "npc")
        ## get to that viewport, so we can convert units
        depth <- downViewport(subpanel.name)
        x.subp <- convertX(x.subp, "native", TRUE)
        y.subp <- convertY(y.subp, "native", TRUE)
        upViewport(depth)
        datax <- pdata$z[, colpos]
        datay <- pdata$z[, rowpos]
        brushed.new <- which(
                             (min(x.subp) < datax) & (datax < max(x.subp)) &
                             (min(y.subp) < datay) & (datay < max(y.subp))
                             )
        playDo(playState,
               splom.drawBrushed(brushed.new, pargs=pdata),
               space=space)
        brushed.old <- playState$brushed[[space]]
        if (!is.null(brushed.old)) brushed.new <- union(brushed.new, brushed.old)
        playState$brushed[[space]] <- brushed.new
        with(playState$tools, {
            if (!is.null(clear)) clear["visible"] <- TRUE
        })
    }
}

brush_postplot_action <- function(widget, playState)
{
    if (playState$is.lattice) {
        ## draw persistent brushing
        for (space in names(playState$brushed)) {
            pdata <- xyCoords(playState, space=space)
            ids.sub <- playState$brushed[[space]]
            playDo(playState,
                   splom.drawBrushed(ids.sub, pargs=pdata),
                   space=space)
        }
    }
}

splom.drawBrushed <- function(ids, pargs=trellis.panelArgs(), threshold=18, col='black', pch=16, cex=1, ...)
{
    nvars <- length(pargs$z)
    for (row in 1:nvars)
        for (column in 1:nvars)
            if (row != column)
            {
                subpanel.name <-
                    paste("subpanel",
                          column, row, sep = ".")
                depth <- downViewport(subpanel.name)
                panel.points(x = pargs$z[ids, column],
                             y = pargs$z[ids, row],
                             pch = pch, col = col, cex = cex,
                             ...)
                upViewport(depth)
            }
}
