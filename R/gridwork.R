## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer


DPI <- function()
    round(mean(dev.size("px") / dev.size("in")))

oldDPI <- function()
{
    ## use cached value if available
    if (!is.null(.PlaywithEnv$.DPI))
        return(.PlaywithEnv$.DPI)
    ## current viewport, restore when finished
    vp <- current.vpPath()
    if (length(vp) > 0) {
        upViewport(0)
        on.exit(downViewport(vp))
    }
    dpi <- convertWidth(unit(1, "inches"),
                        "native", valueOnly=TRUE)
    .PlaywithEnv$.DPI <- dpi
    dpi
}

convertToDevicePixels <-
    function(x, y, dpi=DPI())
#             viewport, unitTo = "native", valueOnly = TRUE)
{
#    vp <- current.vpPath()
    ## restore current viewport when finished
#    on.exit(if (length(vp) > 0) downViewport(vp))
    ## go to viewport
#    upViewport(0)
#    if (!is.null(viewport))
#        downViewport(viewport)
    ## x and y can be unit objects or numeric
    if (!is.unit(x)) x <- unit(x, "native")
    if (!is.unit(y)) y <- unit(y, "native")
    xy <- cbind(convertX(x, "inches", valueOnly=TRUE),
                convertY(y, "inches", valueOnly=TRUE),
                1)
    location <- xy %*% current.transform() ## inches
    locx <- round(dpi * location[,1] * location[,3])
    locy <- round(dpi * location[,2] * location[,3])
    ## convert y coordinate to have origin at top-left
    locy <- dev.size("px")[2] - locy
    list(x = locx, y = locy)
    ## go to root viewport
    ## TODO: get DPI to avoid going to root viewport
#    upViewport(0)
#    list(x = convertX(locationx, unitTo, valueOnly=valueOnly),
#         y = convertY(locationy, unitTo, valueOnly=valueOnly))
}

convertFromDevicePixels <-
    function(x.px, y.px, dpi=DPI(),
             unitTo = "native", valueOnly = FALSE)
{
#    vp <- current.vpPath()
    ## restore current viewport when finished
#    on.exit({
#        upViewport(0)
#        if (length(vp) > 0) downViewport(vp)
#    })
    ## go to root viewport
    ## TODO: get DPI to avoid going to root viewport
#    upViewport(0)
    ## convert pixels to inches
    x.in <- dpi * x.px
    y.in <- dpi * y.px
#    x.in <- convertX(unit(x.px, "native"), "inches", valueOnly=TRUE)
#    y.in <- convertY(unit(y.px, "native"), "inches", valueOnly=TRUE)
    ## TODO: support vector arguments
    xy <- cbind(x.in, y.in, 1)
    ## go to viewport
#    if (!is.null(viewport))
#        downViewport(viewport)
#    inv.transform <- solve(current.transform())
    location <- xy %*% solve(current.transform()) ## inches
    locx <- unit(location[,1] * location[,3], "inches")
    locy <- unit(location[,2] * location[,3], "inches")
    list(x = convertX(locx, unitTo, valueOnly=valueOnly),
         y = convertY(locy, unitTo, valueOnly=valueOnly))
}

inViewport <- function(x.px, y.px, viewport, dpi=DPI())
{
    ## current viewport, restore when finished
    vp <- current.vpPath()
    on.exit({
        upViewport(0)
        if (length(vp) > 0) downViewport(vp)
    })
    upViewport(0)
    if (!is.null(viewport))
        downViewport(viewport)
    ## calculate bounding box
    xy <- convertToDevicePixels(x = unit(0:1, "npc"),
                                y = unit(0:1, "npc"),
                                dpi = dpi)
    ## viewport might be rotated, so use range of x and y
    x <- xy$x
    y <- xy$y
    ## test for point inside bounding box
    ((min(x) <= x.px) && (x.px <= max(x)) &&
     (min(y) <= y.px) && (y.px <= max(y)))
}

grobBBDevicePixels <- function(grob, viewport, dpi=DPI())
{
    ## current viewport, restore when finished
    vp <- current.vpPath()
    on.exit({
        upViewport(0)
        if (length(vp) > 0) downViewport(vp)
    })
    upViewport(0)
    if (!is.null(viewport))
        downViewport(viewport)
    ## calculate bounding box
    if (inherits(grob, "points") ||
        inherits(grob, "lines"))
    {
        ## what about segments, circle etc?
        ## assuming units are simple, i.e. all same unit
        #xx <- unit(range(unclass(grob$x)),
        #           attr(grob$x, "unit")[1])
        #yy <- unit(range(unclass(grob$y)),
        #           attr(grob$y, "unit")[1])
        #xy0 <- convertToDevice(x=xx[1], y=yy[1],
        #                       viewport=viewport)
        #xy1 <- convertToDevice(x=xx[2], y=yy[2],
        #                       viewport=viewport)
        xy <- convertToDevicePixels(x = grob$x,
                                    y = grob$y,
                                    dpi = dpi)
    } else {
        gx <- unit.c(grobX(grob, "west"),
                     grobX(grob, "east"))
        gy <- unit.c(grobY(grob, "south"),
                     grobY(grob, "north"))
        xy <- convertToDevicePixels(x = gx,
                                    y = gy,
                                    dpi = dpi)
    }
    xy$x <- range(xy$x)
    xy$y <- range(xy$y)
    xy
}

showGrobsBB <-
    function(draw = TRUE,
             gp.box = gpar(fill="yellow",
                           lwd=2, alpha=0.05),
             gp.text = gpar(cex=0.75, alpha=0.5))
{
    ## current viewport, restore when finished
    vp <- current.vpPath()
    on.exit({
        upViewport(0)
        if (length(vp) > 0) downViewport(vp)
    })
    ## get a list of all grobs in the scene
    upViewport(0)
    objs <- as.data.frame(unclass(grid.ls(viewports=TRUE,
                                          print=FALSE)),
                          stringsAsFactors=FALSE)
    objs <- objs[objs$type == "grobListing",]
    if (nrow(objs) == 0) return()
    objs$vpPath <- sub("^ROOT::", "", objs$vpPath)
    objs$vpPath[objs$vpPath == "ROOT"] <- ""
    ## draw boxes around grobs
    bblist <- list()
    length(bblist) <- nrow(objs)
    dpi <- DPI()
    for (i in seq_len(nrow(objs))) {
        vpPath <- objs$vpPath[i]
        if (vpPath == "") vpPath <- NULL
        gName <- objs$name[i]
        ## TODO: objs$gPath[i] // strict=TRUE
        grob <- grid.get(gName)
        bb <- grobBBDevicePixels(grob, vpPath, dpi=dpi)
        bb$name <- gName
        bb$vpPath <- vpPath
        ## construct a display name
        displayName <- as.character(grob)
        depth <- objs$vpDepth[i] - 1
        if (depth > 0)
            displayName <- paste(paste(rep(".", depth), collapse=""),
                              displayName, sep="")
        bb$displayName <- displayName
        if (draw) {
            ## draw bounding box and grob name
            grid.rect(x=mean(bb$x), y=mean(bb$y),
                      width=diff(bb$x), height=diff(bb$y),
                      default.units="native", gp=gp.box,
                      name="TMP_BOUNDBOX")
            grid.text(label=gName, x=bb$x[1], y=bb$y[1],
                      just=c("left", "top"),
                      default.units="native", gp=gp.text,
                      name="TMP_BOUNDBOX")
        }
        bblist[[i]] <- bb
    }
    ## remove annotations from display list
    ## but do not redraw it yet (so still visible)
    ## NOTE: after this grid.ls() fails, "evaluation nested too deeply"
    if (draw)
        grid.remove("TMP_BOUNDBOX", global=TRUE, redraw=FALSE)
    invisible(bblist)
}
