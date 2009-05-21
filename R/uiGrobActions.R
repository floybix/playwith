## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer


updateGrobActions <- function(playState)
{
    aGroup <- playState$actionGroups[["PlotActions"]]
    hasGrobs <- (length(grid.ls(print = FALSE)$name) > 0)
    aGroup$getAction("GrobInspector")$setVisible(hasGrobs)
}

grob.inspector_handler <- function(widget, playState)
{
    ## show and return bounding boxes for all grobs
    #bblist <- showGrobsBB(draw = FALSE)
    #if (length(bblist) == 0) stop("No grobs found.")

    foo <- playPointInput(playState,
                          paste("Click on an object to see details,",
                                "Shift-click to destroy;",
                                "Right-click or Esc to cancel."))
    grid.refresh()
    if (is.null(foo)) return()
    isShift <- (foo$modifiers & GdkModifierType["shift-mask"])
    x.px <- foo$dc$x
    y.px <- foo$dc$y
    ## get a list of all grobs in the scene
    #upViewport(0)
#    objs <- as.data.frame(unclass(grid.ls(view=TRUE, print=FALSE)),
#                          stringsAsFactors=FALSE)
#    objs <- objs[objs$type == "grobListing",]
    ## build menu of all grobs the click touched
    menu <- gtkMenu()
    headItem <- gtkMenuItem("Choose object to see its details:")
    if (isShift) headItem <- gtkMenuItem("Choose object to destroy:")
    headItem["sensitive"] <- FALSE
    menu$append(headItem)
    #nhits <- 0

    grobNames <- identifyGrob(list(x = x.px, y = y.px))
    if (length(grobNames) == 0) return()
    for (name in grobNames) {
        item <- gtkMenuItem(name)
        gSignalConnect(item, "activate",
                       function(widget, user.data) {
                           if (isShift)
                               grid.remove(user.data)
                           else
                               str(grid.get(user.data))
                       },
                       data = name)
        menu$append(item)
    }
    ## show the menu
    menu$popup(button=0, activate.time=gtkGetCurrentEventTime())
    while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
}



NOTHING <- function() {





    for (i in length(bblist):1) {
        obj <- bblist[[i]]
#        vpPath <- objs$vpPath[i]
#        if (vpPath == "") vpPath <- NULL
#        gName <- objs$name[i]
#        ## TODO: objs$gPath[i] // strict=TRUE
#        grob <- grid.get(gName)

        name <- obj$name
        x <- obj$x
        y <- obj$y
        if ((min(x) <= x.px) && (x.px <= max(x)) &&
            (min(y) <= y.px) && (y.px <= max(y))) {
#        if (inGrobBB(x.px, y.px, grob=grob, vpPath)) {
#            itemName <- as.character(grob)
            itemName <- obj$displayName
            item <- gtkMenuItem(itemName)
            gSignalConnect(item, "activate",
                           function(widget, user.data) {
                               if (isShift)
                                   grid.remove(user.data)
                               else
                                   str(grid.get(user.data))
                           },
                           data = name)
            menu$append(item)
            nhits <- nhits + 1
        }
    }
    if (nhits == 0) return()

    ## show the menu
    menu$popup(button=0, activate.time=gtkGetCurrentEventTime())
    while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
}

