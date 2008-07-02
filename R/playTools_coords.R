## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### COORDS

toolConstructors$coords <- function(playState)
{
    coordsLabel <- gtkLabel()
    coordsLabel$setMarkup("<tt>     </tt>")
    playState$widgets$coordsLabel <- coordsLabel
    ## add click event handler to plot -- always active
    if (is.null(playState$widgets$plotClickEventSig)) {
        playState$widgets$plotClickEventSig <-
            gSignalConnect(playState$widgets$drawingArea,
                           "button-press-event", coords_click_handler, data=playState)
    }
    widget <- gtkToolItem()
    widget$add(coordsLabel)
    widget
}

coords_click_handler <- function(widget, event, playState)
{
    if (!isTRUE(playState$plot.ready)) return(FALSE)
    if (playState$.need.reconfig) generateSpaces(playState)
    x <- event$x
    y <- event$y
    coordsTxt <- "<tt>     </tt>"
    space <- whichSpace(playState, x, y)
    if (space != "page") {
        xy <- deviceCoordsToSpace(playState, x, y, space=space)
        xy <- spaceCoordsToDataCoords(playState, xy)
        x <- format(xy$x, nsmall=4)
        y <- format(xy$y, nsmall=4)
        x <- substr(x, 1, 5)
        y <- substr(y, 1, 5)
        coordsTxt <- paste("<tt>", x, "\n", y, "</tt>", sep="")
    }
    playState$widgets$coordsLabel$setMarkup(coordsTxt)
    return(FALSE)
}
