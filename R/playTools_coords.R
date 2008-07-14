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
    widget <- gtkToolItem()
    widget$add(coordsLabel)
    widget
}

## this is called by clickhandler tool
coordsCore <- function(playState, foo) {
    if (!("coords" %in% names(playState$tools))) return()
    coords <- foo$coords
    ## convert from log scale if necessary
    coords <- spaceCoordsToDataCoords(playState, coords)
    coordsTxt <- "<tt>     </tt>"
    if (!is.null(coords)) {
        x <- format(coords$x, nsmall=4)
        y <- format(coords$y, nsmall=4)
        x <- substr(x, 1, 5)
        y <- substr(y, 1, 5)
        coordsTxt <- paste("<tt>", x, "\n", y, "</tt>", sep="")
    }
    playState$widgets$coordsLabel$setMarkup(coordsTxt)
}
