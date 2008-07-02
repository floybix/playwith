## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### ARROW

toolConstructors$arrow <- function(playState)
{
    quickTool(playState,
              label = "Arrow",
              icon = "gtk-connect",
              tooltip = "Add an arrow to the plot",
              f = arrow_handler)
}

arrow_handler <- function(widget, playState)
{
    pageAnnotation <- identical(playState$annotation.mode, "page")
    foo <- playLineInput(playState, prompt=
                         "Click and drag to draw an arrow. (Right-click to cancel)")
    if (is.null(foo)) return()
    if (is.null(foo$coords)) pageAnnotation <- TRUE
    if (foo$is.click) return()
    space <- foo$space
    if (pageAnnotation) space <- "page"
    myXY <- if (space == "page") foo$ndc else foo$coords
    myXY$x <- signif(myXY$x, 8)
    myXY$y <- signif(myXY$y, 8)
    annot <- call("grid.lines", x=myXY$x, y=myXY$y)
    if (space != "page") annot$default.units <- "native"
    annot$arrow <- playState$arrow.arrow
    style <- eval(playState$arrow.style)
    if (inherits(style, "gpar"))
        style <- as.call(c(quote(gpar), style))
    if (is.null(playState$arrow.style)) {
        ## default style is taken (at plot time) from lattice settings
        style <- quote(do.call(gpar, trellis.par.get("add.line")))
    }
    if (!is.null(style)) annot$gp <- style
    ## draw it
    playDo(playState, eval(annot), space=space,
           clip.off=identical(playState$clip.annotations, FALSE))
    ## store it
    playState$annotations[[space]] <-
        c(playState$annotations[[space]], annot)
    ## update other tool states
    with(playState$tools, {
        if (exists("edit.annotations", inherits=F))
            edit.annotations["visible"] <- TRUE
        if (exists("clear", inherits=F))
            clear["visible"] <- TRUE
    })
}
