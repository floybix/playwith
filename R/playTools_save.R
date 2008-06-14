## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### SAVE

toolConstructors$save <- function(playState)
{
    saveButton <- gtkMenuToolButton(gtkImageNewFromStock("gtk-save-as",
                                                         size=GtkIconSize["small-toolbar"]), label="Save as")
    gSignalConnect(saveButton, "clicked", save_handler,
                   data=list(playState=playState))
    saveMenu <- gtkMenu()
    items <- list(
                  pdf=gtkMenuItem("PDF"),
                  png=gtkMenuItem("PNG (bitmap)"),
                  jpg=gtkMenuItem("JPEG"),
                  ps=gtkMenuItem("PostScript"),
                  eps=gtkMenuItem("EPS"),
                  svg=gtkMenuItem("SVG"),
                  wmf=gtkMenuItem("WMF"),
                  fig=gtkMenuItem("xfig")
                  )
    for (x in names(items)) {
        saveMenu$append(items[[x]])
        gSignalConnect(items[[x]], "activate", save_handler,
                       data=list(playState=playState, ext=x))
    }
    saveButton$setMenu(saveMenu)
    saveButton
}

save_handler <- function(widget, user.data)
{
    playState <- user.data$playState
    ## disable toolbars until this is over
    playFreezeGUI(playState)
    on.exit(playThawGUI(playState))
    ## get filename
    myExt <- if (!is.null(user.data$ext))
        user.data$ext else "pdf"
    myDefault <- if (is.null(playState$title)) "plot" else playState$title
    myDefault <- paste(myDefault, myExt, sep=".")
    myTitle <- paste("Save plot to file",  if (is.null(user.data$ext)) ""
    else paste("(", myExt, ")", sep=""))
    filename <- gfile(myTitle, type="save", initialfilename=myDefault)
    okExt <- c("pdf","png","jpg","jpeg","ps","eps","svg","wmf","emf","fig")
    if (is.na(filename)) return()
    ext <- tolower(get.extension(filename))
    if ( (!is.null(user.data$ext) && (ext != myExt)) ||
        ((ext %in% okExt) == FALSE) ) {
        filename <- paste(filename, myExt, sep=".")
        ext <- myExt
    }
    ## save plot to file
    playDevSet(playState)
    ## note: baseViewports will be corrupted if device size changes
    ## so need to keep the same size with dev.copy()...
    da <- playState$widgets$drawingArea
    w.px <- da$getAllocation()$width
    h.px <- da$getAllocation()$height
    ## assuming 96 d.p.i. in both dimensions
    w.in <- w.px / 96
    h.in <- h.px / 96
    if (ext %in% "pdf") {
        dev.copy(pdf, file=filename, width=w.in, height=h.in)
        dev.off()
    }
    else if (ext %in% "ps") {
        dev.copy(postscript, file=filename, width=w.in, height=h.in)
        dev.off()
    }
    else if (ext %in% "eps") {
        dev.copy(postscript, file=filename, width=w.in, height=h.in,
                 horizontal=FALSE, onefile=FALSE, paper="special")
        dev.off()
    }
    else if (ext %in% "png") {
        dev.copy(Cairo_png, file=filename, width=w.in, height=h.in)
        dev.off()
    }
    else if (ext %in% c("jpeg","jpg")) {
        dev.copy(jpeg, file=filename, width=w.px, height=h.px, units="px")
        dev.off()
    }
    else if (ext %in% "svg") {
        dev.copy(Cairo_svg, file=filename, width=w.in, height=h.in)
        dev.off()
    }
    else if (ext %in% c("wmf", "emf")) {
        dev.copy(win.metafile, file=filename, width=w.in, height=h.in)
        dev.off()
    }
    else if (ext %in% "fig") {
        dev.copy(xfig, file=filename, width=w.in, height=h.in)
        dev.off()
    }
    else {
        gmessage.error("Unrecognised filename extension")
        return()
    }
}
