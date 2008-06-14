## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### COPY

toolConstructors$copy <- function(playState)
{
    quickTool(playState,
              label = "Copy",
              icon = "gtk-copy",
              tooltip = "Copy this plot to the clipboard (as a bitmap)",
              f = copy_handler)
}

copy_handler <- function(widget, playState)
{
    ## disable toolbars until this is over
    playFreezeGUI(playState)
    on.exit(playThawGUI(playState))
    playDevSet(playState)
    da <- playState$widgets$drawingArea
    w.px <- da$getAllocation()$width
    h.px <- da$getAllocation()$height
    w.in <- w.px / 96
    h.in <- h.px / 96
    if (exists("win.metafile")) { ## i.e. in MS Windows
        copy.exts <- c("wmf", "png")
        copy.labels <- c("Windows Metafile (wmf)", "Bitmap (png)")
        sel.label <- select.list(copy.labels, preselect=copy.labels[1],
                                 title="Copy in which format?")
        playState$win$present()
        if (sel.label == "") return()
        copy.ext <- copy.exts[copy.labels == sel.label]
        if (copy.ext == "wmf") {
            dev.copy(win.metafile, width=w.in, height=h.in)
            dev.off()
            playState$win$present()
            return()
        }
    }
    ## save plot to temporary file, to copy as png
    filename <- paste(tempfile(), ".png", sep="")
    dev.copy(Cairo_png, file=filename, width=w.in, height=h.in)
    dev.off()
    im <- gdkPixbufNewFromFile(filename)$retval
    gtkClipboardGet("CLIPBOARD")$setImage(im)
    file.remove(filename)
}
