## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

globalActionGroup <- function(playState)
{
    entries <-
        list( ## : name, stock icon, label, accelerator, tooltip, callback
             list("Clone", "gtk-new", "Clo_ne window", "<Ctrl>N", "Duplicate this window", clone_handler),
             list("Save", "gtk-save", "_Save", "<Ctrl>S", "Export current plot to an image file", save_handler),
             list("Copy", "gtk-copy", "_Copy", "<Ctrl>C", "Copy current plot as an image", copy_handler),
             list("Print", "gtk-print", "_Print", "<Ctrl>P", "Print current plot", print_handler),
             list("Close", "gtk-close", "Close", "<Ctrl>W", "Close window and device", close_handler),
             list("SetSize", NULL, "Set device _size...", "<Ctrl>0", NULL, set.size_handler),
             list("IncrFont", NULL, "_Increase font size", "<Ctrl>plus", NULL, incr.font_handler),
             list("DecrFont", NULL, "De_crease font size", "<Ctrl>minus", NULL, decr.font_handler),
             list("CustomTheme", "gtk-select-color", "Custom _Theme...", NULL, "Customise plot style...", custom.theme_handler),
             list("EditCall", "gtk-edit", "_Edit call...", "<Ctrl>E", "Edit the plot call", edit.call_handler),
             list("Back", "gtk-go-back", "Back", "<Alt>Left", "Go back to previous plot call", back_handler),
             list("Forward", "gtk-go-forward", "Forward", "<Alt>Right", "Go to next plot call", forward_handler),
             list("Redraw", "gtk-refresh", "Re_draw", "<Ctrl>R", NULL, redraw_handler),
             list("Reload", "gtk-refresh", "_Reload and redraw", "<Ctrl><Shift>R", NULL, reload_handler),
             list("SaveCode", "gtk-save", "Save c_ode", "<Ctrl><Shift>S", "Save R code for this plot and (optionally) data", save.code_handler),
             list("Source", NULL, "Plot s_ource", "<Ctrl>U", NULL, view.source_handler),
             list("HelpPlot", "gtk-help", "_Help for this plot", "F1", "Open help page for this plot", help_handler),
             list("HelpPlaywith", NULL, "help(playwith)", NULL, NULL, help.playwith_handler),
             list("About", NULL, "_About playwith", NULL, NULL, about_handler),
             list("Website", NULL, "_Website", NULL, "The playwith website (for contact, bugs, etc)", website_handler),
             list("SummariseData", NULL, "Summarise object"),
             list("EditData", NULL, "Edit object"),
             list("SaveData", NULL, "Save object"),
             list("PurgeObjects", NULL, "Purge object cache")
             )

    toggleEntries <-
        list( ## : name, stock icon, label, accelerator, tooltip, callback, active?
             list("Keep", "gtk-media-stop", "_Do not replace", "<Ctrl>D", "Do not replace with the next plot", keep_handler, FALSE),
             list("StayOnTop", "gtk-leave-fullscreen", "St_ay on top", NULL, "Show this window above all others", stay.on.top_handler, FALSE),
             list("Toolbars", NULL, "Toolbars", NULL, NULL, show.toolbars_handler, TRUE),
             list("Statusbar", NULL, "Status _bar", NULL, NULL, show.statusbar_handler, TRUE)
             )

    ## construct action group with playState passed to callbacks
    aGroup <- gtkActionGroupNew("GlobalActions")
    aGroup$addActions(entries, playState)
    aGroup$addToggleActions(toggleEntries, playState)
    aGroup
}

updateGlobalActions <- function(playState)
{
    aGroup <- playState$actionGroups[["GlobalActions"]]
    ## Back, Forward
    callEntry <- playState$widgets$callEntry
    nHistory <- callEntry$getModel()$iterNChildren()
    aGroup$getAction("Forward")$setSensitive(callEntry["active"] > 0)
    aGroup$getAction("Back")$setSensitive(callEntry["active"] < nHistory-1)
    ## Keep
    aGroup$getAction("Keep")$setActive(isTRUE(playState$keep))
    ## StayOnTop
    aGroup$getAction("StayOnTop")$setActive(isTRUE(playState$stay.on.top))
    ## Statusbar
    aGroup$getAction("Statusbar")$setActive(isTRUE(playState$show.statusbar))
    ## Toolbars
    aGroup$getAction("Toolbars")$setActive(isTRUE(playState$show.toolbars))
}

clone_handler <- function(widget, playState)
    NA

save_handler <- function(widget, playState)
{
    ## disable toolbars until this is over
    playFreezeGUI(playState)
    on.exit(playThawGUI(playState))
    ## get filename
    myExt <- playwith.getOption("save.as.format")
    myDefault <- if (!is.null(playState$title))
        playState$title else playState$callName
    myDefault <- paste(myDefault, myExt, sep=".")
    ## construct save file dialog
    okExt <- c("pdf","png","jpg","jpeg","ps","eps","svg","wmf","emf","fig")
    filter <- list("All files" = list(patterns = c("*")),
                   "PDF" = list(patterns = c("*.pdf")),
                   "PNG (bitmap)" = list(patterns = c("*.png")),
                   "EPS" = list(patterns = c("*.eps")),
                   "SVG" = list(patterns = c("*.svg")),
                   "WMF (metafile)" = list(patterns = c("*.wmf", "*.emf")),
                   "JPEG" = list(patterns = c("*.jpg", "*.jpeg")),
                   "xfig" = list(patterns = c("*.fig")))
    ## TODO: pdfWriter with bitmap()
    filename <- gfile("Export plot to image file", type = "save",
                      filter = filter, initialfilename = myDefault)
    if (is.na(filename)) return()
    ext <- tolower(get.extension(filename))
    if ((ext %in% okExt) == FALSE) {
        filename <- paste(filename, myExt, sep=".")
        ext <- myExt
    }
    ## save plot to file
    playDevSet(playState)
    ## note: baseViewports will be corrupted if device size changes
    ## so need to keep the same size with dev.copy()...
    w.in <- dev.size("in")[1]
    h.in <- dev.size("in")[2]
    w.px <- dev.size("px")[1]
    h.px <- dev.size("px")[2]
    h.px <- da$getAllocation()$height
    ## TODO: pointsize = playState$pointsize
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

copy_handler <- function(widget, playState)
{
    ## disable toolbars until this is over
    playFreezeGUI(playState)
    on.exit(playThawGUI(playState))
    playDevSet(playState)
    w.in <- dev.size("in")[1]
    h.in <- dev.size("in")[2]
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

print_handler <- function(widget, playState)
{
    playDevSet(playState)
    isWindows <- (.Platform$OS.type == "windows")
    if (isWindows) dev.print(win.print)
    else dev.print()
}

close_handler <- function(widget, playState)
    window.close_handler(playState = playState)

edit.call_handler <- function(widget, playState)
{
    callTxt <-
        paste(deparse(playState$call, width=42, control=
                      playwith.getOption("deparse.options")),
              collapse="\n")
    repeat {
        newTxt <- guiTextInput(callTxt, title="Edit plot call",
                               prompt="", accepts.tab=F)
        ## possible with gWidgets, but way too slow.
        #txtBox <- gtext(callTxt, font.attr=c(family="monospace"), wrap=FALSE, width=600)
        #gbasicdialog(title="Edit plot call", widget=txtBox,
        #             action=environment(), handler=function(h, ...)
        #             assign("newTxt", svalue(h[[1]]), env=h$action)
        #             )
        if (is.null(newTxt)) break
        callTxt <- newTxt
        tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
        ## check whether there was a syntax error
        if (inherits(tmp, "error")) {
            gmessage.error(conditionMessage(tmp))
        } else {
            ## if more than one call, wrap them in braces
            playState$call <- if (length(tmp) > 1)
                as.call(c(as.symbol("{"), tmp)) else tmp[[1]]
            playNewPlot(playState)
            break
        }
    }
    playState$win$present()
}

edit.call.inline_handler <- function(widget, playState)
{
    ## the original call
    callTxt <- deparseOneLine(playState$call, control=
                              playwith.getOption("deparse.options"))
    newTxt <- widget["text"]
    if (identical(newTxt, callTxt)) return()
    if (identical(newTxt, "")) return()
    tmp <- tryCatch(parse(text=newTxt), error=function(e)e)
    ## check whether there was a syntax error
    if (inherits(tmp, "error")) {
        gmessage.error(conditionMessage(tmp))
    } else {
        ## if more than one call, wrap them in braces
        playState$call <- if (length(tmp) == 1) tmp[[1]]
            else as.call(c(as.symbol("{"), tmp))
        playNewPlot(playState)
    }
    playState$win$present()
}

set.size_handler <- function(widget, playState) {
    da <- playState$widgets$drawingArea
    owidth <- da$getAllocation()$width
    oheight <- da$getAllocation()$height
    ## prompt user for new size
    widthW <- gedit(toString(owidth), width=7, coerce.with=as.numeric)
    heightW <- gedit(toString(oheight), width=7, coerce.with=as.numeric)
    unitVals <- c("pixels", "cm", "inches", "percent")
    ## functions to convert between selected units and pixels
    dpi <- dev.size("px")[1] / dev.size("in")[1]
    px2a <- function(px, unit, ref.px)
        signif(switch(unit, pixels = round(px), percent = 100 * (px/ref.px),
                      inches = px/dpi, cm = 2.54 * px/dpi), 3)
    a2px <- function(a, unit, ref.px)
        round(switch(unit, pixels = a, percent = ref.px * (a/100),
                      inches = a*dpi, cm = (a / 2.54)*dpi))
    unitsW <- gcombobox(unitVals, handler = function(h, ...) {
        wnum <- px2a(owidth, svalue(h$obj), owidth)
        hnum <- px2a(oheight, svalue(h$obj), oheight)
        svalue(widthW) <- toString(wnum)
        svalue(heightW) <- toString(hnum)
        })
    lay <- glayout()
    lay[1,1] <- "Width: "
    lay[2,1] <- "Height: "
    lay[1,2] <- widthW
    lay[2,2] <- heightW
    lay[2,3] <- unitsW
    width <- height <- NA
    result <- gbasicdialog(title="Set device size", widget=lay,
                           handler=function(...) {
                               unit <- svalue(unitsW)
                               width <<- a2px(svalue(widthW), unit, owidth)
                               height <<- a2px(svalue(heightW), unit, oheight)
                           })
    playState$win$present()
    if (!isTRUE(result)) return()
    width <- max(10, width)
    height <- max(10, height)
    da$setSizeRequest(width, height)
    playState$win$resize(1, 1) ## as small as possible
    ## try to force resize
    gdkWindowProcessAllUpdates()
    while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
    ## remove constraint after resize
    da$setSizeRequest(-1, -1)
}

incr.font_handler <- function(widget, playState)
    NA
decr.font_handler <- function(widget, playState)
    NA

custom.theme_handler <- function(widget, playState)
    NA

back_handler <- function(widget, playState) {
    with(playState$widgets,
        callEntry["active"] <- callEntry["active"] + 1)
}

forward_handler <- function(widget, playState) {
    with(playState$widgets,
        callEntry["active"] <- callEntry["active"] - 1)
}

redraw_handler <- function(widget, playState)
    playReplot(playState)

reload_handler <- function(widget, playState)
    playNewPlot(playState)


save.code_handler <- function(widget, playState)
    NA

view.source_handler <- function(widget, playState)
    NA

help_handler <- function(widget, playState)
{
    if (playState$accepts.arguments == FALSE) {
        gmessage.error("Do not know the name of the plot function.")
        return()
    }
    ## work out which (S3) method was called, if any
    callName <- playState$callName
    methNames <- methods(callName)
    if (length(methNames) > 0) {
        myClass <- try(class(callArg(playState, 1)), silent=TRUE)
        if (!inherits(myClass, "try-error")) {
            myMeth <- paste(callName, myClass, sep=".")
            ok <- (myMeth %in% methNames)
            if (any(ok)) callName <- myMeth[ok][1]
        }
    }
    print(help(callName))
}

help.playwith_handler <- function(widget, playState)
    print(help("playwith"))

about_handler <- function(widget, playState) {
    activate.email <- function(about, link, data)
        browseURL(paste("mailto:", link, sep=""))
    activate.url <- function(about, link, data)
        browseURL(link)
    gtkAboutDialogSetEmailHook(activate.email)
    gtkAboutDialogSetUrlHook(activate.url)

    ## TODO: this shows the wrong name! ("Rterm.exe")
    gtkShowAboutDialog(playState$win,
                       title = "playwith",
                       name = "playwith",
                       version = packageDescription("playwith")$Version,
                       comments = "interactive plots in R using GTK+",
                       copyright = "(C) 2008 Felix Andrews",
                       license = "GNU General Public Licence version 2 or later",
                       website = "http://playwith.googlecode.com/",
                       authors = c("Felix Andrews <felix@nfrac.org>"),
                       documenters = c("Felix Andrews")
                       )
}

website_handler <- function(...)
    browseURL("http://playwith.googlecode.com/")

keep_handler <- function(widget, playState)
    playState$keep <- widget["active"]

stay.on.top_handler <- function(widget, playState) {
    playState$stay.on.top <- widget["active"]
    playState$win$setKeepAbove(widget["active"])
}

show.statusbar_handler <- function(widget, playState) {
    playState$widgets$statusbarBox["visible"] <- widget["active"]
}

show.toolbars_handler <- function(widget, playState) {
    playState$widgets$leftToolbar["visible"] <- widget["active"]
}
