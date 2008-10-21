##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

## LICENSE
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version. See the file gpl-license.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

panel.usertext <-
    function(x, y = NULL, labels = seq_along(x), col = user.text$col,
             alpha = user.text$alpha, cex = user.text$cex, srt = 0, lineheight = user.text$lineheight,
             font = user.text$font, fontfamily = user.text$fontfamily, fontface = user.text$fontface,
             adj = c(0.5, 0.5), pos = NULL, offset = 0.5, ...)
{
    user.text <- current.user.text()
    panel.text(x, y, labels, col = col, alpha = alpha, cex = cex, srt = srt,
               lineheight = lineheight, font = font, fontfamily = fontfamily,
               fontface = fontface, adj = adj, pos = pos, offset = offset, ...)
}

current.user.text <- function() {
    user.text <- trellis.par.get("user.text")
    if (is.null(eval(user.text))) {
        user.text <- trellis.par.get("add.text")
    }
    user.text
}

current.brush.symbol <- function() {
    brush.symbol <- trellis.par.get("brush.symbol")
    if (is.null(eval(brush.symbol))) {
        ## defaults:
        brush.symbol <-
            list(pch = 21, col = "black", fill = "yellow",
                 alpha = 1, cex = 0.8, font = 1)
        plot.symbol <- trellis.par.get("plot.symbol")
        ## take cex from plot.symbol
        brush.symbol$cex <- plot.symbol$cex
        ## use filled equivalent to current plot symbol
        ppch <- as.character(plot.symbol$pch)
        pch <- 21 ## circle, default
        if (ppch %in% c("0", "7", "12", "15", "22")) {
            pch <- 22 ## square
        } else if (ppch %in% c("5", "9", "18", "23")) {
            pch <- 23 ## diamond
        } else if (ppch %in% c("2", "17", "24")) {
            pch <- 24 ## up triangle
        } else if (ppch %in% c("6", "25")) {
            pch <- 25 ## down triangle
        }
        brush.symbol$pch <- pch
    }
    brush.symbol
}

current.brush.line <- function() {
    brush.line <- trellis.par.get("brush.line")
    if (is.null(eval(brush.line))) {
        ## defaults:
        brush.line <- list(col = "red", alpha = 1,
                           lwd = 2, lty = 1)
    }
    brush.line
}

panel.brushpoints <-
    function(x, y = NULL, col = brush.symbol$col, pch = brush.symbol$pch,
             alpha = brush.symbol$alpha, fill = brush.symbol$fill, cex = brush.symbol$cex, ...)
{
    brush.symbol <- current.brush.symbol()
    panel.points(x, y, col = col, pch = pch, alpha = alpha,
                 fill = fill, cex = cex, ...)
}

panel.brushlines <-
    function(x, y = NULL, type = "l", col = brush.line$col,
             alpha = brush.line$alpha, lty = brush.line$lty,
             lwd = brush.line$lwd, ...)
{
    brush.line <- current.brush.line()
    panel.lines(x, y, type = type, col = col, alpha = alpha,
                lty = lty, lwd = lwd, ...)
}

latticeStyleGUI <-
    function(width = 480, height = 480, pointsize = 12,
             target.device = dev.cur(),
             base.graphics = FALSE)
{
                                        #if (!require("gWidgets", quiet = TRUE))
                                        #    stop("This function requires the gWidgets package")
    force(target.device)
    if (target.device == 1) target.device <- NULL
    pars <- NULL
    devType <- NULL
    if (!is.null(target.device)) {
        dev.set(target.device)
        devType <- .Device
        pars <- trellis.par.get()
    }
    ## GRAPHICAL PARAMETER LISTS
    colList <- palette()
    familyList <-
        c("serif", "sans", "mono",
          "HersheySerif", "HersheySans", "HersheyScript",
          "HersheyGothicEnglish", "HersheyGothicGerman", "HersheyGothicItalian",
          "HersheySymbol", "HersheySansSymbol")
    faceList <-
        c("plain", "bold", "italic", "bold.italic", "symbol",
          "cyrillic", "cyrillic.oblique", "EUC")
    pchList <-
        list(`open circle` = 1,
             `open square` = 0,
             `open diamond` = 5,
             `open triangle` = 2,
             `open tri.down` = 6,
             `solid circle` = 16,
             `solid square` = 15,
             `solid diamond` = 18,
             `solid triangle` = 17,
             `fill circle` = 21,
             `fill square` = 22,
             `fill diamond` = 23,
             `fill triangle` = 24,
             `fill tri.down` = 25,
             `plus (+)` = 3,
             `cross (x)` = 4,
             `star (*)` = 8,
             `dot (.)` = "."
             )
    ltyList <-
        c("solid", "dashed", "dotted",
          "dotdash", "longdash", "twodash", "blank")
    themeList <-
        alist(
              "Default" = standard.theme("pdf"),
              "WhiteBG" = col.whitebg(),
              "Greyscale (for print)" = standard.theme("postscript"),
              "DarkBG" = standard.theme("X11"),
              "ColorBrewer 1" = custom.theme(),
              "ColorBrewer 2" = custom.theme.2(),
              "ColorBrewer black" = custom.theme.black()
              )
    colRampsList <-
        alist(
              "grey.colors" = grey.colors(100),
              "rainbow" = rainbow(100),
              "heat.colors" = heat.colors(100),
              "terrain.colors" = terrain.colors(100),
              "topo.colors" = topo.colors(100),
              "cm.colors" = cm.colors(100)
              )
    brewerQualList <- rownames(subset(brewer.pal.info, category=="qual"))
    brewerSeqList <- rownames(subset(brewer.pal.info, category=="seq"))
    brewerDivList <- rownames(subset(brewer.pal.info, category=="div"))
    ## options for the graphic display
    plotDisplayList <-
        alist("Basic plot" = plot(latticeStyleDemo("plot")),
              "Superpose" = plot(latticeStyleDemo("superpose")),
              "Barchart" = plot(latticeStyleDemo("polygons")),
              "Levelplot" = plot(latticeStyleDemo("regions")),
              "All 4 panels" = plot(latticeStyleDemo()),
              "show.settings" = show.settings())

    doRedraw <- function(...) {
        checkDemoDevice()
        eval(plotDisplayList[[svalue(displayW)]])
    }

    ## list to keep the full user-defined theme
    assign("trellis.par.theme", list(), globalenv())
    ## list keeping track of user changes
    assign("trellis.par.log", list(), globalenv())

    checkDemoDevice <- function() {
        if (demoDevice %in% dev.list()) {
            dev.set(demoDevice)
        } else {
            ## looks like the demo device has closed.
            ## start a new device
            trellis.device(retain = TRUE)
            ## Note, retain = TRUE may not be enough because
            ## it may be a different device (eg Cairo vs X11)
            trellis.par.set(get("trellis.par.theme", globalenv()))
            demoDevice <<- dev.cur()
        }
    }

    updateTargetDeviceSettings <- function() {
        if ((length(devType) > 1) || (base.graphics)) {
            ## target device is different to demo;
            ## need to switch to it and update settings
            if (target.device %in% dev.list()) {
                dev.set(target.device)
                trellis.par.set(get("trellis.par.theme", globalenv()))
                if (base.graphics)
                    latticeStyleToBasePar()
            } else {
                ## looks like the target device has closed.
                warning(paste("It looks like the target device has closed.",
                              "Further changes will not apply to that device type.",
                              sep = "\n"))
                target.device <<- NULL
                checkDemoDevice()
                devType <- .Device
            }
        }
    }

    ## load a pre-defined theme
    setTheme <- function(h, ...) {
        if (length(get("trellis.par.log", globalenv())) > 0) {
            msg <- "Loading a theme will discard your changes. Continue?"
            if (!isTRUE(gconfirm(msg, icon = "warning")))
                return()
        }
        assign("trellis.par.log", list(), globalenv())
        checkDemoDevice()
        expr <- themeList[[ svalue(h$obj) ]]
        trellis.par.set(eval(expr))
        trellis.par.set(grid.pars = list(), strict = TRUE)
        trellis.par.set(user.text = NULL)
        assign("trellis.par.theme", trellis.par.get(), globalenv())
        if (svalue(autoRedrawW)) doRedraw()
        updateFromSettings()
        updateTargetDeviceSettings()
    }

    ## widget change handler to set the parameters
    setPar <- function(h, ...) {
        targets <- h$action
        val <- svalue(h$obj)
        mods <- list()
        ## blank entries should insert NULL into their targets
        ## (not just delete the items, because we are tracking changes)
        if (identical(val, "") || any(is.na(val)))
            val <- expression(NULL)
        for (tgt in targets) {
            ## check if target is vector element
            vecq <- regexpr("[", tgt, fixed = TRUE)
            if (vecq > -1) {
                ## if so, need to get current vector values first
                vec.tgt <- substr(tgt, 1, vecq-1)
                vec.expr <- parse(text = paste("mods", vec.tgt, sep = "$"))[[1]]
                par.expr <- parse(text = paste("trellis.par.theme", vec.tgt, sep = "$"))[[1]]
                eval(call("<-", vec.expr, par.expr))
            }
            expr <- parse(text = paste("mods", tgt, sep = "$"))[[1]]
            eval(call("<-", expr, val))
        }
        ## replace expression(NULL) with NULL:
        mods.ok <- rapply(mods, eval, how = "replace")
        ## update settings and track changes
        checkDemoDevice()
        trellis.par.set(mods.ok)
        assign("trellis.par.theme", trellis.par.get(), globalenv())
        tmplog <- get("trellis.par.log", globalenv())
        tmplog <- modifyList(tmplog, mods)
        if (identical(val, expression(NULL)))
            tmplog <- rapply(tmplog, eval, how = "replace")
        assign("trellis.par.log", tmplog, globalenv())
        if (svalue(autoRedrawW)) doRedraw()
        updateTargetDeviceSettings()
    }

    ## used for the coerce.with argument to pch widgets
    pchValue <- function(x) {
        match <- which(names(pchList) == x)
        if (length(match)) return(pchList[[ match[1] ]])
        ## otherwise just return the value as character
        ## TODO: test for integer?
        x
    }

    ## used for the coerce.with argument to lty widgets
    ## (the names can be used directly, but causes problems
    ##  with superpose.line if mixed names / numerics!)
    ltyValue <- function(x) {
        if (identical(x, "blank")) return(0)
        ## return index of item in ltyList
        match <- which(x == ltyList)
        if (length(match)) return(match)
        x
    }
    faceValue <- function(x) {
        ## return index of item in faceList
        match <- which(x == faceList)
        if (length(match)) return(match)
        x
    }

    ## update widget states to reflect current settings
    updateFromSettings <- function() {
        checkDemoDevice()
        par <- trellis.par.get()
        ## user.text is a custom entry; falls back to add.text
        if (is.null(eval(par$user.text))) {
            trellis.par.set(user.text = trellis.par.get("add.text"))
            par <- trellis.par.get()
        }
        ## don't assign NULL to widgets
        ok <- function(x) {
            if (is.null(x)) x <- ""
            x
        }
        colName <- function(x) {
            x <- switch(tolower(x),
                        "#000000" = "black", #
                        "#ffffff" = "white", #
                        "#ff0000" = "red",   #
                        "#00ff00" = "green", #
                        "#0000ff" = "blue",  #
                        "#00ffff" = "cyan",  #
                        "#ff00ff" = "magenta", #
                        "#ffff00" = "yellow",  #
                        x)
        }
        pchName <- function(x) {
            match <- sapply(pchList, identical, x)
            if (any(match)) return(names(pchList)[match])
            x
        }
        ltyName <- function(x) {
            if (identical(x, 0)) x <- "blank"
            if (is.numeric(x)) x <- ltyList[x]
            x
        }
        faceName <- function(x) {
            if (is.numeric(x)) x <- faceList[x]
            x
        }
        with(par, {
            ## points / lines styles; colors all ambiguous (symbol vs line)
            svalue(wid.super.list$col[[1]]) <- colName(plot.symbol$col)
            svalue(wid.super.list$pch[[1]]) <- pchName(plot.symbol$pch)
            svalue(wid.super.list$lty[[1]]) <- ltyName(plot.line$lty)
            ith <- function(x, i) rep(x, length = i)[i]
            for (i in 2:6) {
                svalue(wid.super.list$col[[i]]) <- colName(ith(superpose.symbol$col, i))
                svalue(wid.super.list$pch[[i]]) <- pchName(ith(superpose.symbol$pch, i))
                svalue(wid.super.list$lty[[i]]) <- ltyName(ith(superpose.line$lty, i))
            }
            ## general plot / line stuff; all ambiguous (plot vs superpose)
            svalue(wid.plot.symbol.cex) <- ok(plot.symbol$cex)
            svalue(wid.plot.symbol.alpha) <- ok(plot.symbol$alpha)
            svalue(wid.plot.line.lwd) <- ok(plot.line$lwd)
            svalue(wid.plot.symbol.fill) <- colName(plot.symbol$fill)
            ## polygons
            svalue(wid.poly.col[[1]]) <- colName(plot.polygon$col)
            for (i in 2:6) {
                svalue(wid.poly.col[[i]]) <- colName(ith(superpose.polygon$col, i))
            }
            svalue(wid.poly.border) <- colName(plot.polygon$border)
            ## regions: can not easily detect these, so just clear them
            svalue(wid.regions.builtin) <- ""
            svalue(wid.regions.seq) <- ""
            svalue(wid.regions.div) <- ""
            svalue(qualPalW) <- ""
            ## default font
            svalue(wid.fontsize) <- ok(fontsize$text)
            svalue(wid.fontfamily) <- ok(grid.pars$fontfamily)
            ## basics
            svalue(wid.background.col) <- colName(ok(background$col))
            svalue(wid.cex) <- ok(grid.pars$cex)
            svalue(wid.axes.col) <- colName(ok(axis.line$col)) ## ambiguous
            svalue(wid.axis.text.cex) <- ok(axis.text$cex)
            svalue(wid.main.col) <- colName(ok(par.main.text$col))
            svalue(wid.main.cex) <- ok(par.main.text$cex)
            svalue(wid.titles.col) <- colName(ok(par.xlab.text$col)) ## ambiguous
            svalue(wid.titles.cex) <- ok(par.xlab.text$cex) ## ambiguous
            svalue(wid.strip.bg.col) <- colName(ok(strip.background$col)[1]) ## ambiguous
            svalue(wid.strip.cex) <- ok(add.text$cex)
            ## user.text
            svalue(wid.user.text.col) <- colName(ok(user.text$col))
            svalue(wid.user.text.alpha) <- ok(user.text$alpha)
            svalue(wid.user.text.cex) <- ok(user.text$cex)
            svalue(wid.user.text.fontfamily) <- ok(user.text$fontfamily)
            svalue(wid.user.text.fontface) <- faceName(ok(user.text$fontface))
            svalue(wid.user.text.lineheight) <- ok(user.text$lineheight)
            ## add.line
            svalue(wid.add.line.col) <- colName(ok(add.line$col))
            svalue(wid.add.line.alpha) <- ok(add.line$alpha)
            svalue(wid.add.line.lty) <- ltyName(ok(add.line$lty))
            svalue(wid.add.line.lwd) <- ok(add.line$lwd)
            ## reference.line
            svalue(wid.ref.line.col) <- colName(ok(reference.line$col))
            svalue(wid.ref.line.alpha) <- ok(reference.line$alpha)
            svalue(wid.ref.line.lty) <- ltyName(ok(reference.line$lty))
            svalue(wid.ref.line.lwd) <- ok(reference.line$lwd)
        })
    }

    ## CUSTOM WIDGETS
    gdroplist <- function(..., width = 80) {
        foo <- gWidgets::gdroplist(...)
        size(foo) <- c(width, -1)
        foo
    }
    ggroupThin <- function(..., spacing = 1) {
        foo <- gWidgets::ggroup(..., spacing = spacing)
        ## remove outer border
        if (!inherits(guiToolkit(), "guiWidgetsToolkittcltk")) {
            ## tcltk fails here (gWidgetstcltk 0.0-15)
            svalue(foo) <- 0
        }
        foo
    }

    ## THE WINDOW LAYOUT
    win <- gwindow(title = "Lattice Style GUI")
    metagroup <- ggroup(horizontal = FALSE, container = win)
    displayg <- gframe("Display", horizontal = TRUE, container = metagroup)
                                        #font(displayg) <- list(weight="bold")
    hgroup <- ggroupThin(horizontal = TRUE, container = metagroup, expand = TRUE)
    vgroup <- ggroupThin(horizontal = FALSE, container = hgroup)
    ## add the graphics device
    gg <- ggraphics(width = width, height = height, ps = pointsize,
                    container = hgroup, expand = TRUE)
    trellis.device(new = FALSE, retain = TRUE) ## i.e. new device if needed
    ## set initial style from target device
    trellis.par.set(pars)
    assign("trellis.par.theme", trellis.par.get(), globalenv())
    ## store device ID -- trellis.par.set is specific to this device!
    demoDevice <- dev.cur()
    devType <- unique(c(devType, .Device))
    devTypeStr <- paste(devType, collapse = " and ")
    ## initial display
    grid::grid.newpage()
    grid::grid.text(paste(c("Loading...",
                            "",
                            "This device will show a preview",
                            "of your settings. The settings",
                            paste("will apply to", devTypeStr, "devices."),
                            "(You can set them for others too).",
                            "",
                            if (base.graphics)
                            c("This is base graphics mode (par).",
                              ""),
                            "Your full style settings are kept",
                            "in the object `trellis.par.theme`,",
                            "and your modifications only in",
                            "`trellis.par.log`.",
                            "",
                            "Changes take effect immediately.",
                            "Load a new theme to reset."),
                          collapse="\n"),
                    x = 0.05, y = 0.95, just = c("left", "top"),
                    gp = gpar())

    ## PLOT CONTROLS:
    displayW <- gradio(names(plotDisplayList), horizontal = TRUE,
                       handler = doRedraw, container = displayg)
    redrawW <- gbutton("Redraw", handler = doRedraw, container = displayg)
    autoRedrawW <- gcheckbox("Automatic", checked = TRUE, container = displayg)

    ## THEME
    themeg <- ggroup(horizontal = TRUE, container = vgroup)
    glabel("Load a theme:", container = themeg)
    wid.theme <- gdroplist(names(themeList), selected = 0, width = 150,
                           handler = setTheme, container = themeg)
    ## HELP
    gbutton("HELP", container = themeg,
            handler = function(...) print(help("latticeStyleGUI")) )

    ## the tabs containing most settings
    tabs <- gnotebook(container = vgroup)
    plotg <- ggroup(horizontal = FALSE, spacing = 2, container = tabs,
                    label = "Points & Lines")

    ## this is used to change the superpose style settings
    ## corresponding to a group of widgets (col / pch / lty)
    setStyleAndUpdate <- function(col = NULL, pch = NULL, lty = NULL) {
        ## set lattice parameters first, then update widgets from them
        checkDemoDevice()
        symbolList <- list()
        lineList <- list()
        if (!is.null(col)) { symbolList$col <- col; lineList$col <- col }
        if (!is.null(pch)) symbolList$pch <- pch
        if (!is.null(lty)) lineList$lty <- lty
        if (length(symbolList) > 0) {
            trellis.par.set(superpose.symbol = symbolList)
            trellis.par.set(plot.symbol = lapply(symbolList, head, 1))
        }
        if (length(lineList) > 0) {
            trellis.par.set(superpose.line = lineList)
            trellis.par.set(plot.line = lapply(lineList, head, 1))
        }
        ## update widgets
        updateFromSettings()
        ## finally, trigger update for main widget to record changes
        ## and to set other parameters for the main style (i.e. maintargets)
        ## (note superpose.*[] will be reset by the main widget update).
        ## suppress redraws
        odraw <- svalue(autoRedrawW)
        svalue(autoRedrawW) <- FALSE
        types <- c(if (!is.null(col)) "col",
                   if (!is.null(pch)) "pch",
                   if (!is.null(lty)) "lty")
        for (type in types)
            setPar(list(obj = wid.super.list[[type]][[1]],
                        action = maintargets[[type]]))
        svalue(autoRedrawW) <- odraw
        if (odraw) doRedraw()
    }

    makeMainStyle <- function(h, ...) {
        ## make this item the main style
        ## and move the rest down
        checkDemoDevice()
        which <- h$action
        col <- trellis.par.get("superpose.symbol")$col
        pch <- trellis.par.get("superpose.symbol")$pch
        lty <- trellis.par.get("superpose.line")$lty
        col <- c(col[which], col[-which])
        pch <- c(pch[which], pch[-which])
        lty <- c(lty[which], lty[-which])
        setStyleAndUpdate(col = col, pch = pch, lty = lty)
    }

    copyStyleToAll <- function(h, ...) {
        ## copy pch and lty settings from main plot style to all others
        checkDemoDevice()
        pch <- rep(trellis.par.get("plot.symbol")$pch, 6)
        lty <- rep(trellis.par.get("plot.line")$lty, 6)
        setStyleAndUpdate(pch = pch, lty = lty)
    }

    ## SUPERPOSED POINTS / LINES STYLES
    glabel("Main plot style:", anchor = c(0, 0), container = plotg)
    styg <- glayout(spacing = 1, container = plotg)
    wid.super.list <- list()
    wid.super.list$col <- list()
    maintargets <- list()
    maintargets$col <- c("plot.symbol$col", "plot.line$col",
                         "superpose.symbol$col[1]", "superpose.line$col[1]",
                         "box.rectangle$col", "box.umbrella$col", "dot.symbol$col")
    maintargets$pch <- c("plot.symbol$pch", "superpose.symbol$pch[1]")
    maintargets$lty <- c("plot.line$lty", "superpose.line$lty[1]")
    wid.super.list$col[[1]] <-
        gedit("", width = 10, container = styg,
              handler = setPar,
              action = maintargets$col)
    wid.super.list$pch <- list()
    wid.super.list$pch[[1]] <-
        gdroplist(names(pchList), selected = 0, container = styg,
                  editable = TRUE, coerce.with = pchValue, handler = setPar,
                  action = maintargets$pch)
    wid.super.list$lty <- list()
    wid.super.list$lty[[1]] <-
        gdroplist(ltyList, selected = 0, width = 60, container = styg,
                  editable = TRUE, coerce.with = ltyValue, handler = setPar,
                  action = maintargets$lty)
    styg[1,2] <- "Color:"
    styg[1,3] <- "Symbol:"
    styg[1,4] <- "Line:"
    styg[2,1] <- "Plot:"
    styg[2,2] <- wid.super.list$col[[1]]
    styg[2,3] <- wid.super.list$pch[[1]]
    styg[2,4] <- wid.super.list$lty[[1]]
    styg[2,5] <- gbutton("fill down", handler = copyStyleToAll, container = styg)
    styg[3,1:4] <- gseparator(container = styg)
    styg[4,1:4, anchor = c(-1,-1)] <-
        glabel("Superposed styles:", container = styg)
    styg[5,1] <- "2nd:"
    styg[6,1] <- "3rd:"
    styg[7,1] <- "4th:"
    styg[8,1] <- "5th:"
    styg[9,1] <- "6th:"
    targets <- c("superpose.symbol", "superpose.line")
    for (i in 2:6) {
        action.col <- paste(targets, "$col[", i, "]", sep = "")
        action.pch <- paste(targets[1], "$pch[", i, "]", sep = "")
        action.lty <- paste(targets[2], "$lty[", i, "]", sep = "")
        wid.super.list$col[[i]] <-
            gedit("", width = 10, container = styg,
                  handler = setPar, action = action.col)
        wid.super.list$pch[[i]] <-
            gdroplist(names(pchList), selected = 0, container = styg,
                      editable = TRUE, coerce.with = pchValue, handler = setPar,
                      action = action.pch)
        wid.super.list$lty[[i]] <-
            gdroplist(ltyList, selected = 0, width = 60, container = styg,
                      editable = TRUE, coerce.with = ltyValue, handler = setPar,
                      action = action.lty)
        styg[i+3, 2] <- wid.super.list$col[[i]]
        styg[i+3, 3] <- wid.super.list$pch[[i]]
        styg[i+3, 4] <- wid.super.list$lty[[i]]
        styg[i+3, 5] <- gbutton("main", container = styg,
                                handler = makeMainStyle, action = i)
    }
    visible(styg) <- TRUE

    ## SUPERPOSE COLOR PALETTES

    loadQualPal <- function(h, ..., lattice = FALSE, default = FALSE,
                            forPolygons = FALSE)
    {
        pal <- ""
        if (lattice) {
            pal <- standard.theme("pdf")$superpose.symbol$col
        } else if (default) {
            old <- palette("default")
            pal <- palette()
            palette(old)
        } else {
            pal <- brewer.pal(8, name = svalue(h$obj))
        }
        if (forPolygons)
            return(setPolyStyleAndUpdate(col = pal))
        ## default is for superpose styles
        setStyleAndUpdate(col = pal)
    }
    paletteg <- gframe("Load colors for points / lines", container = plotg)
    palg <- ggroupThin(horizontal = FALSE, container = paletteg)
    tmpg <- ggroupThin(horizontal = TRUE, container = palg)
    glabel("Load palette:", container = tmpg)
    gbutton("Lattice default", container = tmpg,
            handler = function(...) loadQualPal(lattice = TRUE))
    gbutton("R default", container = tmpg,
            handler = function(...) loadQualPal(default = TRUE))
    glabel(" or...", container = tmpg)
    ## load ColorBrewer Qual palette
    tmpg <- ggroupThin(horizontal = TRUE, container = palg)
    glabel("ColorBrewer Qualitative palette:", container = tmpg)
    qualPalW <- gdroplist(c("", brewerQualList), container = tmpg,
                          handler = loadQualPal)
    showQualW <- gbutton("Display Qual. palettes", container = palg,
                         handler = function(...) display.brewer.all(8, "qual"))

    ## GENERAL PROPERTIES
    genplg <- gframe("General points / lines properties", horizontal = FALSE,
                     container = plotg)
    tmp1g <- ggroupThin(container = genplg)
    tmp2g <- ggroupThin(container = genplg)
    glabel("Point scale:", container = tmp1g)
    wid.plot.symbol.cex <- gedit("", width = 4, container = tmp1g,
                                 coerce.with = as.numeric, handler = setPar,
                                 action = c("plot.symbol$cex", "superpose.symbol$cex[]")) # box.dot?
    glabel(" Alpha:", container = tmp1g)
    wid.plot.symbol.alpha <- gedit("", width = 4, container = tmp1g,
                                   coerce.with = as.numeric, handler = setPar,
                                   action = c("plot.symbol$alpha", "superpose.symbol$alpha[]"))
    glabel("Line width:", container = tmp2g)
    wid.plot.line.lwd <- gedit("", width = 4, container = tmp2g,
                               coerce.with = as.numeric, handler = setPar,
                               action = c("plot.line$lwd", "superpose.line$lwd[]"))
    glabel(" Points fill:", container = tmp2g)
    wid.plot.symbol.fill <- gedit("", width = 10, container = tmp2g,
                                  handler = setPar,
                                  action = c("plot.symbol$fill", "superpose.symbol$fill[]"))

    ## POLYGONS and REGIONS
    prg <- ggroup(horizontal = FALSE, spacing = 2, container = tabs,
                  label = "Polygons & Regions")

    ## POLYGONS

    ## change the whole set of polygon colors at once
    setPolyStyleAndUpdate <- function(col) {
        checkDemoDevice()
        ## also see setStyleAndUpdate()
        trellis.par.set(superpose.polygon = list(col = col),
                        plot.polygon = list(col = col[1]))
        ## update widgets
        updateFromSettings()
        ## finally, trigger update for main widget to record changes
        ## (note superpose.polygon$col[] will be reset by the main widget update).
        setPar(list(obj = wid.poly.col[[1]],
                    action = c("plot.polygon$col", "superpose.polygon$col[1]")))
    }
    makeMainPoly <- function(h, ...) {
        ## make this item the main style
        ## and move the rest down
        checkDemoDevice()
        which <- h$action
        col <- trellis.par.get("superpose.polygon")$col
        col <- c(col[which], col[-which])
        setPolyStyleAndUpdate(col = col)
    }
    polyg <- gframe("Polygons (for barchart etc)", container = prg)
    lay <- glayout(spacing = 1, container = polyg)
    wid.poly.col <- list()
    wid.poly.col[[1]] <- gedit("", width = 10, container = lay,
                               handler = setPar,
                               action = c("plot.polygon$col", "superpose.polygon$col[1]"))
    wid.poly.border <- gedit("", width = 10, container = lay,
                             handler = setPar,
                             action = c("plot.polygon$border", "superpose.polygon$border[]"))
    lay[1,2] <- "Color:"
    lay[2,1] <- "Plot:"
    lay[2,2] <- wid.poly.col[[1]]
    lay[1,4] <- "Border:"
    lay[2,4] <- wid.poly.border
    lay[3,1:2] <- gseparator(container = lay)
    lay[4,1:2, anchor = c(-1,-1)] <-
        glabel("Superposed styles:", container = lay)
    lay[5,1] <- "2nd:"
    lay[6,1] <- "3rd:"
    lay[7,1] <- "4th:"
    lay[8,1] <- "5th:"
    lay[9,1] <- "6th:"
    for (i in 2:6) {
        action.col <- paste("superpose.polygon$col[", i, "]", sep = "")
        wid.poly.col[[i]] <-
            gedit("", width = 10, container = lay,
                  handler = setPar, action = action.col)
        lay[i+3, 2] <- wid.poly.col[[i]]
        lay[i+3, 3] <- gbutton("main", container = lay,
                               handler = makeMainPoly, action = i)
    }
    visible(lay) <- TRUE

    ## POLYGON COLOR PALETTES
    ## note, this code is a copy of that for superpose styles, above

    ppaletteg <- gframe("Load colors for polygons", container = prg)
    palg <- ggroupThin(horizontal = FALSE, container = ppaletteg)
    tmpg <- ggroupThin(horizontal = TRUE, container = palg)
    glabel("Load palette:", container = tmpg)
    gbutton("Lattice default", container = tmpg, handler =
            function(...) loadQualPal(lattice = TRUE, forPolygons = TRUE))
    gbutton("R default", container = tmpg, handler =
            function(...) loadQualPal(default = TRUE, forPolygons = TRUE))
    glabel(" or...", container = tmpg)
    ## load ColorBrewer Qual palette
    tmpg <- ggroupThin(horizontal = TRUE, container = palg)
    glabel("ColorBrewer Qualitative palette:", container = tmpg)
    qualPalW <- gdroplist(c("", brewerQualList), container = tmpg,
                          handler = function(...) loadQualPal(..., forPolygons = TRUE))
    showQualW <- gbutton("Display Qual. palettes", container = palg,
                         handler = function(...) display.brewer.all(8, "qual"))

    ## REGIONS

    setRegions <- function(h, ...) {
        if (svalue(h$obj) == "") return()
        if (h$action == "builtin") {
            svalue(wid.regions.seq) <- ""
            svalue(wid.regions.div) <- ""
            colval <- eval(colRampsList[[svalue(h$obj)]])
        }
        if (h$action == "seq") {
            svalue(wid.regions.builtin) <- ""
            svalue(wid.regions.div) <- ""
            colval <- brewer.pal(n = 9, name = svalue(h$obj))
            colval <- colorRampPalette(colval)(100)
        }
        if (h$action == "div") {
            svalue(wid.regions.builtin) <- ""
            svalue(wid.regions.seq) <- ""
            colval <- brewer.pal(n = 11, name = svalue(h$obj))
            colval <- colorRampPalette(colval)(100)
        }
        checkDemoDevice()
        tmp <- list(regions = list(col = colval))
        trellis.par.set(tmp)
        assign("trellis.par.theme", trellis.par.get(), globalenv())
        G <- globalenv()
        G$trellis.par.log <- modifyList(G$trellis.par.log, tmp)
        if (svalue(autoRedrawW)) doRedraw()
        updateTargetDeviceSettings()
    }

    revRegions <- function(...) {
        checkDemoDevice()
        colval <- rev(trellis.par.get("regions")$col)
        tmp <- list(regions = list(col = colval))
        trellis.par.set(tmp)
        assign("trellis.par.theme", trellis.par.get(), globalenv())
        G <- globalenv()
        G$trellis.par.log <- modifyList(G$trellis.par.log, tmp)
        if (svalue(autoRedrawW)) doRedraw()
        updateTargetDeviceSettings()
    }

    regiong <- gframe("Regions (color ramp palettes)", horizontal = FALSE,
                      container = prg)
    tmp1g <- ggroupThin(container = regiong)
    tmp2g <- ggroupThin(container = regiong)
    tmp3g <- ggroupThin(container = regiong)
    glabel("R's built-in palettes: ", container = tmp1g)
    wid.regions.builtin <- gdroplist(c("", names(colRampsList)),
                                     width = 120, container = tmp1g,
                                     handler = setRegions, action = "builtin")
    glabel("ColorBrewer Sequential:", container = tmp2g)
    wid.regions.seq <- gdroplist(c("", brewerSeqList), container = tmp2g,
                                 handler = setRegions, action = "seq")
    gbutton("Display Seq.", container = tmp2g,
            handler = function(...) display.brewer.all(type = "seq"))
    glabel("ColorBrewer Diverging: ", container = tmp3g)
    wid.regions.div <- gdroplist(c("", brewerDivList), container = tmp3g,
                                 handler = setRegions, action = "div")
    gbutton("Display Div.", container = tmp3g,
            handler = function(...) display.brewer.all(type = "div"))
    gbutton("Reverse", container = regiong, handler = revRegions)

    ## tab with extra settings (not about plot data itself)
    extrag <- ggroup(horizontal = FALSE, spacing = 2, container = tabs,
                     label = "Other settings")

    ## FONT
    fontg <- gframe("Default Font", horizontal = TRUE, container = extrag)
    glabel("Pt.size:", container = fontg)
    wid.fontsize <- gedit("", width = 3, container = fontg,
                          coerce.with = as.numeric,
                          handler = setPar, action = "fontsize$text")
    glabel(" Family:", container = fontg)
    wid.fontfamily <- gdroplist(familyList, selected = 0, container = fontg,
                                editable = TRUE, width = 100,
                                handler = setPar, action = "grid.pars$fontfamily")

    ## BASICS
    basicg <- gframe("Basics", horizontal = FALSE, container = extrag)
    lay <- glayout(spacing = 1, container = basicg)
    wid.background.col <- gedit("", width = 12, container = lay,
                                handler = setPar, action = "background$col")
    wid.cex <- gedit("", width = 4, container = lay,
                     coerce.with = as.numeric, handler = setPar,
                     action = c("grid.pars$cex", "grid.pars$lex"))
    wid.axes.col <- gedit("", width = 12, handler = setPar, container = lay,
                          action = c("axis.line$col", "axis.text$col",
                          "strip.border$col", "box.3d$col", "box.dot$col",
                          "plot.polygon$border", "superpose.polygon$border"))
    wid.axis.text.cex <- gedit("", width = 4, container = lay,
                               coerce.with = as.numeric, handler = setPar,
                               action = "axis.text$cex")
    wid.main.col <- gedit("", width = 12, container = lay,
                          handler = setPar, action = "par.main.text$col")
    wid.main.cex <- gedit("", width = 4, container = lay,
                          coerce.with = as.numeric, handler = setPar,
                          action = "par.main.text$cex")
    wid.titles.col <- gedit("", width = 12, handler = setPar, container = lay,
                            action = c("par.xlab.text$col", "par.ylab.text$col",
                            "par.zlab.text$col", "par.sub.text$col", "add.text$col"))
    wid.titles.cex <- gedit("", width = 4, container = lay,
                            coerce.with = as.numeric, handler = setPar,
                            action = c("par.xlab.text$cex", "par.ylab.text$cex",
                            "par.zlab.text$cex", "par.sub.text$cex"))
    wid.strip.bg.col <- gedit("", width = 12, container = lay,
                              handler = setPar, action = "strip.background$col[]")
    wid.strip.cex <- gedit("", width = 4, container = lay,
                           coerce.with = as.numeric, handler = setPar,
                           action = "add.text$cex")
    lay[1,2] <- "Color:"
    lay[1,3] <- "Text scale:"
    lay[2,1] <- "Background:"
    lay[2,2] <- wid.background.col
    lay[2,3] <- wid.cex
    lay[3,1] <- "Axes / box:"
    lay[3,2] <- wid.axes.col
    lay[3,3] <- wid.axis.text.cex
    lay[4,1] <- "Main title:"
    lay[4,2] <- wid.main.col
    lay[4,3] <- wid.main.cex
    lay[5,1] <- "Other titles:"
    lay[5,2] <- wid.titles.col
    lay[5,3] <- wid.titles.cex
    lay[6,1] <- "Strips:"
    lay[6,2] <- wid.strip.bg.col
    lay[6,3] <- wid.strip.cex
    visible(lay) <- TRUE

    ## USER.TEXT
    addtextg <- gframe("Annotations (user.text)", horizontal = FALSE,
                       container = extrag)
    svalue(addtextg) <- 1
    tmp1g <- ggroupThin(spacing = 0, container = addtextg)
    tmp2g <- ggroupThin(spacing = 0, container = addtextg)
    tmp3g <- ggroupThin(spacing = 0, container = addtextg)
    glabel("Color: ", container = tmp1g)
    wid.user.text.col <- gdroplist(colList, selected = 0, container = tmp1g,
                                   editable = TRUE, handler = setPar,
                                   action = "user.text$col")
    glabel(" Alpha:", container = tmp1g)
    wid.user.text.alpha <- gedit("", width = 4, container = tmp1g,
                                 coerce.with = as.numeric, handler = setPar,
                                 action = "user.text$alpha")
    glabel("Family:", container = tmp2g)
    wid.user.text.fontfamily <-
        gdroplist(familyList, selected = 0, container = tmp2g,
                  editable = TRUE, width = 100,
                  handler = setPar, action = "user.text$fontfamily")
    glabel(" Scale:", container = tmp2g)
    wid.user.text.cex <- gedit("", width = 4, container = tmp2g,
                               coerce.with = as.numeric, handler = setPar,
                               action = "user.text$cex")
    glabel("Face:  ", container = tmp3g)
    wid.user.text.fontface <-
        gdroplist(c("", faceList), container = tmp3g,
                  coerce.with = faceValue,
                  handler = setPar, action = "user.text$fontface")
    glabel(" Lineheight:", container = tmp3g)
    wid.user.text.lineheight <-
        gedit("", width = 4, container = tmp3g,
              coerce.with = as.numeric, handler = setPar,
              action = c("user.text$lineheight", "add.text$lineheight"))

    ## ADD.LINE
    addlineg <- gframe("Annotations (add.line)", horizontal = FALSE,
                       spacing = 0, container = extrag)
    tmp1g <- ggroupThin(container = addlineg)
    tmp2g <- ggroupThin(container = addlineg)
    glabel("Color:", container = tmp1g)
    wid.add.line.col <- gdroplist(colList, selected = 0, container = tmp1g,
                                  editable = TRUE, handler = setPar,
                                  action = "add.line$col")
    glabel(" Alpha:", container = tmp1g)
    wid.add.line.alpha <- gedit("", width = 4, container = tmp1g,
                                coerce.with = as.numeric, handler = setPar,
                                action = "add.line$alpha")
    glabel("Type:", container = tmp2g)
    wid.add.line.lty <- gdroplist(ltyList, selected = 0, container = tmp2g,
                                  editable = TRUE, handler = setPar,
                                  coerce.with = ltyValue,
                                  action = "add.line$lty")
    glabel(" Width:", container = tmp2g)
    wid.add.line.lwd <- gedit("", width = 4, container = tmp2g,
                              coerce.with = as.numeric, handler = setPar,
                              action = "add.line$lwd")

    ## REFERENCE.LINE
    reflineg <- gframe("Grids etc (reference.line)", horizontal = FALSE,
                       container = extrag)
    tmp1g <- ggroupThin(container = reflineg)
    tmp2g <- ggroupThin(container = reflineg)
    glabel("Color:", container = tmp1g)
    wid.ref.line.col <- gdroplist(colList, selected = 0, container = tmp1g,
                                  editable = TRUE, handler = setPar,
                                  action = "reference.line$col")
    glabel(" Alpha:", container = tmp1g)
    wid.ref.line.alpha <- gedit("", width = 4, container = tmp1g,
                                coerce.with = as.numeric, handler = setPar,
                                action = "reference.line$alpha")
    glabel("Type:", container = tmp2g)
    wid.ref.line.lty <- gdroplist(ltyList, selected = 0, container = tmp2g,
                                  editable = TRUE, handler = setPar,
                                  coerce.with = ltyValue,
                                  action = "reference.line$lty")
    glabel(" Width:", container = tmp2g)
    wid.ref.line.lwd <- gedit("", width = 4, container = tmp2g,
                              coerce.with = as.numeric, handler = setPar,
                              action = "reference.line$lwd")

    ## switch back to first tab
    svalue(tabs) <- 1

    updateFromSettings()
    doRedraw()

    ## release size constraint on graphics device
                                        #size(gg) <- c(-1, -1) # doesn't work

    return(invisible())
}

latticeStyleToBasePar <- function() {
    opar <- par(no.readonly = TRUE)
    trellispar <- trellis.par.get()
    ## palette() has no alpha setting; need to apply it to col
    setAlpha <- function(col, alpha) {
        crgb <- col2rgb(col, alpha = TRUE)
        crgb[4] <- alpha * 255
        rgb(crgb[1], crgb[2], crgb[3], crgb[4], max = 255)
    }
    with(trellispar, {
        col <- plot.symbol$col
        alpha <- plot.symbol$alpha
        col <- setAlpha(col, alpha)
        cols <- c(col, superpose.symbol$col[-1])
        palette(cols)
        par(bg = background$col,
            fg = axis.line$col)
        par(pch = plot.symbol$pch,
            lty = plot.line$lty,
            lwd = plot.line$lwd,
            cex = plot.symbol$cex,
            ps = fontsize$text,
            col = axis.line$col,
            col.axis = axis.line$col,
            cex.axis = axis.text$cex,
            col.main = par.main.text$col,
            cex.main = par.main.text$cex,
            col.sub = par.sub.text$col,
            cex.sub = par.sub.text$cex,
            col.lab = par.xlab.text$col,
            cex.lab = par.xlab.text$cex,
            lheight = add.text$lineheight)
        if (!is.null(grid.pars$fontfamily))
            par(family = grid.pars$fontfamily)
    })
    invisible(opar)
}

latticeStyleDemo <-
    function(type = c("plot", "superpose", "polygons", "regions"))
{
    type <- match.arg(type, several.ok = TRUE)
    ## set up keys
    linesKey <- list(c("Main", "2nd", "3rd", "4th", "5th", "6th"),
                     lines = TRUE, points = FALSE, type = "b",
                     columns = 3, between = 1)
    polygonKey <- list(c("Main", "2nd", "3rd", "4th", "5th", "6th"),
                       rectangles = TRUE, points = FALSE,
                       columns = 3, between = 1)
    ## create a list of trellis objects
    objs <- list()
    if ("plot" %in% type) {
        objs$plot.symbol <-
            xyplot(ozone ~ wind^2, environmental,
                   type = c("p", "smooth"),
                   panel = function(x, y, ...) {
                       try(panel.refline(h = 0))
                       panel.xyplot(x, y, ...)
                       lims <- current.panel.limits()
                       panel.abline(h = mean(lims$ylim))
                       panel.bwplot(x, rep(quantile(lims$ylim, 0.9), length(x)),
                                    box.width = diff(lims$ylim) * 0.08)
                       panel.usertext(mean(lims$xlim), mean(lims$ylim),
                                      "This is user.text; \n that is add.line", pos = 3)
                   })
    }
    if ("polygons" %in% type) {
        objs$polygons <- barchart(margin.table(Titanic, c(1,4)),
                                  legend = list(top = list(fun = "drawSimpleKey", args = polygonKey)))
    }
    if ("superpose" %in% type) {
        objs$superpose <-
            xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width, iris,
                   type = c("p", "r"), jitter.x = TRUE, jitter.y = TRUE, factor = 5,
                   legend = list(bottom = list(fun = "drawSimpleKey", args = linesKey)))
    }
    if ("regions" %in% type) {
        objs$regions <- levelplot(volcano[c(TRUE,FALSE), c(TRUE,FALSE)],
                                  scales = list(draw = FALSE))
    }
    ## merge multiple panels into one display
    if (length(objs) == 1)
        obj <- objs[[1]]
    else obj <- do.call("c", objs)
    rm(objs)

    obj <- update(obj, as.table = TRUE, main = "Lattice style demo",
                  xlab = expression(NULL), ylab = "I am a ylab")
    if (length(type) > 1) {
        obj <- update(obj, scales = list(x = list(draw = FALSE)),
                      legend = list(
                      right = if ("regions" %in% type)
                      list(fun = "draw.colorkey", args = list(list(at=0:100))),
                      bottom = if ("superpose" %in% type)
                      list(fun = "drawSimpleKey", args = linesKey),
                      top = if ("polygons" %in% type)
                      list(fun = "drawSimpleKey", args = polygonKey)
                      ))
    }
    obj
}

custom.theme.2 <-
    function(symbol = brewer.pal(n = 9, name = "Set1")[c(2:1, 3:5, 7:9)], ## blue first
             fill = brewer.pal(n = 8, name = "Accent"),
             region = brewer.pal(n = 11, name = "RdBu"),
             reference = "#e8e8e8", bg = "transparent", fg = "black")
{
    custom.theme(symbol = symbol, fill = fill, region = region,
                 reference = reference, bg = bg, fg = fg)
}

custom.theme.black <-
    function(symbol = brewer.pal(n = 8, name = "Set2"),
             fill = brewer.pal(n = 8, name = "Set2"),
             region = rev(brewer.pal(n = 9, name = "YlOrRd")),
             reference = "#444444", bg = "black", fg = "white",
             etc = TRUE)
{
    foo <- custom.theme(symbol = symbol, fill = fill, region = region,
                        reference = reference, bg = bg, fg = fg)
    etcList <- list(add.text = list(col = "#eeeeee"),
                    plot.symbol = list(pch = 16, alpha = 0.5),
                    superpose.symbol = list(pch = 16, alpha = 0.5),
                    plot.line = list(lwd = 2),
                    superpose.line = list(lwd = 2),
                    reference.line = list(lwd = 2),
                    add.line = list(lwd = 2),
                    plot.polygon = list(border = "transparent"),
                    superpose.polygon = list(border = "transparent"),
                    strip.background = list(col = grey(3:8/8)),
                    strip.shingle = list(col = grey(2:7/8)))
    if (etc)
        foo <- modifyList(foo, etcList)
    ## need to reset any existing "user.text" entry (usually black)
    ## this seems to be the only way to do it (needs to be eval'd later!)
    foo$user.text <- expression(NULL)
    foo
}

gedit <- function(..., width = 25) {
    ## hack to resize gedit fields
    ## (broken in gWidgetsRGtk2 0.0-38)
    foo <- gWidgets::gedit(..., width = width)
    if (inherits(guiToolkit(), "guiWidgetsToolkitRGtk2")) {
        foo.gtk <- getToolkitWidget(foo)
        foo.gtk["width-chars"] <- width
    }
    foo
}
