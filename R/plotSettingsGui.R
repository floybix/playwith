## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

plot.settings_handler <- function(widget, playState)
{
    if (playState$is.lattice) {
        latticeSettingsGUI(playState)
    } else {
        basePlotSettingsGUI(playState)
    }
}

latticeSettingsGUI <- function(playState)
{
    dialog <- gwindow(title="Plot settings")
    wingroup <- ggroup(horizontal=FALSE, container = dialog)
    tabs <- gnotebook(container = wingroup)
    wid <- list()

    origCall <- playState$call
    trell <- playState$trellis
    isLatt3D <- !is.null(trell$panel.args.common$scales.3d)
    isMulti <- (prod(dim(trell)) > 1)
    scaleNames <- c("x", "y")
    if (isLatt3D) scaleNames <- c("x", "y", "z")
    relations <- c("same", "free", "sliced")
    aspects <- c('"fill"', '"iso"', '"xy"', '0.5', '1', '2')
    indexConds <- c("function(x, y) median(x, na.rm=TRUE)",
                    "function(x, y) median(y, na.rm=TRUE)",
                    "function(x, y) mean(x, na.rm=TRUE)",
                    "function(x, y) mean(y, na.rm=TRUE)",
                    "function(x, y) mean(y - x, na.rm=TRUE)",
                    "function(x, y) sum(complete.cases(x))",
                    "function(x, y) sum(complete.cases(x, y))")

    setArg <- function(h, ...) {
        target <- parse(text=h$action)[[1]]
        val <- svalue(h$obj)
        ## NAs possible from coerce.with = as.numeric
        if ((length(val) == 1) && is.na(val))
            val <- NULL
        callArg(playState, target) <- val
    }

    parseArg <- function(txt) {
        val <- parse(text = txt)
        if (length(val) == 0) return(NULL)
        val[[1]]
    }

    setArgTitle <- function(h, ...) {
        nm <- h$action
        val <- svalue(wid[[nm]])
        if (val == "") val <- NULL
        isExpr <- svalue(wid[[paste(nm, "expr", sep=".")]])
        if (!is.null(val) && isExpr)
            val <- parse(text=val, srcfile=NULL)
        ## insert explicit NULL, do not just omit the argument
        ## (to over-ride default labels)
        if (is.null(val)) val <- expression(NULL)
        callArg(playState, nm) <- val
    }

    setArgLayout <- function(h, ...) {
        val <- c(svalue(wid$layout.cols),
                 svalue(wid$layout.rows))
        if (any(is.na(val))) val <- NULL
        callArg(playState, "layout") <- val
    }

    setArgTck <- function(h, ...) {
        w <- h$action
        target <- paste("scales", w, "tck", sep="$")
        target <- parse(text=target)[[1]]
        val <- svalue(wid[[w]]$tck)
        if (is.na(val)) val <- NULL
        if (!is.null(val) && !svalue(wid[[w]]$ticks.opp))
            val[2] <- 0
        callArg(playState, target) <- val
    }

    ## (NEW TAB)
    basicsTab <- ggroup(horizontal=FALSE, container = tabs,
                        label = "Basic settings")

    ## TITLES
    labgroup <- gframe("Titles", horizontal=FALSE, container=basicsTab)
    lay <- glayout(spacing = 1, container=labgroup)
    rownum <- 1
    isComplexTitle <- list()
    titleNames <- c("main", "sub", "xlab", "ylab")
    if (isLatt3D) titleNames <- c(titleNames, "zlab")
    for (nm in titleNames) {
        isComplexTitle[[nm]] <- FALSE
        ## lattice titles can be: grob / list / vector
        argVal <- trell[[nm]]
        if (inherits(argVal, "grob")) {
            isComplexTitle[[nm]] <- TRUE
            argVal <- NULL
        }
        if (is.list(argVal)) {
            isComplexTitle[[nm]] <- TRUE
            argValMatch <- match.call(function(label, ...) NA,
                                      as.call(c(quote(foo), argVal)))
            argVal <- argValMatch$label
        }
        if (is.character(argVal) && (length(argVal) > 1)) {
            argVal <- as.expression(argVal)
        }
        isExpr <- is.language(argVal)
        if (isExpr) {
            argVal <- as.expression(argVal)
            ## multiple expressions in this format are parsed correctly
            argVal <- paste(sapply(argVal, deparseOneLine), collapse="; ")
        }
        nm.expr <- paste(nm, "expr", sep=".")
        lay[rownum, 1] <- nm
        lay[rownum, 2] <- wid[[nm]] <-
            gedit(toString(argVal), width=60, container = lay,
                  handler = setArgTitle, action = nm)
        lay[rownum, 3] <- wid[[nm.expr]] <-
            gcheckbox("plotmath", checked=isExpr, container = lay,
                      handler = setArgTitle, action = nm)
        if (isComplexTitle[[nm]]) {
            enabled(wid[[nm]]) <- FALSE
            enabled(wid[[nm.expr]]) <- FALSE
        }
        rownum <- rownum + 1
    }
    visible(lay) <- TRUE

    if (isLatt3D) {
        tmp <- ggroup(horizontal = TRUE, container = basicsTab)
        glabel("Axis labels offset distance: ", container = tmp)
        val <- trell$panel.args.common$scales.3d$x.scales$distance
        wid$scales.distance <-
            gedit(toString(val), width = 4, container = tmp,
                  handler = setArg, coerce.with = as.numeric,
                  action = "scales$distance")
    }

    ## STRIPS
    stripgroup <- gframe("Strips", horizontal=FALSE, container=basicsTab)
    tmp <- ggroup(horizontal=TRUE, container = stripgroup)
    wid$strip <- gcheckbox("Show top strips", container = tmp,
                           checked = !identical(trell$strip, FALSE),
                           handler = setArg, action = "strip")
    wid$strip.left <- gcheckbox("Show left strips", container = tmp,
                           checked = !identical(trell$strip.left, FALSE),
                           handler = setArg, action = "strip.left")
    ## TODO: show names / show levels
    ## TODO: edit strip text... (only for one conditioning variable)
    ## strip.custom(factor.levels=c(...), strip.names=FALSE, strip.levels=TRUE)
    wid$strip.abbrev <- gcheckbox("Abbreviate text", container = stripgroup,
                           checked = isTRUE(trell$par.strip.text$abbreviate),
                           handler = setArg, action = "par.strip.text$abbreviate")

    ## LAYOUT
    if (isMulti) {
        layoutgroup <- gframe("Layout", horizontal=FALSE, container=basicsTab)
        tmp <- ggroup(horizontal=TRUE, container = layoutgroup)
        val <- if (!is.null(trell$layout)) trell$layout else c(1, 1, 1)
        glabel("Layout per page: ", container = tmp)
        wid$layout.cols <- gspinbutton(from = 0, to = 16, value = val[1],
                                       handler = setArgLayout, container = tmp)
        glabel("columns, ", container = tmp)
        wid$layout.rows <- gspinbutton(from = 0, to = 16, value = val[2],
                                       handler = setArgLayout, container = tmp)
        glabel("rows. ", container = tmp)
        wid$as.table <-
            gcheckbox("as table", container = tmp,
                  checked = trell$as.table,
                  handler = setArg, action = "as.table")
        ## index.cond
        tmp <- ggroup(horizontal=TRUE, container = layoutgroup)
        glabel("Order panels by (index.cond): ", container = tmp)
        wid$index.cond <-
            gdroplist(indexConds, container = tmp, selected = 0,
                      editable = TRUE, coerce.with = parseArg,
                      handler = setArg, action = "index.cond")
        val <- callArg(playState, "index.cond", eval = FALSE)
        if (!is.null(val))
            svalue(wid$index.cond) <- deparseOneLine(val)
    }


    ## (NEW TAB)
    scalesTab <- ggroup(horizontal=FALSE, container = tabs,
                        label="Scales")

    ## ASPECT
    aspectgroup <- gframe("Aspect ratio", horizontal = FALSE, container = scalesTab)
    tmp <- ggroup(horizontal=TRUE, container = aspectgroup)
    glabel("Panel aspect ratio y/x: ", container = tmp)
    val <- callArg(playState, "aspect")
    if (is.null(val)) {
        val <- round(trell$aspect.ratio, 3)
        if (trell$aspect.fill) val <- "fill"
    }
    wid$aspect <- gdroplist(aspects, container = tmp,
                            editable = TRUE,
                            handler = setArg, coerce.with = parseArg,
                            action = if (isLatt3D) "panel.aspect" else "aspect")
    svalue(wid$aspect) <- deparse(val)
    ## TODO: aspect 3D?

    ## SCALES
    lay <- glayout(spacing = 1, container = scalesTab)
    col <- 2
    init <- TRUE
    for (w in scaleNames) {
        wid[[w]] <- list()
        if (isLatt3D) {
            w.scales <- trell$panel.args.common$scales.3d
            w.scales <- w.scales[[paste(w, "scales", sep=".")]]
        } else {
            w.scales <- trell[[paste(w, "scales", sep=".")]]
        }
        row <- 1
        lay[1, col] <- glabel(paste("<b>", w, "-axis</b>", sep=""),
                              markup = TRUE, container = lay)
        if (isMulti) {
            row <- row + 1
            if (init) lay[row, 1] <- "relation:"
            lay[row, col] <- wid[[w]]$relation <-
                gdroplist(relations, container = lay,
                          selected = which(w.scales$relation == relations),
                          handler = setArg,
                          action = paste("scales", w, "relation", sep="$"))
        }
        row <- row + 1
        lay[row, col] <- wid[[w]]$draw <-
            gcheckbox("draw axis", container = lay,
                      checked = w.scales$draw,
                      handler = setArg,
                      action = paste("scales", w, "draw", sep="$"))
        if (isLatt3D) {
            row <- row + 1
            lay[row, col] <- wid[[w]]$arrows <-
                gcheckbox("arrows only", container = lay,
                          checked = w.scales$arrows,
                          handler = setArg,
                          action = paste("scales", w, "arrows", sep="$"))
        }
        row <- row + 1
        lay[row, col] <- wid[[w]]$log <-
            gcheckbox("logarithmic", container = lay,
                      checked = !identical(w.scales$log, FALSE),
                      handler = setArg,
                      action = paste("scales", w, "log", sep="$"))
        row <- row + 1
        lay[row, col] <- wid[[w]]$axs <-
            gcheckbox("padding", container = lay,
                      checked = (w.scales$axs == "r"),
                      handler = function(h, ...) {
                          target <- parse(text=h$action)[[1]]
                          val <- if (svalue(h$obj)) "r" else "i"
                          callArg(playState, target) <- val
                      },
                      action = paste("scales", w, "axs", sep="$"))
        row <- row + 1
        if (init) lay[row, 1] <- "~num. ticks:"
        lay[row, col] <- wid[[w]]$tick.number <-
            gedit(toString(w.scales$tick.number), width = 4, container = lay,
                  handler = setArg, coerce.with = as.numeric,
                  action = paste("scales", w, "tick.number", sep="$"))
        row <- row + 1
        if (init) lay[row, 1] <- "tick length:"
        lay[row, col] <- wid[[w]]$tck <-
            gedit(toString(w.scales$tck[1]), width = 4, container = lay,
                  handler = setArgTck, coerce.with = as.numeric,
                  action = w)
        if (!isLatt3D) {
            row <- row + 1
            lay[row, col] <- wid[[w]]$ticks.opp <-
                gcheckbox("on opp. side", container = lay,
                          checked = (w.scales$tck[2] != 0),
                          handler = setArgTck,
                          action = w)
        }
        row <- row + 1
        if (init) lay[row, 1] <- "label side:"
        alternList <- list(none = 0, standard = 1, opposite = 2,
                           alternating = c(1, 2), both = 3)
        whichAltern <- which(sapply(alternList, identical, w.scales$alternating))
        if (length(whichAltern) == 0) whichAltern <- 0
        lay[row, col] <- wid[[w]]$alternating <-
            gdroplist(names(alternList), container = lay,
                      selected = whichAltern,
                      handler = setArg,
                      coerce.with = function(nm) alternList[[nm]],
                      action = paste("scales", w, "alternating", sep="$"))
        row <- row + 1
        if (init) lay[row, 1] <- "rotation:"
        lay[row, col] <- wid[[w]]$rot <-
            gedit(as.numeric(w.scales$rot[1]), width = 4, container = lay,
                  handler = setArg, coerce.with = as.numeric,
                  action = paste("scales", w, "rot", sep="$"))
        row <- row + 1
        lay[row, col] <- wid[[w]]$abbreviate <-
            gcheckbox("abbreviate labels", container = lay,
                      checked = w.scales$abbreviate,
                      handler = setArg,
                      action = paste("scales", w, "abbreviate", sep="$"))
        ## TODO: specify labels...
        ## TODO: custom axis components?

        ## next column
        col <- col + 1
        init <- FALSE
    }
    visible(lay) <- TRUE

    svalue(tabs) <- 1

    ok_handler <- function(h, ...)
    {
        playReplot(playState)
        if (h$action == "apply") return()
        dispose(h$obj)
        playState$win$present()
    }

    buttgroup <- ggroup(container=wingroup)
    addSpring(buttgroup)
    okbutt <- gbutton("OK", handler=ok_handler,
                      action="ok", container=buttgroup)
    prebutt <- gbutton("Apply", handler=ok_handler,
                       action="apply", container=buttgroup)
    canbutt <- gbutton("Cancel", container=buttgroup,
                       handler = function(h, ...) {
                           playState$call <- origCall
                           dispose(h$obj)
                       })
    size(okbutt) <- size(prebutt) <- size(canbutt) <- c(80, 30)
}


basePlotSettingsGUI <- function(playState)
{
    dialog <- gwindow(title="Plot settings")
    wingroup <- ggroup(horizontal=FALSE, container = dialog)
    tabs <- gnotebook(container=wingroup)
    wid <- list()
    origCall <- playState$call

    setArg <- function(h, ...) {
        target <- parse(text=h$action)[[1]]
        val <- svalue(h$obj)
        if (is.na(val)) val <- NULL
        callArg(playState, target) <- val
    }

    setArgTitle <- function(h, ...) {
        nm <- h$action
        val <- svalue(wid[[nm]])
        if (val == "") val <- NULL
        isExpr <- svalue(wid[[paste(nm, "expr", sep=".")]])
        if (!is.null(val) && isExpr)
            val <- parse(text=val, srcfile=NULL)
        ## insert explicit NULL, do not just omit the argument
        ## (to over-ride default labels)
        if (is.null(val)) val <- expression(NULL)
        callArg(playState, nm) <- val
    }

    setMar <- function(h, ...) {
        val <- c(svalue(wid$mar.bottom),
                 svalue(wid$mar.left),
                 svalue(wid$mar.top),
                 svalue(wid$mar.right))
        par(mar = val)
    }

    setType <- function(h, ...) {
        val <- NULL
        if (svalue(wid$points)) {
            val <- "p"
            if (svalue(wid$lines)) val <- "b"
        } else {
            if (svalue(wid$lines)) val <- "l"
            if (svalue(wid$droplines)) val <- "h"
        }
        callArg(playState, "type") <- val
    }

    setLog <- function(h, ...) {
        val <- paste(c(if (svalue(wid$x$log)) "x",
                       if (svalue(wid$y$log)) "y"),
                     collapse="")
        callArg(playState, "log") <- val
    }

    setLab <- function(h, ...) {
        val <- c(svalue(wid$x$tick.number),
                 svalue(wid$y$tick.number),
                 par("lab")[3])
        callArg(playState, "lab") <- val
    }

    ## convenience extractor
    arg <- function(x) callArg(playState, x)

    ## (NEW TAB)
    basicsTab <- ggroup(horizontal=FALSE, container = tabs,
                        label = "Basic settings")

    ## TITLES
    labgroup <- gframe("Titles", horizontal=FALSE, container=basicsTab)
    lay <- glayout(spacing = 1, container=labgroup)
    rownum <- 1
    titleNames <- c("main", "sub", "xlab", "ylab")
    for (nm in titleNames) {
        ## (base graphics etc) just a named argument
        argVal <- callArg(playState, nm)
        isExpr <- is.language(argVal)
        if (isExpr) {
            argVal <- as.expression(argVal)
            ## multiple expressions in this format are parsed correctly
            argVal <- paste(sapply(argVal, deparseOneLine), collapse="; ")
        }
        nm.expr <- paste(nm, "expr", sep=".")
        lay[rownum, 1] <- nm
        lay[rownum, 2] <- wid[[nm]] <-
            gedit(toString(argVal), width=60, container = lay,
                  handler = setArgTitle, action = nm)
        lay[rownum, 3] <- wid[[nm.expr]] <-
            gcheckbox("plotmath", checked=isExpr, container = lay,
                      handler = setArgTitle, action = nm)
        rownum <- rownum + 1
    }
    visible(lay) <- TRUE

    ## MARGINS
    marginsgroup <- gframe("Margins", horizontal=FALSE, container=basicsTab)
    lay <- glayout(spacing = 1, container=marginsgroup)
    mar <- par("mar")
    lay[2,1] <- "Margins (lines): "
    lay[1,2] <- "bottom"
    lay[2,2] <- wid$mar.bottom <-
            gedit(toString(mar[1]), width = 4, container = lay,
                  handler = setMar, coerce.with = as.numeric)
    lay[1,3] <- "left"
    lay[2,3] <- wid$mar.left <-
            gedit(toString(mar[2]), width = 4, container = lay,
                  handler = setMar, coerce.with = as.numeric)
    lay[1,4] <- "top"
    lay[2,4] <- wid$mar.top <-
            gedit(toString(mar[3]), width = 4, container = lay,
                  handler = setMar, coerce.with = as.numeric)
    lay[1,5] <- "right"
    lay[2,5] <- wid$mar.right <-
            gedit(toString(mar[4]), width = 4, container = lay,
                  handler = setMar, coerce.with = as.numeric)
    visible(lay) <- TRUE

    ## TYPE
    typegroup <- gframe("Plot type", horizontal=FALSE, container=basicsTab)
    arg_type <- callArg(playState, "type")
    hasPoints <- (is.null(arg_type) || any(c("p","b","o") %in% arg_type))
    hasLines <- any(c("l","b","o") %in% arg_type)
    hasDrops <- any("h" %in% arg_type)
    tmp <- ggroup(horizontal = TRUE, container=typegroup)
    wid$points <- gcheckbox("Points", checked=hasPoints, container = tmp,
                            handler = setType)
    wid$lines <- gcheckbox("Lines", checked=hasLines, container = tmp,
                           handler = setType)
    wid$droplines <- gcheckbox("Drop lines", checked=hasDrops, container = tmp,
                               handler = setType)


    ## (NEW TAB)
    scalesTab <- ggroup(horizontal=FALSE, container = tabs,
                        label="Scales")

    ## ASPECT
    aspectgroup <- gframe("Aspect ratio", horizontal = FALSE, container = scalesTab)
    tmp <- ggroup(horizontal=FALSE, container = aspectgroup)
    wid$asp1 <- gcheckbox("Isometric scales", container = aspectgroup,
                          checked = isTRUE(arg("asp") == 1),
                          handler = function(h, ...) {
                              val <- if (svalue(h$obj)) 1 else NULL
                              callArg(playState, "asp") <- val
                          })
    wid$ptys <- gcheckbox("Square plot region", container = aspectgroup,
                          checked = (par("pty") == "s"),
                          handler = function(h, ...) {
                              val <- if (svalue(h$obj)) "s" else "m"
                              par(pty = val)
                          })

    ## SCALES
    lay <- glayout(spacing = 1, container = scalesTab)
    col <- 2
    init <- TRUE
    for (w in c("x", "y")) {
        wid[[w]] <- list()
        row <- 1
        lay[1, col] <- glabel(paste("<b>", w, "-axis</b>", sep=""),
                              markup = TRUE, container = lay)
        row <- row + 1
        w.axt <- paste(w, "axt", sep="")
        lay[row, col] <- wid[[w]]$draw <-
            gcheckbox("draw axis", container = lay,
                      checked = !(any(arg("axes") == FALSE) ||
                                  any(arg(w.axt) == "n")),
                      handler = function(h, ...) {
                          val <- if (svalue(h$obj)) NULL else "n"
                          callArg(playState, h$action) <- val
                      }, action = w.axt)
        row <- row + 1
        lay[row, col] <- wid[[w]]$log <-
            gcheckbox("logarithmic", container = lay,
                      checked = par(paste(w, "log", sep="")),
                      handler = setLog)
        row <- row + 1
        w.axs <- paste(w, "axs", sep="")
        lay[row, col] <- wid[[w]]$axs <-
            gcheckbox("padding", container = lay,
                      checked = !any(arg(w.axs) == "i"),
                      handler = function(h, ...) {
                          val <- if (svalue(h$obj)) NULL else "i"
                          callArg(playState, h$action) <- val
                      }, action = w.axs)
        row <- row + 1
        if (init) lay[row, 1] <- "~num. ticks:"
        lay[row, col] <- wid[[w]]$tick.number <-
            gedit(toString(par("lab")[1]), width = 4, container = lay,
                  handler = setLab, coerce.with = as.numeric)
        row <- row + 1
        if (init) lay[row, 1] <- "tick length:"
        lay[row, col] <- wid[[w]]$tck <-
            gedit(toString(par("tcl")), width = 4, container = lay,
                  handler = setArg, coerce.with = as.numeric,
                  action = "tcl")
        ## next column
        col <- col + 1
        init <- FALSE
    }
    visible(lay) <- TRUE

    svalue(tabs) <- 1

    ok_handler <- function(h, ...)
    {
        playReplot(playState)
        if (h$action == "apply") return()
        dispose(h$obj)
        playState$win$present()
    }

    buttgroup <- ggroup(container=wingroup)
    addSpring(buttgroup)
    okbutt <- gbutton("OK", handler=ok_handler,
                      action="ok", container=buttgroup)
    prebutt <- gbutton("Apply", handler=ok_handler,
                       action="apply", container=buttgroup)
    canbutt <- gbutton("Cancel", container=buttgroup,
                       handler=function(h, ...) {
                           playState$call <- origCall
                           dispose(h$obj)
                       })
    size(okbutt) <- size(prebutt) <- size(canbutt) <- c(80, 30)
}



## TODO
addReferenceLinesGUI <- function(playState)
{
    ## (NEW TAB)
    decoTab <- ggroup(horizontal=FALSE)
    add(tabs, decoTab, label="Reference lines")

    ## REFERENCE LINES
    decogroup <- gframe("Reference lines", horizontal=FALSE, container=decoTab)
    enabled(decogroup) <- FALSE
    ## grid
    gridgroup <- ggroup(container=decogroup)
    wid$grid_h <- gcheckbox("horizontal", checked=F)
    wid$grid_v <- gcheckbox("vertical", checked=F)
    add(gridgroup, glabel("Grid:"))
    add(gridgroup, wid$grid_h)
    add(gridgroup, wid$grid_v)
    ## reference lines
    linegroup <- ggroup(container=decogroup)
    wid$abline_h <- gcheckbox("horizontal", checked=F)
    wid$abline_v <- gcheckbox("vertical", checked=F)
    wid$abline_d <- gcheckbox("diagonal", checked=F)
    add(linegroup, glabel("Line through origin:"))
    add(linegroup, wid$abline_h)
    add(linegroup, wid$abline_v)
    add(linegroup, wid$abline_d)
    ## rug
    ruggroup <- ggroup(container=decogroup)
    wid$rug_bot <- gcheckbox("bottom", checked=F)
    wid$rug_top <- gcheckbox("top", checked=F)
    wid$rug_left <- gcheckbox("left", checked=F)
    wid$rug_right <- gcheckbox("right", checked=F)
    add(ruggroup, glabel("Rug (marginal distribution):"))
    add(ruggroup, wid$rug_bot)
    add(ruggroup, wid$rug_top)
    add(ruggroup, wid$rug_left)
    add(ruggroup, wid$rug_right)
    ## stats: min / max / median / quartiles / mean
    statgroup <- ggroup(container=decogroup)
    wid$stat_min <- gcheckbox("min", checked=F)
    wid$stat_max <- gcheckbox("max", checked=F)
    wid$stat_median <- gcheckbox("median", checked=F)
    wid$stat_quart <- gcheckbox("quart", checked=F)
    wid$stat_mean <- gcheckbox("mean", checked=F)
    wid$stat_yaxis <- gcheckbox("y axis", checked=F)
    wid$stat_xaxis <- gcheckbox("x axis", checked=F)
    add(statgroup, glabel("Stats:"))
    add(statgroup, wid$stat_min)
    add(statgroup, wid$stat_max)
    add(statgroup, wid$stat_median)
    add(statgroup, wid$stat_quart)
    add(statgroup, wid$stat_mean)
    statgroup2 <- ggroup(container=decogroup)
    addSpring(statgroup2)
    add(statgroup2, glabel("on"))
    add(statgroup2, wid$stat_yaxis)
    add(statgroup2, wid$stat_xaxis)
    ## loess
    loessgroup <- ggroup(container=decogroup)
    wid$loess <- gcheckbox("Loess smoother", checked=F)
    wid$loess_span <- gedit("0.75", width=5, coerce.with=as.numeric)
    add(loessgroup, wid$loess)
    add(loessgroup, glabel("span:"))
    add(loessgroup, wid$loess_span)
    ## panel.lmline()


    ## lattice
    ## panel.grid()
    ## panel.rug()
    ## panel.abline()
    ## panel.lmline()
    ## panel.loess()

    ## plot.default (panel.first)
    ## grid()
    ## rug()
    ## abline()
    ## lines(lm())

    ## grid / panel.grid()
    ## rug
    ## axis lines abline(h=0), abline(v=0)
    ## regression line panel.lmline / type="r"
    ## smooth panel.smooth

}












makeLayersMenuButton <- function()
{
    name <- StateEnv$.current
    layersButton <- gtkMenuToolButton(gtkImageNewFromStock("gtk-index",
                                                           size=GtkIconSize['small-toolbar']), label="Layers...")
    thisTips <- gtkTooltips()
    thisTips$setTip(layersButton, "Toggle visible layers")
    itemNames <- NULL
    itemIDs <- NULL
    itemStates <- NULL
    the.call <- StateEnv[[name]]$call
    layerType <- NA
    if ('layers' %in% names(the.call)) {
        layerType <- "layers"
        ## store evaluated list in call
        StateEnv[[name]]$call$layers <- eval(the.call$layers, StateEnv[[name]]$env)
        the.call <- StateEnv[[name]]$call
        for (i in seq_along(the.call$layers)) {
            itemName <- names(the.call$layers)[i]
            if (is.null(itemName) || (nchar(itemName) == 0)) {
                if (is.expression(the.call$layers[[i]])) {
                    itemName <- deparse(the.call$layers[[i]][[1]])[1]
                } else itemName <- deparse(the.call$layers[[i]])[1]
            }
            itemNames[i] <- itemName
            itemIDs[i] <- i
            itemStates[i] <- !any(grep("\\.off$", itemName))
        }
    } else if ('sp.layout' %in% names(the.call)) {
        layerType <- "sp.layout"
        for (i in seq_along(the.call$sp.layout)) {
            itemName <- names(the.call$sp.layout)[i]
            if (is.null(itemName) || (nchar(itemName) == 0)) {
                itemName <- deparse(the.call$sp.layout[[i]])[1]
                ## TODO: make lists pretty
            }
            itemNames[i] <- itemName
            itemIDs[i] <- i
            itemStates[i] <- !identical(the.call$sp.layout[[i]]$which, 0)
        }
    } else if ('panel' %in% names(the.call)) {
        layerType <- "panel"
        ## skip if this is not an inline function
        if (is.symbol(the.call$panel)) return(NA)
        panelBody <- body(eval(the.call$panel, StateEnv[[name]]$env))
        ## treat any call to panel.* or grid.* or sp.* as a layer
        r.grep.call <- function(x, pattern="^panel\\.|^grid\\.|^sp\\.", indexPath=NULL) {
            stopifnot(is.call(x))
            if (any(grep(pattern, deparse(x[[1]])[1]))) {
                return(bquote(c(.(indexPath))))
            }
            unlist(lapply(seq_along(x)[-1], function(i)
                          if (is.call(x[[i]]))
                          r.grep.call(x[[i]], pattern=pattern,
                                      indexPath=c(indexPath, i))))
        }
        itemIDs <- r.grep.call(panelBody)
        itemIDs <- lapply(itemIDs, eval) ## convert from `call` to vector
        itemIDsStr <- sapply(itemIDs, toIndexStr)
        itemNames <- sapply(paste('panelBody',itemIDsStr,sep=''),
                            function(s) deparse(eval(parse(text=s)))[1] )
        ## work out whether each item is turned off with `if (FALSE)`
        itemStates <- rep(T, length(itemIDs))
        drop_last <- function(xx) lapply(xx, function(x)
                                         if (length(x) > 1) x[-length(x)] else x)
        itemParentIDsStr <- sapply(drop_last(itemIDs), toIndexStr)
        itemStates <- sapply(paste('panelBody',itemParentIDsStr,sep=''),
                             function(s) {
                                 parentExpr <- eval(parse(text=s))
                                 !(identical(parentExpr[[1]], as.symbol("if")) &&
                                   identical(parentExpr[[2]], FALSE))
                             })
        itemIDs[itemStates==F] <- drop_last(itemIDs[itemStates==F])
        ## tmp <- quote({ print(x); while (x) { print(panel.list('a','b')); x <- something(grid.lines()) } })
    }
    itemNames <- sapply(itemNames, toString, width=34)
    ## omit the button if only one layer
    if (length(itemIDs) <= 1) return(NA)
    ## store layers info
    StateEnv[[name]]$layers.names <- itemNames
    StateEnv[[name]]$layers.ids <- itemIDs
    ## make button menu
    layersMenu <- gtkMenu()
    for (i in seq_along(itemIDs)) {
        menuItem <- gtkCheckMenuItem(itemNames[i]) #gtkMenuItem(itemName)
        menuItem['active'] <- itemStates[i]
        layersMenu$append(menuItem)
        gSignalConnect(menuItem, "activate", .plotAndPlay_layers_event,
                       data=list(index=i, ID=itemIDs[[i]], layerType=layerType))
    }
    layersButton$setMenu(layersMenu)
    ## set main button handler
    gSignalConnect(layersButton, "clicked", .plotAndPlay_layers_event,
                   data=list(menu=layersMenu, layerType=layerType))
    layersButton
}


.plotAndPlay_layers_event <- function(widget, user.data=NULL)
{
    name <- StateEnv$.current
    itemIdx <- user.data$index
    itemID <- user.data$ID
    layerType <- user.data$layerType

    if (is.null(itemID)) {
        menuItems <- user.data$menu$getChildren()
        n <- length(StateEnv[[name]]$layers.names)
        items <- paste(1:n, StateEnv[[name]]$layers.names)
        states <- sapply(menuItems, function(x) x['active'])
        newItems <- select.list(items, preselect=items[states],
                                multiple=T, title="Select layers")
        StateEnv[[name]]$win$present()
        newStates <- (items %in% newItems)
        if (all(states == newStates)) return()
        if (all(newStates == FALSE)) return() ## might be 'cancel'
        StateEnv[[name]]$skip.updates <- T
        for (i in seq_along(newStates)) {
            menuItems[[i]]['active'] <- newStates[i]
        }
        StateEnv[[name]]$skip.updates <- F
        #plotAndPlayUpdate()
        return()
    }
    ## a single menu item toggled
    isActive <- widget['active']
    if (layerType == "layers") {
        layerName <- names(StateEnv[[name]]$call$layers)[itemIdx]
        if (is.null(layerName)) layerName <- ""
        if (isActive) {
            layerName <- sub("\\.off$", "", layerName)
            names(StateEnv[[name]]$call$layers)[itemIdx] <- layerName
        } else {
            layerName <- paste(layerName, ".off", sep="")
            names(StateEnv[[name]]$call$layers)[itemIdx] <- layerName
        }
    } else if (layerType == "sp.layout") {
        ## TODO: store existing `which`
        if (isActive) {
            StateEnv[[name]]$call$sp.layout[[itemIdx]]$which <- NULL
        } else {
            StateEnv[[name]]$call$sp.layout[[itemIdx]]$which <- 0
        }
    } else if (layerType == "panel") {
        ## make sure panel function in call has been evaluated
        StateEnv[[name]]$call$panel <- eval(StateEnv[[name]]$call$panel,
                                            StateEnv[[name]]$env)
        target <- paste('body(StateEnv[[name]]$call$panel)',toIndexStr(itemID),sep='')
        if (isActive) {
            cmd <- paste(target, " <- ", target, "[[3]]", sep='')
        } else {
            cmd <- paste(target, "<- call('if', FALSE,", target, ")")
        }
        eval(parse(text=cmd))
    }
    #plotAndPlayUpdate()
}

toIndexStr <- function(x) paste('[[', x ,']]', sep='', collapse='')


