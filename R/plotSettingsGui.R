## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

settings_handler <- function(widget, playState)
{
    dialog <- gwindow(title="Plot settings")
    loadingLabel <- glabel("Loading...")
    add(dialog, loadingLabel)
    wingroup <- ggroup(horizontal=FALSE)
    tabs <- gnotebook(container=wingroup)
    wid <- list()

    ## convenience extractor
    isLatt <- playState$is.lattice
    x.scales <- if (isLatt) playState$trellis$x.scales
    y.scales <- if (isLatt) playState$trellis$y.scales
    arg <- function(x) callArg(playState, x)

    ## TODO: LEGEND / KEY

    ## (NEW TAB)
    annTab <- ggroup(horizontal=FALSE)
    add(tabs, annTab, label="Title and Axes")

    ## TITLES
    labgroup <- gframe("Titles", horizontal=FALSE, container=annTab)
    lay <- glayout(container=labgroup)
    rownum <- 1
    for (nm in c("main", "sub", "xlab", "ylab")) {
        argVal <- if (playState$is.lattice) playState$trellis[[nm]]
        else callArg(playState, nm)
        isExpr <- is.language(argVal)
        if (isExpr) argVal <- deparse(as.expression(argVal)[[1]])
        wid[[nm]] <- gedit(toString(argVal), width=60)
        nm.expr <- paste(nm, "expr", sep=".")
        wid[[nm.expr]] <- gcheckbox("plotmath", checked=isExpr)
        lay[rownum, 1] <- nm
        lay[rownum, 2] <- wid[[nm]]
        lay[rownum, 3] <- wid[[nm.expr]]
        rownum <- rownum + 1
    }
    visible(lay) <- TRUE


    ## AXES
    axisgroup <- gframe("Axes", horizontal=FALSE, container=annTab)
    lay <- glayout(container=axisgroup)
    wid$xaxis.show <-
        gcheckbox("visible",
                  checked=if (isLatt) x.scales$draw else
                  !(any(arg("axes") == FALSE) ||
                    any(arg("xaxt") == "n")))
    wid$yaxis.show <-
        gcheckbox("visible",
                  checked=if (isLatt) y.scales$draw else
                  !(any(arg("axes") == FALSE) ||
                    any(arg("yaxt") == "n")))
    wid$xaxis.log <-
        gcheckbox("logarithmic",
                  checked=if (isLatt) !identical(x.scales$log, FALSE) else
                  par("xlog")) #any(grep("x", arg("log"))))
    wid$yaxis.log <-
        gcheckbox("logarithmic",
                  checked=if (isLatt) !identical(y.scales$log, FALSE) else
                  par("ylog")) #any(grep("y", arg("log"))))

    ## TODO: lattice scale options (same / sliced / free)
    ## TODO: lattice aspect options
    wid$aspect.iso <-
        gcheckbox("isometric scale", checked=(any(arg("aspect") == "iso") ||
                                              any(arg("asp") == 1)
                                              ))
    lay[1,1] <- "x-axis:"
    lay[1,2] <- wid$xaxis.show
    lay[1,3] <- wid$xaxis.log
    lay[2,1] <- "y-axis:"
    lay[2,2] <- wid$yaxis.show
    lay[2,3] <- wid$yaxis.log
    visible(lay) <- TRUE
    add(axisgroup, wid$aspect.iso)

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

    ## REFERENCE LINE STYLE
    reflinestylegroup <- gframe("Style of reference lines", horizontal=FALSE, container=decoTab)
    enabled(reflinestylegroup) <- FALSE
    ## col / lty / lwd
    colList <- c(palette(), trellis.par.get("superpose.symbol")$col)
    wid$refline_col <- gdroplist(colList, selected=0, editable=TRUE)
    wid$refline_alpha <- gspinbutton(value=1, from=0, to=1, by=0.05, digits=2)
    ## lty
    ltyList <- c("solid", "dashed", "dotted", "dotdash", "longdash")
    wid$refline_lty <- gdroplist(ltyList, selected=0)
    ## lwd
    wid$refline_lwd <- gedit("1", width=5, coerce.with=as.numeric)
    lay <- glayout(container=reflinestylegroup)
    lay[1,1] <- "Color:"
    lay[1,2] <- wid$refline_col
    lay[2,1] <- "Alpha (opacity):"
    lay[2,2] <- wid$refline_alpha
    lay[3,1] <- "Line type:"
    lay[3,2] <- wid$refline_lty
    lay[4,1] <- "Line width:"
    lay[4,2] <- wid$refline_lwd
    visible(lay) <- TRUE

    ## (NEW TAB)
    styleTab <- ggroup(horizontal=FALSE)
    add(tabs, styleTab, label="Style")

    ## STYLE
    stylegroup <- gframe("Plot style", horizontal=FALSE, container=styleTab)
    enabled(stylegroup) <- FALSE
    ## type
    arg_type <- callArg(playState, "type")
    hasPoints <- (is.null(arg_type) || any(c("p","b","o") %in% arg_type))
    hasLines <- any(c("l","b","o") %in% arg_type)
    hasDroplines <- any("h" %in% arg_type)
    wid$points <- gcheckbox("Points", checked=hasPoints)
    wid$lines <- gcheckbox("Lines", checked=hasLines)
    wid$droplines <- gcheckbox("Drop lines", checked=hasDroplines)
    typegroup <- ggroup(container=stylegroup)
    add(typegroup, wid$points)
    add(typegroup, wid$lines)
    add(typegroup, wid$droplines)
    lay <- glayout(container=stylegroup)
    ## cex
    wid$cex <- gedit(toString(callArg(playState, "cex")), width=5,
                     coerce.with=as.numeric)
    lay[1,1] <- "Expansion factor:"
    lay[1,2] <- wid$cex
    ## pch
    pchList <-
        list(`open circle`=21,
             `open diamond`=23,
             `open square`=22,
             `open triangle`=24,
             `solid circle`=19,
             `solid diamond`=18,
             `solid square`=15,
             `solid triangle`=17,
             `plus (+)`=3,
             `cross (x)`=4,
             `dot (.)`="."
             )
    which_pch <- which(sapply(pchList, identical,
                              callArg(playState, "pch")))
    if (length(which_pch) == 0) which_pch <- 0
    wid$pch <- gdroplist(names(pchList), selected=which_pch)
    lay[2,1] <- "Plot symbol:"
    lay[2,2] <- wid$pch
    ## col
    colList <- c(palette(), trellis.par.get("superpose.symbol")$col)
    arg_col <- callArg(playState, "col")
    which_col <- if (is.numeric(arg_col)) which(arg_col == seq_along(colList))
    else which(sapply(colList, identical, arg_col))
    if (length(which_col) == 0) which_col <- 0
    wid$col <- gdroplist(colList, selected=which_col, editable=TRUE)
    lay[3,1] <- "Color (foreground):"
    lay[3,2] <- wid$col
    ## lty
    ltyList <- c("solid", "dashed", "dotted", "dotdash", "longdash")
    arg_lty <- callArg(playState, "lty")
    which_lty <- if (is.numeric(arg_lty)) which(arg_lty == seq_along(ltyList))
    else which(sapply(ltyList, identical, arg_lty))
    if (length(which_lty) == 0) which_lty <- 0
    wid$lty <- gdroplist(ltyList, selected=which_lty, editable=TRUE)
    lay[4,1] <- "Line type:"
    lay[4,2] <- wid$lty
    ## lwd
    wid$lwd <- gedit(toString(callArg(playState, "lwd")), width=5,
                     coerce.with=as.numeric)
    lay[5,1] <- "Line width:"
    lay[5,2] <- wid$lwd
    ## fontface / fontfamily / font
    familyList <- list("sans", "serif", "mono")
    fontList <- list(plain=1, bold=2, italic=3, bolditalic=4)
    visible(lay) <- TRUE

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

    ## OTHER

    ## superpose

    ## layers

    svalue(tabs) <- 1

    settings_handler <- function(h, ...)
    {
        ## note: playState is accessed from the function environment!

        ## TITLES
        argExpr <- function(wid, expr.wid) {
            newVal <- svalue(wid)
            if (newVal == "") return(NULL)
            if (svalue(expr.wid))
                newVal <- parse(text=newVal, srcfile=NULL)
            newVal
        }
        callArg(playState, "main") <- argExpr(wid$main, wid$main.expr)
        callArg(playState, "sub") <- argExpr(wid$sub, wid$sub.expr)
        callArg(playState, "xlab") <- argExpr(wid$xlab, wid$xlab.expr)
        callArg(playState, "ylab") <- argExpr(wid$ylab, wid$ylab.expr)

        ## AXES
        if (playState$is.lattice) {
            newXdraw <- if (svalue(wid$xaxis.show)) NULL else FALSE
            newYdraw <- if (svalue(wid$yaxis.show)) NULL else FALSE
            callArg(playState, quote(scales$x$draw)) <- newXdraw
            callArg(playState, quote(scales$y$draw)) <- newYdraw
            newXlog <- if (svalue(wid$xaxis.log)) TRUE else NULL
            newYlog <- if (svalue(wid$yaxis.log)) TRUE else NULL
            callArg(playState, quote(scales$x$log)) <- newXlog
            callArg(playState, quote(scales$y$log)) <- newYlog
            newAspect <- if (svalue(wid$aspect.iso)) "iso" else NULL
            callArg(playState, "aspect") <- newAspect
        } else {
            ## base graphics plot
            newXaxt <- if (svalue(wid$xaxis.show)) NULL else "n"
            newYaxt <- if (svalue(wid$yaxis.show)) NULL else "n"
            callArg(playState, "xaxt") <- newXaxt
            callArg(playState, "yaxt") <- newYaxt
            newLog <- paste(c(if (svalue(wid$xaxis.log)) "x",
                              if (svalue(wid$yaxis.log)) "y"),
                            collapse="")
            if (newLog == "") newLog <- NULL
            callArg(playState, "log") <- newLog
            newAsp <- if (svalue(wid$aspect.iso)) 1 else NULL
            callArg(playState, "asp") <- newAsp
        }

        ## REFERENCE LINES
        wid$grid_h
        wid$grid_v
        wid$abline_h
        wid$abline_v
        wid$abline_d
        wid$rug_bot
        wid$rug_top
        wid$rug_left
        wid$rug_right
        wid$stat_min
        wid$stat_max
        wid$stat_median
        wid$stat_quart
        wid$stat_mean
        wid$stat_yaxis
        wid$stat_xaxis
        wid$loess
        wid$loess_span

        ## REFERENCE LINE STYLE
        wid$refline_col
        wid$refline_alpha
        wid$refline_lty
        wid$refline_lwd

        ## STYLE
        ## lattice: use callArg(playState, "par.settings") <-
        ## list(superpose.symbol=list(pch=c(22, 23),cex=c(1.7,1.6),col="black"))
        wid$points
        wid$lines
        wid$droplines

        wid$cex
        wid$pch
        wid$col
        wid$alpha
        wid$lty
        wid$lwd

        playReplot(playState)

        if (h$action == "apply") return()
        dispose(h$obj)
        playState$win$present()
    }

    buttgroup <- ggroup(container=wingroup)
    addSpring(buttgroup)
    okbutt <- gbutton("OK", handler=settings_handler,
                      action="ok", container=buttgroup)
    prebutt <- gbutton("Apply", handler=settings_handler,
                       action="apply", container=buttgroup)
    canbutt <- gbutton("Close", handler=function(h, ...) dispose(h$obj),
                       container=buttgroup)
    size(okbutt) <- size(prebutt) <- size(canbutt) <- c(80, 30)

    delete(dialog, loadingLabel)
    add(dialog, wingroup)
    return()
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


