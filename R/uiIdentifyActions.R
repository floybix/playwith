## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

identifyActionGroup <- function(playState)
{
    entries <-
        list( ## : name, stock icon, label, accelerator, tooltip, callback
             list("SetLabelsTo", "gtk-index", "Set _labels to...", "<Ctrl>L", NULL, set.labels_handler),
             list("SetLabelStyle", NULL, "Set label st_yle...", NULL, NULL, set.label.style_handler),
             list("Identify", "gtk-info", "_Identify...", NULL, "Identify all points in a selected region", identify_handler),
             list("IdTable", "gtk-info", "Select from _table...", NULL, "Select points from a table", id.table_handler),
             list("FindLabels", "gtk-find", "_Find...", "<Ctrl>F", "Find points with labels matching...", id.find_handler),
             list("SaveIDs", NULL, "_Save IDs...", NULL, "_Save current IDs to an object", save.ids_handler)
             )

    ## construct action group with playState passed to callbacks
    aGroup <- gtkActionGroupNew("IdentifyActions")
    aGroup$addActions(entries, playState)
    aGroup
}

initIdentifyActions <- function(playState)
{
    playState$tmp$identify.ok <- FALSE
    hasArgs <- playState$accepts.arguments
    isLatt <- playState$is.lattice
    isLatt3D <- isLatt && !is.null(playState$trellis$panel.args.common$scales.3d)
    isBase <- !isLatt && is.null(playState$viewport)
    isBaseMulti <- isBase && any(par("mfrow") > 1)
    ## detect known plots that this will not work with
    ## TODO: check that xyData() has x and y coords
    if (is.null(playState$data.points)) {
        if (!hasArgs) return()
        if (isBaseMulti) return()
        ## lattice package:
        if (isLatt3D) return()
        if (playState$callName %in%
            c("splom", "contourplot",
              "histogram", "densityplot", "barchart")) return()
        if (playState$callName %in% "marginal.plot") return()
        ## what about parallel?
        ## from graphics package:
        if (playState$callName %in%
            c("hist", "barplot", "spineplot", "mosaic",
              "assocplot", "fourfoldplot",
              "coplot", "image", "contour", "persp",
              "pie", "pairs")) return()
    }
    mainCall <- mainCall(playState)
    labels <- playState$.args$labels
    ## try to guess labels if they were not given
    if (is.null(labels)) {
        if (is.null(playState$data.points)) {
            ## try to construct labels from the plot call

            tmp.data <- getDataArg(playState)

            if (length(mainCall > 1)) {
                ## check for named "data" argument
                tmp.data <- callArg(playState, "data")
                if (!is.null(tmp.data))
                    labels <- makeLabels(tmp.data)
                ## hard-coded exceptions...
                if (playState$callName == "qqplot") {
                    tmp.x <- callArg(playState, 1)
                    tmp.y <- callArg(playState, 2)
                    x.lab <- makeLabels(tmp.x, orSeq=T)
                    y.lab <- makeLabels(tmp.y, orSeq=T)
                    labels <- paste(sep="",
                                    x.lab[order(tmp.x)], ",",
                                    y.lab[order(tmp.y)])
                }
                ## otherwise: default handler...
                if (is.null(labels)) {
                    ## look at first argument (tmp.data may be NULL)
                    tmp.x <- callArg(playState, 1, data=tmp.data)
                    if (inherits(tmp.x, "formula")) {
                        ## if 1st arg is formula, 2nd is `data`
                        if (is.null(tmp.data) &&
                            (length(mainCall) >= 3) &&
                            (is.null(names(mainCall)) ||
                             identical(names(mainCall)[[3]], ""))
                            )
                            tmp.data <- callArg(playState, 2)
                        xObj <- if (length(tmp.x) == 2)
                            tmp.x[[2]] else tmp.x[[3]]
                        ## get left-most term in formula
                        while (is.call(xObj) && toString(xObj[[1]]) %in%
                               c("|", "*", "+"))
                            xObj <- xObj[[2]]
                        xObj <- if (!is.null(tmp.data))
                            eval(xObj, tmp.data, playState$env)
                        else eval(xObj, environment(tmp.x), playState$env)
                        labels <- makeLabels(xObj, orSeq=T)
                    } else {
                        if (is.null(row.names(tmp.x)) &&
                            is.list(tmp.x) &&
                            all(c("x","y") %in% names(tmp.x)))
                            tmp.x <- tmp.x$x
                        labels <- makeLabels(tmp.x, orSeq=T)
                    }
                }
            }
        } else {
            ## data.points were supplied
            tmp.x <- playState$data.points
            if (is.null(row.names(tmp.x)) &&
                is.list(tmp.x) &&
                all(c("x","y") %in% names(tmp.x)))
                tmp.x <- tmp.x$x
            labels <- makeLabels(tmp.x, orSeq=T)
        }
    }
    playState$labels <- labels
    playState$tmp$identify.ok <- TRUE
}

updateIdentifyActions <- function(playState)
{
    aGroup <- playState$actionGroups[["IdentifyActions"]]
    ## Identify etc
    canIdent <- playState$tmp$identify.ok
    aGroup$getAction("Identify")$setVisible(canIdent)
    aGroup$getAction("IdTable")$setVisible(canIdent)
    aGroup$getAction("SaveIDs")$setVisible(canIdent)
    ## draw persistent labels
    if (!canIdent) return()
    for (space in names(playState$ids)) {
        idInfo <- playState$ids[[space]]
        drawLabels(playState, which=idInfo$which, space=space,
                   pos=idInfo$pos)
    }
}

makeLabels <- function(x, orSeq=FALSE)
{
    labels <- row.names(x)
    if (is.factor(x) || is.character(x))
        labels <- as.character(x)
    if (inherits(x, "POSIXt") ||
        inherits(x, "Date") ||
        inherits(x, "yearmon") ||
        inherits(x, "yearqtr"))
        labels <- format(x)
    if (inherits(x, "ts") || inherits(x, "zoo"))
        labels <- rep(format(stats::time(x)), NCOL(x))
    if (is.null(labels) && is.numeric(x))
        labels <- names(x)
    if (is.null(labels) && orSeq) labels <- seq_along(x)
    labels
}

drawLabels <- function(playState, which, space="plot", pos=1)
{
    playDevSet(playState)
    data <- xyCoords(playState, space=space)
    if (length(data$x) == 0) return(FALSE)
    if (length(data$y) == 0) return(FALSE)
    ## convert to log scale if necessary
    data <- dataCoordsToSpaceCoords(playState, data)
    x <- data$x[which]
    y <- data$y[which]
    if (playState$is.lattice && !is.null(data$subscripts)) {
        subwhich <- findInterval(which, data$subscripts)
        x <- data$x[subwhich]
        y <- data$y[subwhich]
    }
    labels <- playState$labels[which]
    pos <- rep(pos, length=length(labels))
    offset <- as.numeric(playState$label.offset)
    annots <- expression()
    for (i in seq_along(labels)) {
        annots[[i]] <- call("panel.text", x[i], y[i],
                            labels[i], pos = pos[i])
        if (offset != 0.5)
            annots[[i]]$offset <- offset
    }
    playDo(playState, eval(annots), space=space,
           clip.off=identical(playState$clip.annotations, FALSE))
}

set.labels_handler <- function(widget, playState)
{
    box <- ggroup(horizontal = FALSE)
    datArg <- getDataArg(playState, eval = FALSE)
    dat <- try(eval(datArg, playState$env))
    labcode <- NULL
    labdesc <- NULL
    if (!is.null(dat)) {
        datOpts <- c(deparseOneLine(call("rownames", datArg)),
                     colnames(dat))
        labcode <- datOpts
        labdesc <- datOpts
    }
    labcode <- c(labcode,
                 "NULL",
                 "xyData(playDevCur())$x",
                 "xyData(playDevCur())$y",
                 'with(xyData(playDevCur()), paste(y, x, sep="@"))')
    labdesc <- c(labdesc,
                 "data subscripts",
                 "data x values",
                 "data y values",
                 "data (y@x) values")
    labradio <- gradio(labdesc, selected = 0, container = box,
                       handler = function(h, ...) {
                           idx <- max(1, svalue(labradio, index=TRUE))
                           svalue(labedit) <- labcode[idx]
                       })
    labedit <- gedit(labcode[1], container = box)
    ## show dialog
    gbasicdialog("Set labels to...", widget = box,
                 handler = function(h, ...) {
                     tmp <- tryCatch(
                             eval(parse(text=svalue(labedit)), dat, playState$env),
                                     error=function(e)e)
                     ## check whether there was an error
                     if (inherits(tmp, "error")) {
                         gmessage.error(conditionMessage(tmp))
                     } else {
                         playState$labels <- tmp
                     }
                 })
    ## TODO: refresh?
}

## TODO: get rid of this -- set label style now means changing lattice settings
set.label.style_handler <- function(widget, playState)
{
    style <- playState$label.style
    if (is.null(playState$label.style)) {
        style <- do.call(gpar, trellis.par.get("add.text"))
    }
    if (inherits(style, "gpar"))
        style <- as.call(c(quote(gpar), style))

    callTxt <- deparseOneLine(style)

    repeat {
        newTxt <- NA
        editbox <- gedit(callTxt, width=120)
        gbasicdialog("Edit label style", widget=editbox, action=environment(),
                     handler=function(h, ...) {
                         h$action$newTxt <- svalue(editbox)
                     })
        if (is.na(newTxt)) break
        if (newTxt == "") break
        if (identical(newTxt, callTxt)) break
        callTxt <- newTxt
        tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
        ## check whether there was a syntax error
        if (inherits(tmp, "error")) {
            gmessage.error(conditionMessage(tmp))
        } else {
            playState$label.style <- eval(tmp)
            break
        }
    }
    playState$win$present()
}

identify_handler <- function(widget, playState)
{
    foo <- playSelectData(playState,
                          "Click or drag to identify points. Right-click to cancel.")
    if (is.null(foo)) return()
    if (length(foo$which) == 0) return()
    if (!is.null(foo$subscripts))
        foo$which <- foo$subscripts
    with(foo, {
        if (!is.click) pos <- 1
        ## store newly identified points in playState
        ids.new <- data.frame(which=which, pos=pos)
        ids.old <- playState$ids[[space]] ## may be NULL
        if (is.null(ids.old)) ids.old <- ids.new
        else ids.new <- rbind(ids.old, ids.new)
        playState$ids[[space]] <- ids.new
        ## draw them
        drawLabels(playState, which=which, space=space, pos=pos)
    })
    ## update other tool states
    updateAnnotationActionStates(playState)
}

identifyCore <- function(playState, foo, deidentify = FALSE)
{
    if (!isTRUE(playState$tmp$identify.ok)) return()
    if (is.null(playState$labels)) return()
    space <- foo$space
    data <- xyCoords(playState, space=foo$space)
    ## convert to log scale if necessary
    data <- dataCoordsToSpaceCoords(playState, data)
    if (length(data$x) == 0) return(FALSE)
    if (length(data$y) == 0) return(FALSE)
    coords <- foo$coords

    x <- coords$x[1]
    y <- coords$y[1]
    ppxy <- playDo(playState,
                   list(lx=convertX(unit(x, "native"), "points", TRUE),
                        ly=convertY(unit(y, "native"), "points", TRUE),
                        px=convertX(unit(data$x, "native"), "points", TRUE),
                        py=convertY(unit(data$y, "native"), "points", TRUE)),
                   space=foo$space)
    pdists <- with(ppxy, sqrt((px - lx)^2 + (py - ly)^2))
    ## all data points within 11 points
    which <- which(pdists < 11)
    if (length(which) == 0) return()
    ## order by distance from click
    which <- which[order(pdists[which])]
    idMenu <- gtkMenu()
    for (w in which) {
        datx <- data$x[[w]]
        daty <- data$y[[w]]
        pos <- with(ppxy, lattice:::getTextPosition(x = lx - px[w],
                                                    y = ly - py[w]))
        ss <- data$subscripts[[w]]
        if (is.null(ss)) ss <- w
        label <- toString(playState$labels[[ss]])
        label <- paste(label, " (x: ", format(signif(datx, 4)),
                       ", y: ", format(signif(daty, 4)), ")", sep="")
        item <- gtkMenuItem(label)
        idMenu$append(item)
        idInfo <- list(which=ss, pos=pos)
        gSignalConnect(item, "activate",
                       function(widget, user.data) {
                           which <- user.data$which
                           pos <- user.data$pos
                           ## store newly identified points in playState
                           ids.new <- data.frame(which=which, pos=pos)
                           ids.old <- playState$ids[[space]] ## may be NULL
                           if (is.null(ids.old)) ids.old <- ids.new
                           else ids.new <- rbind(ids.old, ids.new)
                           playState$ids[[space]] <- ids.new
                           ## draw them
                           drawLabels(playState, which=which, space=space, pos=pos)
                           ## update other tool states
                           updateAnnotationActionStates(playState)
                       }, data=idInfo)
    }
    ## show the menu
    idMenu$popup(button=0, activate.time=gtkGetCurrentEventTime())
    while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
}


id.table_handler <- function(widget, playState) {
}

id.find_handler <- function(widget, playState) {
}

save.ids_handler <- function(widget, playState) {

}
