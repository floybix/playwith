## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
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


playwith <-
    function(expr,
             new = playwith.getOption("new"),
             title = NULL,
             labels = NULL,
             data.points = NULL,
             viewport = NULL,
             parameters = list(),
             tools = list(),
             update.actions = list(),
             init.actions = list(),
             ...,
             width = playwith.getOption("width"),
             height = playwith.getOption("height"),
             pointsize = playwith.getOption("pointsize"),
             eval.args = playwith.getOption("eval.args"),
             on.close = playwith.getOption("on.close"),
             modal = FALSE,
             playState = if (!new) playDevCur(),
             plot.call,
             main.function)
{
    if (!missing(plot.call) && !missing(expr))
        stop("give only one of 'expr' and 'plot.call'")
    if (missing(plot.call) && missing(expr)) {
        ## basic device mode
        expr <- quote({})
        if (missing(title))
            title <- "playwith (basic device mode)"
        show.call <- FALSE
    }
    if (missing(plot.call)) plot.call <- substitute(expr)
    if (is.expression(plot.call)) {
        plot.call <- if (length(plot.call) > 1)
            as.call(c(as.symbol("{"), plot.call)) else plot.call[[1]]
    }
    if (missing(main.function)) main.function <- NULL
    if (is.character(main.function))
        main.function <- get(main.function)
    ## check types
    if (!is.call(plot.call)) stop("'expr' / 'plot.call' should be a call")
    if (!is.null(title) && !is.character(title)) stop("'title' should be character")
    if (!is.null(viewport) && !is.list(viewport)) viewport <- list(plot=viewport)
    ## playState is the <environment> encapsulating the plot, window and device
    cleanupStateEnv()
    if (!is.null(playState)) {
        stopifnot(inherits(playState, "playState"))
        if (is.null(title)) title <- playState$title
    }
    if (is.null(playState) || isTRUE(playState$keep)) {
        playState <- new.env(parent = emptyenv())
        class(playState) <- c("playState", "environment")
        ID <- basename(tempfile())
        StateEnv[[ID]] <- playState
    }
    playState$plot.ready <- FALSE
    StateEnv$.current <- playState
    ## env is the <environment> containing local cached objects
    env <- new.env(parent = globalenv())
    ## work out evaluation rules
    envir <- parent.frame() ## where to look for variables
    invert.match <- FALSE
    if (is.list(eval.args)) {
        if (!is.null(eval.args$envir)) envir <- eval.args$envir
        if (!is.null(eval.args$invert)) invert.match <- eval.args$invert
        eval.args <- eval.args[[1]]
    }
    evalGlobals <- !is.na(eval.args)
    if (is.na(eval.args))
        eval.args <- (environmentName(envir) != "R_GlobalEnv")
    if (!identical(eval.args, FALSE)) {
        try(copyLocalArgs(plot.call, envir=envir, newEnv=env,
                          evalGlobals=evalGlobals, pattern=eval.args,
                          invert.match=invert.match))
    }
    ## check whether the window already exists
    myWin <- playState$win
    if (!is.null(myWin) && inherits(myWin, "GtkWindow")) {
        daSize <- playState$widgets$drawingArea$getAllocation()
        if (missing(width)) width <- daSize$width / 96
        if (missing(height)) height <- daSize$height / 96
        ## remove everything
        playState$devoff <- TRUE ## to avoid trigger close
        myWin$getChild()$destroy()
        myWin$present()
        myWin$resize(width * 96, height * 96)
    } else {
        ## create a new window
        myWin <- gtkWindow(show=FALSE)
        if (!inherits(myWin, "GtkWindow"))
            stop(paste("Could not create the GTK window.",
                       "Make sure you have recent versions of",
                       "RGtk2 and the GTK+ libraries.",
                       "See http://www.ggobi.org/rgtk2/"))
        ## set approx window size; NOTE: device size is adjusted below
        myWin["default-width"] <- width * 96
        myWin["default-height"] <- height * 96
        myWin["modal"] <- modal
        myWin$show()
        ## switch to GTK event loop while the window is in focus (for tooltips)
        myWin$addEvents(GdkEventMask["focus-change-mask"])
        gSignalConnect(myWin, "focus-in-event", gtkmain_handler,
                       data=playState)
        gSignalConnect(myWin, "focus-out-event", gtkmainquit_handler,
                       data=playState)
        gSignalConnect(myWin, "delete-event", gtkmainquit_handler,
                       data=playState)
        ## run user-defined close action
        gSignalConnect(myWin, "delete-event",  window.close_handler,
                       data=playState)
    }
    if (!is.null(title)) myWin["title"] <- title
    myVBox <- gtkVBox()
    myWin$add(myVBox)
    playState$win <- myWin
    uiManager <- constructUIManager(playState)
    actionGroups <- uiManager$getActionGroups()
    names(actionGroups) <- sapply(actionGroups, gtkActionGroupGetName)
    ## construct menus
    menubar <- uiManager$getWidget("/MenuBar")
    menubar$show() ## location, layout behavior, padding
    myVBox$packStart(menubar, expand=FALSE)
    ## construct the call toolbar
    callToolbar <- uiManager$getWidget("/CallToolbar")
    callToolbar$setTooltips(TRUE) #playwith.getOption("show.tooltips"))
    callToolbar["toolbar-style"] <- GtkToolbarStyle["icons"]
    callToolbar["show-arrow"] <- FALSE
    ## merge in the address bar
    callEntry <- gtkComboBoxEntryNewText()
    callEntry$show()
    ## "changed" emitted on typing and selection
    gSignalConnect(callEntry, "changed",
                   function(widget, playState)
                     if (widget["active"] > -1)
                       edit.call.inline_handler(widget$getChild(), playState),
                   data=playState)
    gSignalConnect(callEntry$getChild(), "activate",
                   edit.call.inline_handler, data=playState)
    item <- gtkToolItem()
    item$add(callEntry)
    item$setExpand(TRUE)
    callToolbar$insert(item, -1)
    callEditButton <- gtkButton(label="Edit call...")
    gSignalConnect(callEditButton, "clicked",
                   edit.call_handler, data=playState)
    item <- gtkToolItem()
    item$add(callEditButton)
    callToolbar$insert(item, -1)
    callToolbar$show()
    tbStyle <- GtkToolbarStyle[playwith.getOption("toolbar.style")]
    ## create the top toolbar
    topToolbar <- uiManager$getWidget("/TopToolbar")
    topToolbar$setTooltips(TRUE)
    topToolbar["toolbar-style"] <- tbStyle
    ## create the bottom toolbar
    bottomToolbar <- uiManager$getWidget("/BottomToolbar")
    bottomToolbar$setTooltips(TRUE)
    bottomToolbar["toolbar-style"] <- tbStyle
    ## create the left toolbar
    leftToolbar <- uiManager$getWidget("/LeftToolbar")
    leftToolbar$setTooltips(TRUE)
    leftToolbar["toolbar-style"] <- tbStyle
    leftToolbar["orientation"] <- GtkOrientation["vertical"]
    ## create the right toolbar
    rightToolbar <- uiManager$getWidget("/RightToolbar")
    rightToolbar$setTooltips(TRUE)
    rightToolbar["toolbar-style"] <- tbStyle
    rightToolbar["orientation"] <- GtkOrientation["vertical"]
    ## create the statusbar and coords readout
    statusbarBox <- gtkHBox()
    coordsLabel <- gtkLabel()
    coordsLabel["single-line-mode"] <- TRUE
    coordsLabel["selectable"] <- TRUE
    statusbarBox$packStart(coordsLabel, expand=FALSE)
    statusbar <- gtkStatusbar()
    statusbarBox$packStart(statusbar)
    statusbarBox["visible"] <- isTRUE(playwith.getOption("show.statusbar"))
    ## place toolbars in the window layout
    myVBox$packStart(callToolbar, expand=FALSE)
    myVBox$packStart(topToolbar, expand=FALSE)
    myHBox <- gtkHBox()
    myVBox$packStart(myHBox)
    myHBox$packStart(leftToolbar, expand=FALSE)
    myHBox$packEnd(rightToolbar, expand=FALSE)
    myVBox$packEnd(statusbarBox, expand=FALSE)
    myVBox$packEnd(bottomToolbar, expand=FALSE)
    #statusbar["has-resize-grip"] <- TRUE

    ## create the plot area
    myDA <- gtkDrawingArea()
    myDA$addEvents(GdkEventMask["enter-notify-mask"]
                   + GdkEventMask["button-press-mask"]
                   + GdkEventMask["button-release-mask"]
                   + GdkEventMask["exposure-mask"])
    myHBox$packStart(myDA)
    ## note, this constraint is removed below
    myDA$setSizeRequest(width * 96, height * 96)
    asCairoDevice(myDA, pointsize=pointsize)
    ## need to regenerate coord spaces after resize
    gSignalConnect(myDA, "configure-event", configure_handler,
                   data=playState)
    gSignalConnect(myDA, "enter-notify-event", auto.reconfig_handler,
                   data=playState)
    gSignalConnect(myHBox, "remove", devoff_handler,
                   data=playState, after=TRUE)
    ## initialise trellis settings for the device
    trellis.device(new=FALSE)
    ## create the page scrollbar
    pageScrollBox <- gtkVBox(show=FALSE)
    myHBox$packStart(pageScrollBox, expand=FALSE)
    pageScrollBox$packStart(gtkLabel("Page"), expand=FALSE)
    pageEntry <- gtkEntry()
    pageEntry["width-chars"] <- 2
    gSignalConnect(pageEntry, "activate",
                   function(widget, playState) {
                       if (!playState$plot.ready) return()
                       newPage <- round(as.numeric(widget["text"]))
                       if (newPage == playState$page) return()
                       playState$page <- newPage
                       playReplot(playState)
                   },
                   data=playState)
    pageScrollBox$packStart(pageEntry, expand=FALSE)
    pageScrollbar <- gtkVScrollbar()
    pageScrollbar["adjustment"] <- gtkAdjustment(value=1, lower=1, upper=1+1,
                                                 step.incr=1, page.incr=1, page.size=1)
    pageScrollbar["update-policy"] <- GtkUpdateType["delayed"]
    gSignalConnect(pageScrollbar, "value-changed",
                   function(widget, playState) {
                       if (!playState$plot.ready) return()
                       newPage <- round(widget$getValue())
                       if (newPage == playState$page) return()
                       playState$page <- newPage
                       playReplot(playState)
                   },
                   data=playState)
    pageScrollBox$packStart(pageScrollbar)
    ## create the time/index scrollbar
    timeScrollBox <- gtkHBox(show=FALSE)
    myVBox$packStart(timeScrollBox, expand=FALSE)
    timeScrollBox$packStart(gtkLabel("Time"), expand=FALSE)
    timeEntry <- gtkEntry()
    timeEntry["width-chars"] <- 30
    gSignalConnect(timeEntry, "activate",
                   time.mode_entry_handler, data=playState)
    timeScrollBox$packStart(timeEntry, expand=FALSE)
    timeScrollbar <- gtkHScrollbar()
    timeScrollbar["adjustment"] <- gtkAdjustment()
    timeScrollbar["update-policy"] <- GtkUpdateType["delayed"]
    gSignalConnect(timeScrollbar, "value-changed",
                   time.mode_scrollbar_handler, data=playState)
    timeScrollBox$packStart(timeScrollbar)
    ## add dummy toolbar buttons to force plot device to approx size
#    initTbar <- function(tbar, horiz) {
#        tbar$show()
#        tbar$insert(quickTool(playState, label="Loading...",
#                              icon="gtk-execute"), -1)
#        ## try to force resize
#        gdkWindowProcessAllUpdates()
#        while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
#        if (horiz) tbar$setSizeRequest(-1, tbar$getAllocation()$height)
#        else tbar$setSizeRequest(tbar$getAllocation()$width, -1)
#    }
#    if (length(top.tools)) initTbar(topToolbar, horiz=TRUE)
#    if (length(bottom.tools)) initTbar(bottomToolbar, horiz=TRUE)
#    if (length(left.tools)) initTbar(leftToolbar, horiz=FALSE)
#    if (length(right.tools)) initTbar(rightToolbar, horiz=FALSE)
    myHBox["resize-mode"] <- GtkResizeMode["queue"] ## does nothing?
    ## after resize, remove minimum size constraint from device
    myDA$setSizeRequest(-1, -1)
    ## store the state of this plot window in a new environment
    ## set defaults -- these can be replaced by explicit arguments
    playState$page <- 1
    playState$pages <- 1
    playState$is.lattice <- FALSE
    playState$is.ggplot <- FALSE
    playState$show.tooltips <- playwith.getOption("show.tooltips")
    playState$show.toolbars <- playwith.getOption("show.toolbars")
    playState$show.calltoolbar <- playwith.getOption("show.calltoolbar")
    playState$show.menubar <- playwith.getOption("show.menubar")
    playState$show.statusbar <- playwith.getOption("show.statusbar")
    playState$annotation.mode <- playwith.getOption("annotation.mode")
    playState$clip.annotations <- playwith.getOption("clip.annotations")
    playState$label.style <- playwith.getOption("label.style")
    playState$label.offset <- playwith.getOption("label.offset")
    playState$arrow.style <- playwith.getOption("arrow.style")
    playState$arrow.arrow <- playwith.getOption("arrow.arrow")
    ## store extra arguments (...) in the state object (playState)
    dots <- list(...)
    for (arg in names(dots)) {
        if (arg == "") next
        playState[[arg]] <- dots[[arg]]
    }
    time.mode <- dots$time.mode
    missing_time.mode <- is.null(time.mode)
    if (is.null(time.mode)) time.mode <- FALSE
    if (!is.null(playState$time.vector)) {
        if (missing_time.mode) time.mode <- TRUE
        ## set current state variables
        env$cur.index <-
            if (!is.null(playState$cur.index)) {
                playState$cur.index
            } else if (!is.null(playState$cur.time)) {
                max(1, findInterval(playState$cur.time,
                                    playState$time.vector))
            } else 1
        env$cur.time <- playState$time.vector[env$cur.index]
        env$time.vector <- playState$time.vector
    }
    ## set initial values of any parameters
    for (i in seq_along(parameters)) {
        parname <- names(parameters)[i]
        parval <- parameters[[i]]
        assign(parname, parval[1], envir=env)
    }
    ## construct the state object (playState)
    playState$win <- myWin
    playState$dev <- dev.cur()
    playState$call <- plot.call
    playState$env <- env
    playState$tmp <- list()
    playState$time.mode <- time.mode
    playState$labels <- labels
    playState$data.points <- data.points
    playState$viewport <- viewport
    playState$parameters <- parameters
    playState$tools <- tools
    playState$update.actions <- update.actions
    playState$init.actions <- init.actions
    ## TODO: store ids etc in an environment for linking
    playState$ids <- list()
    playState$brushed <- list()
    playState$annotations <- list()
    playState$uiManager <- uiManager
    playState$actionGroups <- actionGroups
    playState$widgets <-
        list(drawingArea = myDA,
             topToolbar = topToolbar,
             leftToolbar = leftToolbar,
             bottomToolbar = bottomToolbar,
             rightToolbar = rightToolbar,
             callToolbar = callToolbar,
             callEntry = callEntry,
             pageEntry = pageEntry,
             pageScrollbar = pageScrollbar,
             pageScrollBox = pageScrollBox,
             timeEntry = timeEntry,
             timeScrollbar = timeScrollbar,
             timeScrollBox = timeScrollBox,
             coordsLabel = coordsLabel,
             statusbar = statusbar,
             statusbarBox = statusbarBox,
             vbox = myVBox,
             hbox = myHBox)
    playState$on.close <- on.close
    playState$.args <-
        list(missing_time.mode = missing_time.mode,
             data.points = data.points,
             labels = labels,
             title = title,
             main.function = main.function)
    ## do the plot
    playNewPlot(playState)
    invisible(playState)
}

playNewPlot <- function(playState)
{
    playDevSet(playState)
    ## clear the current plot if any, to avoid lengthy redraws
    playState$plot.ready <- FALSE
    on.exit(playState$plot.ready <- TRUE)
    grid.newpage()
    playPrompt(playState, NULL)
    ## disable toolbars until this is over
    playFreezeGUI(playState)
    on.exit(playThawGUI(playState))
    ## hide scrollbars if they are not needed
    if (playState$time.mode == FALSE) {
        hideWidgetNoRedraw(playState, playState$widgets$timeScrollBox,
                           horiz=TRUE)
    }
    if (playState$pages == 1) {
        hideWidgetNoRedraw(playState, playState$widgets$pageScrollBox,
                           horiz=FALSE)
    }
    ## find which component of the call takes arguments (xlim etc)
    ## (also put main call into canonical form, with match.call)
    updateMainCall(playState)
    ## update address bar with current call
    updateAddressBar(playState)
    ## eval plot call
    ## (NOTE this will draw the plot UNLESS it is lattice or ggplot)
    if (isTRUE(playwith.getOption("catch.errors"))) {
      result <- tryCatch(eval(playState$call, playState$env),
                         error = function(e)e)
      if (inherits(result, "error")) {
        callText <- deparseOneLine(playState$call)
        msg <- paste(conditionMessage(result),
                     "\n in: \n",
                     callText, sep="")
        gmessage.error(msg)
        return(result)
      }
    } else {
      result <- eval(playState$call, playState$env)
    }
    ## detect lattice
    playState$is.lattice <- (inherits(result, "trellis"))
    if (playState$is.lattice) playState$trellis <- result
    ## detect ggplot
    playState$is.ggplot <- (inherits(result, "ggplot"))
    if (playState$is.ggplot) playState$ggplot <- result

    ## lattice needs subscripts argument to correctly identify points.
    ## warn, and just show the within-panel indices unless subscripts=T
    if (playState$is.lattice &&
        playState$accepts.arguments &&
        prod(dim(playState$trellis)) > 1)
    {
        if (is.null(playState$trellis$panel.args[[1]]$subscripts)) {
                gmessage("Call may need subscripts=TRUE to correctly identify points.",
                         title="Warning", icon="warning")
        }
    }

    ## initialise tools and add them to the toolbars
    ## note that these may be user-defined constructor functions
    ## if a constructor evaluates to NA it is skipped
    populateToolbar <- function(toolbar, tools)
    {
        for (i in seq_along(tools)) {
            myName <- names(tools)[i]
            if (!any(nchar(myName))) myName <- NA
            toolFun <- eval(tools[[i]])
            if (is.character(toolFun)) {
                myName <- toolFun
                toolFun <- toolConstructors[[myName]]
            }
            if (is.na(myName)) myName <-
                paste(deparse(substitute(tools)), i, sep="_")
            if (!is.function(toolFun)) {
                warning("constructor for ", myName, " is not a function")
                next
            }
            ## call the tool constructor
            newTool <- try(toolFun(playState))
            if (inherits(newTool, "try-error") || is.null(newTool)) {
                warning("constructor for ", myName, " failed")
                next
            }
            if (identical(newTool, NA)) next
            result <- try(toolbar$insert(newTool, -1))
            if (inherits(result, "try-error")) next
            playState$tools[[myName]] <- newTool
        }
    }
    ## set up toolbar tools
    playState$tools <- list()
    tbars <- playState$widgets[c("topToolbar", "leftToolbar",
                                 "bottomToolbar", "rightToolbar")]
    doToolbars <- quote({
        for (tbar in tbars) {
            for (x in rev(tbar$getChildren())) x$destroy()
        }
        with(playState$widgets, {
            populateToolbar(topToolbar, playState$.args$top.tools)
            populateToolbar(leftToolbar, playState$.args$left.tools)
            populateToolbar(bottomToolbar, playState$.args$bottom.tools)
            populateToolbar(rightToolbar, playState$.args$right.tools)
        })
        paramToolbar <- playState$widgets[[
            paste(playwith.getOption("parameters.toolbar"), "Toolbar", sep="") ]]
        horiz <- playwith.getOption("parameters.toolbar") %in% c("bottom","top")
        params <- playState$parameters
        for (i in seq_along(params)) {
            parname <- names(params)[i]
            parval <- params[[i]]
            newTool <- try(parameterControlTool(playState, name=parname,
                                                value=parval, horizontal=horiz))
            if (inherits(newTool, "try-error")) next
            if (i == 1) populateToolbar(paramToolbar, list("~~"))
            paramToolbar$insert(newTool, -1)
            populateToolbar(paramToolbar, list("~~"))
        }
        for (tbar in tbars) {
            if (length(tbar$getChildren())) tbar$show()
            else tbar$hide()
        }
    })
 #   if (playState$is.lattice || playState$is.ggplot) eval(doToolbars)
 #   else blockRedraws(eval(doToolbars), playState)

    ## initialise tools for a new plot (see ui.R)
    initActions(playState)
    ## hide empty toolbars (TODO: this can not actually change with a new plot...)
    blockRedraws({
        for (nm in paste("/", c("Top", "Left", "Bottom", "Right"),
                         "Toolbar", sep="")) {
            tbar <- playState$uiManager$getWidget(nm)
            if (length(tbar$getChildren())) tbar$show()
            else tbar$hide()
        }
    })
    ## try to force redraw
    gdkWindowProcessAllUpdates()
    while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
    ## continue
    playPostPlot(result, playState)
}

playReplot <- function(playState)
{
    if (isTRUE(playState$skip.redraws)) return()
    playState$plot.ready <- FALSE
    on.exit(playState$plot.ready <- TRUE)
    playDevSet(playState)
    grid.newpage()
    playPrompt(playState, NULL)
    ## disable toolbars until this is over
    playFreezeGUI(playState)
    on.exit(playThawGUI(playState))
    ## hide scrollbars if they are not needed
    if (playState$time.mode == FALSE) {
        hideWidgetNoRedraw(playState, playState$widgets$timeScrollBox,
                           horiz=TRUE)
    }
    if (playState$pages == 1) {
        hideWidgetNoRedraw(playState, playState$widgets$pageScrollBox,
                           horiz=FALSE)
    }
    ## update address bar with current call
    updateAddressBar(playState)
    ## eval plot call
    ## (NOTE this will draw the plot UNLESS it is lattice or ggplot)
    result <- eval(playState$call, playState$env)
    ## continue
    playPostPlot(result, playState)
}

playPostPlot <- function(result, playState)
{
    ## set back to this device, since may have switched during plot
    playDevSet(playState)
    if (inherits(result, "trellis")) {
        ## work out panels and pages
        nPackets <- prod(dim(result))
        nPanels <- nPackets
        nPages <- 1
        if (!is.null(myLayout <- result$layout)) {
            nPanels <- myLayout[1] * myLayout[2]
            if (myLayout[1] == 0) nPanels <- myLayout[2]
            nPages <- ceiling(nPackets / nPanels)
            result$layout[3] <- 1
        }
        if (playState$page > nPages) playState$page <- 1
        playState$pages <- nPages
        ## plot trellis object (specified page only)
        plot(result, packet.panel=packet.panel.page(playState$page))
        playState$trellis <- result
    }
    if (inherits(result, "ggplot")) {
        ## plot ggplot object
        if (packageDescription("grid")$Version < package_version("2.7"))
            print(result, pretty=FALSE) ## there is a bug in grid < 2.7
        else print(result)
        playState$ggplot <- result
        ## typically want: playState$viewport <- list(plot="panel_1_1")
        vpNames <- grid.ls(viewports=TRUE, grobs=FALSE, print=FALSE)$name
        panelNames <- vpNames[grep("panel", vpNames)]
        panelNames <- unique(panelNames)
        tmp.vp <- as.list(panelNames)
        names(tmp.vp) <- panelNames
        names(tmp.vp)[1] <- "plot"
        playState$viewport <- tmp.vp
    }
    ## store coordinate system(s)
    generateSpaces(playState)
    playState$plot.ready <- TRUE
    ## update toolitem and menuitem states
    updateActions(playState)
    ## and update the pages scrollbar
    pages_post.plot.action(playState$widgets$pageScrollBox,
                           playState=playState)


    ## run update actions on buttons
if (FALSE) {
    blockRedraws({
        for (x in playState$tools) {
            playDevSet(playState)
            xUpd <- attr(x, "post.plot.action")
            if (!is.null(xUpd)) {
                if (!is.function(xUpd)) {
                    warning("post.plot.action not a function")
                    next
                }
                xUpd(x, playState=playState)
            }
        }
        ## the pages scrollbar
        pages_post.plot.action(playState$widgets$pageScrollBox,
                               playState=playState)
    })
}
    invisible(result)
}

updateAddressBar <- function(playState)
{
    ## add current call to "address bar" and set up associated tools
    callTxt <- ""
    if (object.size(playState$call) < 50000) {
        widg <- playState$widgets
        callTxt <- deparseOneLine(playState$call, control=
                                  playwith.getOption("deparse.options"))
        if (is.null(playState$.args$title)) playState$win["title"] <-
            toString(callTxt, width=34)
        oldCallTxt <- widg$callEntry$getActiveText()
        if ((widg$callEntry["active"] == -1) || (callTxt != oldCallTxt)) {
            ## a new call: edited inline OR playState$call modified
            widg$callEntry$prependText(callTxt)
            widg$callEntry["active"] <- 0
            ## remove any later history
            histLev <- playState$tmp$call.history.level
            if (any(histLev > 0)) {
                for (i in seq(histLev-1, 0)+1)
                  widg$callEntry$removeText(i)
            }
        }
        playState$tmp$call.history.level <- widg$callEntry["active"]
    }
}

## TODO: use new stuff in gridwork.R
## store current.transform() for each viewport?
generateSpaces <- function(playState)
{
    playState$deviceToSpace <- list()
    if (!is.null(playState$viewport)) {
        ## grid graphics plot
        for (space in names(playState$viewport)) {
            playState$deviceToSpace[[space]] <-
                playDo(playState, deviceToUserCoordsFunction(), space=space)
        }
    }
    else if (playState$is.lattice) {
        ## lattice plot
        packets <- trellis.currentLayout(which="packet")
        for (pn in packets[packets > 0]) {
            space <- paste("packet", pn)
            playState$deviceToSpace[[space]] <-
                playDo(playState, deviceToUserCoordsFunction(), space=space)
        }
    }
    else {
        ## base graphics plot
        upViewport(0)
        if (length(playState$baseViewports$plot.clip.off)) {
            test <- try(downViewport(playState$baseViewports$plot.clip.off),
                        silent=TRUE)
            if (!inherits(test, "try-error") && length(current.vpPath()))
                popViewport(0)
        }
        ## suppressWarnings about log scale
        vps <- suppressWarnings(baseViewports())
        playState$baseViewports <- list()
        pushViewport(vps$inner)
        playState$baseViewports$inner <- current.vpPath()
        pushViewport(vps$figure)
        playState$baseViewports$figure <- current.vpPath()
        ## set clipping
        vps$plot$clip <- TRUE
        pushViewport(vps$plot)
        playState$baseViewports$plot <- current.vpPath()
        pushViewport(viewport(
                              xscale=convertX(unit(0:1, "npc"), "native"),
                              yscale=convertY(unit(0:1, "npc"), "native"),
                              clip="off"))
        playState$baseViewports$plot.clip.off <- current.vpPath()
        upViewport(4)
        playState$deviceToSpace[["plot"]] <-
            playDo(playState, deviceToUserCoordsFunction(), space="plot")
    }
    playState$tmp$need.reconfig <- FALSE
}

### Window signal handlers

pages_post.plot.action <- function(widget, playState)
{
    widg <- playState$widgets
    if (playState$pages > 1) {
        widg$pageScrollbar["adjustment"]["upper"] <- playState$pages+1
        widg$pageScrollbar["adjustment"]["value"] <- playState$page
        widg$pageEntry["text"] <- as.character(playState$page)
        widg$pageScrollBox["sensitive"] <- TRUE
        widg$pageScrollbar["sensitive"] <- TRUE
        widg$pageEntry["sensitive"] <- TRUE
        widg$pageScrollBox$show()
    } else {
                                        #widg$pageScrollBox$hide()
        widg$pageScrollbar["sensitive"] <- FALSE
        widg$pageEntry["sensitive"] <- FALSE
    }
}

window.close_handler <- function(widget, event, playState)
{
    if (!is.null(playState$on.close)) {
        foo <- try(playState$on.close(playState))
        ## if on.close() returns TRUE, do not close the window
        if (isTRUE(foo)) return(TRUE)
    }
    ## close the window and clean up
    playDevOff(playState)
    return(FALSE)
}

configure_handler <- function(widget, event, playState)
{
    playState$tmp$need.reconfig <- TRUE
    return(FALSE)
}

auto.reconfig_handler <- function(widget, event, playState)
{
    ## avoid weird stack smash
    if (length(playState$is.lattice) == 0) return(FALSE)
    if (!isTRUE(playState$plot.ready)) return(FALSE)
    if (isBasicDeviceMode(playState)) {
        ## do not know when the plot is updated
        ## so need to keep regenerating data space
        playState$tmp$need.reconfig <- TRUE
    }
    if (playState$tmp$need.reconfig) {
        generateSpaces(playState)
    }
    return(FALSE)
}

devoff_handler <- function(widget, event, playState)
{
    ## this handles dev.off()
    ## destroy the window, but store a flag to avoid destroying twice
    if (isTRUE(playState$devoff)) return(FALSE)
    playState$devoff <- TRUE
    playDevOff(playState)
    return(FALSE)
}

gtkmain_handler <- function(widget, event, playState)
{
  if (!isTRUE(playState$show.tooltips))
    return(gtkmainquit_handler(widget, event, playState))
  ## switch to GTK event loop while the window is in focus (for tooltips)
  if (!isTRUE(playState$tmp$gtkMain)) {
    playState$tmp$gtkMain <- TRUE
    gtkMain()
  }
  return(FALSE)
}

gtkmainquit_handler <- function(widget, event, playState)
{
  if (isTRUE(playState$tmp$gtkMain)) {
    playState$tmp$gtkMain <- FALSE
    gtkMainQuit()
  }
  return(FALSE)
}

## General utility functions

## NOT USED. +.layer approach better i think.
plotadd <- function(FUN, ..., add.stuff=expression()) {
    force(FUN)
    foo <- sys.call()
    foo[[1]] <- foo[[2]]
    foo <- foo[-2]
    foo$add.stuff <- NULL
    retval <- eval.parent(foo)
    for (x in add.stuff) eval.parent(x)
    retval
}

## TODO: this fails with one-line inline functions
deparseOneLine <-
    function(expr, width.cutoff=500, ...)
{
    tmp <- deparse(expr, width.cutoff=width.cutoff, ...)
    indents <- attr(regexpr("^ *", tmp), "match.length")
    breaks <- c(diff(indents) <= 0, FALSE)
    tmp <- gsub("^ +", "", tmp)
    tmp <- gsub(" +$", "", tmp)
    breaks[c(tmp[-1]=="{", FALSE)] <- F
    tmp <- paste(tmp, ifelse(breaks, ";", ""), sep="", collapse=" ")
    tmp <- gsub("\\{;", "\\{", tmp)
    tmp <- gsub(";\\}", " \\}", tmp)
    tmp <- gsub(";\\{", " \\{", tmp)
    tmp
}

xy.coords.call <-
    function(the.call, envir=parent.frame(), log=NULL, recycle=TRUE)
{
    stopifnot(is.call(the.call))
    ## put call into canonical form
    the.call <- match.call(eval(the.call[[1]], envir=envir), the.call)
    tmp.x <- eval(the.call$x, envir)
    tmp.y <- if ('y' %in% names(the.call)) eval(the.call$y, envir)
    if (inherits(tmp.x, "zoo") && is.null(tmp.y))
        return(xy.coords(stats::time(tmp.x), as.vector(tmp.x), log=log, recycle=recycle))
    xy.coords(tmp.x, tmp.y, log=log, recycle=recycle)
}

## export this?
copyLocalArgs <-
    function(the.call,
             envir = parent.frame(),
             newEnv,
             evalGlobals = FALSE,
             pattern = TRUE,
             invert.match = FALSE)
{
    stopifnot(is.call(the.call) || is.list(the.call) || is.expression(the.call))
    isMatch <- !invert.match
    for (i in seq_along(the.call)) {
        this.arg <- the.call[[i]]
        if (is.call(the.call) && (i == 1)) {
            if (mode(this.arg) == "name") {
                callname <- as.character(this.arg)
                ## skip base operators, unlikely to be locally redefined
                if (exists(callname, baseenv(), mode="function"))
                    next
            } else {
                ## anonymous function call
                next
            }
        }

        if (is.call(this.arg)) {
            if (mode(this.arg[[1]]) == "name") {
                argcallname <- as.character(this.arg[[1]])
                ## skip function definitions
                ## (could skip function entirely, but it may refer
                ##  to variables in outer context)
                if (argcallname == "function")
                    this.arg <- this.arg[[3]]
                ## skip assigned variables
                if (argcallname == "<-")
                    this.arg <- this.arg[[3]]
                ## skip literal symbol in "$" extractor
                if (argcallname == "$")
                    this.arg <- this.arg[[2]]
            }
        }

        if (mode(this.arg) %in% c("call", "(", "list", "expression")) {
            ## call recursively...
            copyLocalArgs(this.arg, envir=envir, newEnv=newEnv,
                          evalGlobals=evalGlobals, pattern=pattern,
                          invert.match=invert.match)
        } else if (mode(this.arg) == "name") {
            this.name <- as.character(this.arg)
            ## check if this name already exists in local env
            if (exists(this.name, newEnv, inherits=F))
                next
            ## check that the name matches the pattern
            if (!isTRUE(pattern) &&
                (any(grep(pattern, this.name))) != isMatch)
                next
            ## check if name exists in 'envir' or its parents
            ## (up to global env only, i.e. local objects)
            testenv <- envir
            hit <- FALSE
            while (TRUE) {
                if (exists(this.name, testenv, inherits=FALSE)) {
                    assign(this.name, get(this.name, testenv), newEnv)
                    hit <- TRUE
                    break
                }
                testenv <- parent.env(testenv)
                if (environmentName(testenv) %in%
                    c("R_GlobalEnv", "R_EmptyEnv", "base"))
                    break
            }
            if (hit) next
            ## eval globals
            if (evalGlobals &&
                exists(this.name, globalenv(), inherits=FALSE))
                assign(this.name, get(this.name, testenv), newEnv)
        }
        ## leave constants alone
    }
}

recursiveIndex <- function(call, index) {
    ## if index is simple...
    if (length(index) == 1) {
        ## if index is NA, use original call object
        if (is.na(index)) return(call)
        return(call[[index]])
    }
    getx <- paste("[[", index, "]]", sep="", collapse="")
    eval(parse(text=paste("call", getx, sep="")))
}

"recursiveIndex<-" <- function(call, index, value) {
    ## if index is simple...
    if (length(index) == 1) {
        ## if index is NA, use original call object
        if (is.na(index)) return(value)
        call[[index]] <- value
        return(call)
    }
    getx <- paste("[[", index, "]]", sep="", collapse="")
    eval(parse(text=paste("call", getx, " <- value", sep="")))
    call
}

recursive.as.list.call <- function(x) {
    stopifnot(is.call(x))
    x <- as.list(x)
    lapply(x, function(z) if (is.call(z))
           recursive.as.list.call(z) else z)
}

## by Deepayan Sarkar <Deepayan.Sarkar@R-project.org>

packet.panel.page <- function(n)
{
    ## returns a function that when used as the 'packet.panel'
    ## argument in print.trellis plots page number 'n' only
    function(layout, page, ...) {
        stopifnot(layout[3] == 1)
        packet.panel.default(layout = layout,
                             page = page + n - 1,
                             ...)
    }
}


