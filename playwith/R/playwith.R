## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

MAJOR <- "0"
MINOR <- "8"
REVISION <- unlist(strsplit("$Revision: 0 $", split=" "))[2]
VERSION <- paste(MAJOR, MINOR, REVISION, sep=".")
COPYRIGHT <- "(c) 2007 Felix Andrews <felix@nfrac.org>"
WEBSITE <- "http://playwith.googlecode.com/"

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

if (!exists("StateEnv", environment(), inherits=FALSE)) {
	StateEnv <- new.env()
}

## This is the high-level function: create the GUI

playwith <- function(
	expr, 
	title = NULL, 
	playDev = NULL,
	time.mode = FALSE,
	top.tools = playApplicationTools, 
	left.tools = playInteractionTools, 
	bottom.tools = list(),
	right.tools = list(), 
	show.call = TRUE,
	label.points = NULL, 
	labels = NULL, 
	label.style = list(cex=1),
	is.lattice = NA, 
	vp = NULL, 
	eval.args = NA, 
	invert.match = F, 
	envir = parent.frame(), 
	size = c(640, 480), 
	modal = FALSE,
	deletable = TRUE,
	restore.on.close = NULL, 
	plot.call)
{
	if (!missing(plot.call) && !missing(expr)) stop("give only one of 'expr' and 'plot.call'")
	if (missing(plot.call) && missing(expr)) expr <- quote({})
	if (missing(plot.call)) plot.call <- substitute(expr)
	if (is.expression(plot.call)) {
		plot.call <- if (length(plot.call) > 1)
			as.call(c(as.symbol("{"), plot.call)) else plot.call[[1]]
	}
	# check types
	if (!is.call(plot.call)) stop("'plot.call' should be a call object")
	if (!is.null(title) && !is.character(title)) stop("'title' should be character")
	# work out id (unique identifier for the "device")
	ID <- title
	if (!is.null(playDev)) {
		ID <- playDev$ID
		if (is.null(title)) title <- playDev$title
	}
	if (is.null(ID)) ID <- basename(tempfile())
	# playState is the <environment> encapsulating the plot window and device
	playState <- if (exists(ID, StateEnv, inherits=FALSE)) {
		playState <- StateEnv[[ID]]
	} else {
	 	playState <- new.env()
	}
	playState$ID <- ID
	StateEnv[[ID]] <- playState
	StateEnv$.current <- playState
	# work out evaluation rules
	env <- new.env()
	inherits <- !is.na(eval.args)
	if (is.na(eval.args)) eval.args <- (environmentName(envir) != "R_GlobalEnv")
	if (!identical(eval.args, FALSE)) {
		copyArgsIntoEnv(plot.call, envir=envir, newEnv=env, inherits=inherits, 
			pattern=eval.args, invert.match=invert.match)
	}
	# check whether the window already exists
	if (!is.null(myWin <- playState$win) &&
		inherits(myWin, "GtkWindow")) {
		# remove everything
		playState$devoff <- TRUE # to avoid trigger close
		myWin$getChild()$destroy()
		#myWin$remove(myWin$getChild())
		myWin$present()
	} else {
		# create a new window
		myWin <- gtkWindow(show=FALSE)
		if (!inherits(myWin, "GtkWindow")) stop(paste(
			"Could not create the GTK window.",
			"Make sure you have recent versions of",
			"RGtk2 and the GTK+ libraries.",
			"See http://www.ggobi.org/rgtk2/"))
		myWin["default-width"] <- size[1]
		myWin["default-height"] <- size[2]
		myWin["modal"] <- modal
		myWin["deletable"] <- deletable
		myWin$show()
		gSignalConnect(myWin, "delete-event",  window.close_handler, 
			data=playState)
		gSignalConnect(myWin, "focus-in-event", 
			window.focus.in_handler, data=playState)
		gSignalConnect(myWin, "focus-out-event", 
			window.focus.out_handler, data=playState)
	}
	if (!is.null(title)) myWin["title"] <- title
	widg <- list()
	myVBox <- gtkVBox()
	myWin$add(myVBox)
	# create the call toolbar (similar to web browser address bar)
	callToolbar <- gtkToolbar(show=show.call)
	callToolbar["toolbar-style"] <- GtkToolbarStyle["icons"]
	callToolbar["show-arrow"] <- FALSE
	undoButton <- quickTool(playState, "Back", 
		icon = "gtk-undo-ltr", 
		tooltip = "Go back to previous plot call", 
		f = function(widget, playState) {
			with(playState$widgets, {
				callEntry["active"] <- callEntry["active"] + 1
				callEntry$getChild()$activate()
				#playState$call <- parse(text=callEntry['text'])[[1]]
				#TODO if callEntry['model']$iterNChildren()
				redoButton['sensitive'] <- TRUE
			})
		})
	redoButton <- quickTool(playState, "Forward", 
		icon = "gtk-redo-ltr", 
		tooltip = "Go forward to next plot call", 
		f = function(widget, playState) {
			with(playState$widgets, {
				callEntry["active"] <- callEntry["active"] - 1
				callEntry$getChild()$activate()
			})
		})
	redrawButton <- quickTool(playState, "Redraw", 
		icon = "gtk-refresh", 
		tooltip = "Re-draw the current plot", 
		f = function(widget, playState) {
			playNewPlot(playState)
		})
	undoButton["sensitive"] <- FALSE
	redoButton["sensitive"] <- FALSE
	helpButton <- toolConstructors$help(playState)
	callToolbar$insert(undoButton, -1)
	callToolbar$insert(redoButton, -1)
	callToolbar$insert(redrawButton, -1)
	callToolbar$insert(helpButton, -1)
	callEntry <- gtkComboBoxEntryNewText()
	callEntry$show()
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
	myVBox$packStart(callToolbar, expand=FALSE)
	# top toolbar: shares space with the prompt (used for interaction)
	toolbarPromptHBox <- gtkHBox()
	myVBox$packStart(toolbarPromptHBox, expand=FALSE)
	# create the top toolbar
	topToolbar <- gtkToolbar()
	topToolbar["toolbar-style"] <- GtkToolbarStyle['both']
	toolbarPromptHBox$packStart(topToolbar)
	# create the prompt
	promptBox <- gtkEventBox(show=FALSE)
	promptLabel <- gtkLabel()
	promptBox$add(promptLabel)
	promptBox$modifyBg(GtkStateType['normal'], "yellow")
	promptLabel$modifyFg(GtkStateType['normal'], "black")
	toolbarPromptHBox$packStart(promptBox)
	# create the plot area and side toolbars
	myHBox <- gtkHBox()
	myVBox$packStart(myHBox)
	# create the left toolbar
	leftToolbar <- gtkToolbar()
	leftToolbar["orientation"] <- GtkOrientation["vertical"]
	leftToolbar["toolbar-style"] <- GtkToolbarStyle['both']
	myHBox$packStart(leftToolbar, expand=FALSE)
	# create the plot area
	myDA <- gtkDrawingArea()
	myHBox$packStart(myDA)
	myHBox["resize-mode"] <- GtkResizeMode["queue"] # ?
	asCairoDevice(myDA)
	gSignalConnect(myHBox, "remove", devoff_handler, 
		data=playState, after=TRUE)
	trellis.device(new=F)
	# create the page scrollbar
	pageScrollBox <- gtkVBox()
	myHBox$packStart(pageScrollBox, expand=FALSE)
	pageScrollBox$packStart(gtkLabel("Page"), expand=FALSE)
	pageEntry <- gtkEntry()
	pageEntry["width-chars"] <- 2
	gSignalConnect(pageEntry, "activate", 
		function(widget, playState) {
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
	pageScrollbar["update-policy"] <- GtkUpdateType["discontinuous"]
	gSignalConnect(pageScrollbar, "value-changed", 
		function(widget, playState) {
			newPage <- round(widget$getValue())
			if (newPage == playState$page) return()
			playState$page <- newPage
			playReplot(playState)
		},
		data=playState)
	pageScrollBox$packStart(pageScrollbar)
	# create the right toolbar
	rightToolbar <- gtkToolbar()
	rightToolbar["orientation"] <- GtkOrientation["vertical"]
	rightToolbar["toolbar-style"] <- GtkToolbarStyle["both"]
	myHBox$packStart(rightToolbar, expand=FALSE)
	# create the time/index scrollbar
	timeScrollBox <- gtkHBox()
	myVBox$packStart(timeScrollBox, expand=FALSE)
	timeScrollBox$packStart(gtkLabel("Time"), expand=FALSE)
	timeEntry <- gtkEntry()
	timeEntry["width-chars"] <- 30
	gSignalConnect(timeEntry, "activate", 
		function(widget, playState) {
			newLim <- strsplit(widget["text"], " to ")[[1]]
			if ((length(newLim) != 2)) {
				gmessage.error("Give bounds in form \"LOWER to UPPER\".")
				return()
			}
			# TODO date / time
			callArg(playState, xlim) <- as.numeric(newLim)
			playReplot(playState)
		},
		data=playState)
	timeScrollBox$packStart(timeEntry, expand=FALSE)
	timeScrollbar <- gtkHScrollbar()
	timeScrollbar["adjustment"] <- gtkAdjustment()
	timeScrollbar["update-policy"] <- GtkUpdateType["discontinuous"]
	gSignalConnect(timeScrollbar, "value-changed", 
		function(widget, playState) {
			newLim <- widget$getValue()
			newLim[2] <- newLim + widget["adjustment"]["page-size"]
			if (widget["adjustment"]["page-size"] == 0) stop()
			#oldLim <- rawXLim(playState)
			#if (min(oldLim) == min(newLim)) return()
			rawXLim(playState) <- newLim
			playReplot(playState)
		},
		data=playState)
	timeScrollBox$packStart(timeScrollbar)
	
			# TODO ... (old stuff)
			makeIndexButton <- function() {
				name <- StateEnv$.current
				myStep <- 1
				if ('gui.step' %in% names(playState$call))
					myStep <- eval(playState$call$gui.step, playState$env)
				spinner <- gtkSpinButton(min=1, max=999999, step=myStep)
				spinner["value"] <- 1
				playState$env$gui.index <- 1
				gSignalConnect(spinner, "value-changed", .plotAndPlay_index_event)
				vbox <- gtkVBox()
				vbox$packStart(gtkLabel("Index:"))
				vbox$packStart(spinner)
				foo <- gtkToolItem()
				foo$add(vbox)
				foo
			}
			.plotAndPlay_index_event <- function(widget, user.data) {
				name <- StateEnv$.current
				playState$env$gui.index <- widget["value"]
				plotAndPlayUpdate()
			}
	
	# create the bottom toolbar
	bottomToolbar <- gtkToolbar()
	bottomToolbar["toolbar-style"] <- GtkToolbarStyle['both']
	myVBox$packStart(bottomToolbar, expand=FALSE)
	# store the state of this plot window in a new environment
	missing_top.tools <- missing(top.tools)
	missing_left.tools <- missing(left.tools)
	evalq({
		win <- myWin
		dev <- dev.cur()
		old.dev <- dev.cur()
		call <- plot.call
		env <- env
		time.mode <- time.mode
		label.points <- label.points
		labels <- labels
		label.style <- label.style
		is.lattice <- is.lattice
		vp <- vp
		ids <- list()
		brushed <- list()
		annotations <- list()
		page <- 1
		restore.on.close <- restore.on.close
		tools <- list()
		widgets <- list(
			drawingArea = myDA,
			topToolbar = topToolbar,
			leftToolbar = leftToolbar,
			bottomToolbar = bottomToolbar,
			rightToolbar = rightToolbar,
			callToolbar = callToolbar,
			callEntry = callEntry,
			undoButton = undoButton,
			redoButton = redoButton,
			pageEntry = pageEntry,
			pageScrollbar = pageScrollbar,
			pageScrollBox = pageScrollBox,
			timeEntry = timeEntry,
			timeScrollbar = timeScrollbar,
			timeScrollBox = timeScrollBox,
			promptBox = promptBox,
			promptLabel = promptLabel,
			vbox = myVBox,
			hbox = myHBox
		)
		.args <- list(
			top.tools = top.tools, 
			left.tools = left.tools,
			bottom.tools = bottom.tools,
			right.tools = right.tools,
			missing_top.tools = missing_top.tools,
			missing_left.tools = missing_left.tools,
			is.lattice = is.lattice,
			label.points = label.points,
			labels = labels,
			title = title
		)
	},
		envir=playState,
		enclos=environment()
	)
	# do the plot
	invisible(playNewPlot(playState))
}

playNewPlot <- function(playState) {
	playDevSet(playState)
	plot.call <- playState$call
	env <- playState$env
	# clear the current plot if any, to avoid redraws
	plot.new()
	# clear toolbars and scrollbars
	playState$widgets$pageScrollBox$hide()
	playState$widgets$timeScrollBox$hide()
	tbars <- playState$widgets[
		c("topToolbar", "leftToolbar", "bottomToolbar", "rightToolbar")]
	for (tbar in tbars) {
		tbar$hide()
		for (x in rev(tbar$getChildren())) x$destroy()
	}
	# get access to some of the original arguments
	argfoo <- playState$.args
	for (foo in names(argfoo)) assign(foo, argfoo[[foo]])
	# TODO: global user option for default buttons?
	# put call into canonical form
	callFun <- eval(plot.call[[1]], env)
	callName <- deparseOneLine(plot.call[[1]])
	if (is.na(is.lattice)) is.lattice <- (callName %in% latticeNames)
	playState$is.lattice <- is.lattice
	# check whether the call accepts arguments
	noArgs <- F
	if ((typeof(callFun) == "closure") && !is.null(formals(callFun))) {
		firstArgName <- names(plot.call)[2]
		plot.call <- match.call(callFun, plot.call)
		if (is.null(firstArgName) || (firstArgName == ""))
			if (!is.null(names(plot.call))) names(plot.call)[2] <- ""
	} else {
		noArgs <- T
	}
	# set cleaned-up call
	playState$call <- plot.call
	# choose buttons automatically depending on the type of plot
	if (noArgs) {
		if (missing_left.tools) left.tools <- list()
	}
	if (is.lattice && (callName %in% c("cloud", "wireframe"))) {
		if (missing_left.tools) left.tools <- play3DTools
	}
	if (is.lattice && (callName %in% "splom")) {
		if (missing_left.tools) left.tools <- playSplomTools
	}
	# eval buttons and add them to the toolbars
	# note that these may be user-defined initialisation functions
	# if an element evaluates to NA it is skipped
	populateToolbar <- function(toolbar, tools) {
		for (i in seq_along(tools)) {
			myName <- names(tools)[i]
			if (!any(nchar(myName))) myName <- NA
			toolFun <- tools[[i]]
			if (is.character(toolFun)) {
				myName <- tools[[i]]
				toolFun <- toolConstructors[[myName]]
			}
			if (!is.function(toolFun)) {
				warning("constructor for ", myName, " is not a function")
				next
			}
			# call the tool constructor
			newTool <- try(toolFun(playState))
			if (inherits(newTool, "try-error") || is.null(newTool)) {
				warning("constructor for ", myName, " failed")
				next
			}
			if (identical(newTool, NA)) next
			result <- try(toolbar$insert(newTool, -1))
			if (inherits(result, "try-error")) next
			if (any(nchar(myName))) playState$tools[[myName]] <- newTool
		}
	}
	# set up the toolbars
	with(playState$widgets, {
		populateToolbar(topToolbar, top.tools)
		populateToolbar(leftToolbar, left.tools)
		populateToolbar(bottomToolbar, bottom.tools)
		populateToolbar(rightToolbar, right.tools)
	})
	for (tbar in tbars) {
		if (length(tbar$getChildren()) > 0) tbar$show()
	}
	playReplot(playState)
}

playReplot <- function(playState) {
	if (isTRUE(playState$skip.redraws)) return()
	#str(sys.calls())
	playDevSet(playState)
	plot.new()
	playPrompt(playState) <- NULL
	# disable toolbars until this is over
	playFreezeGUI(playState)
	on.exit(playThawGUI(playState))
	widg <- playState$widgets
	# add current call to text box
	callTxt <- ""
	if (object.size(playState$call) < 50000) {
		callTxt <- deparseOneLine(playState$call) #control="showAttributes")
		if (is.null(playState$title)) playState$win["title"] <- 
			toString(callTxt, width=34)
		oldCallTxt <- widg$callEntry$getActiveText()
		if ((widg$callEntry["active"] == -1) || (callTxt != oldCallTxt)) {
			# a new call: edited inline OR playState$call modified
			widg$callEntry$prependText(callTxt)
			widg$callEntry["active"] <- 0
			widg$undoButton["sensitive"] <- TRUE
		}
		widg$redoButton["sensitive"] <- (widg$callEntry["active"] > 0)
	}
	# do the plot
	result <- eval(playState$call, playState$env)
	if (inherits(result, "trellis")) {
		playState$trellis <- result
		# set back to this device, since user may have switched during plot
		playDevSet(playState)
		# work out panels and pages
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
		curPage <- playState$page
		if (nPages > 1) {
			blockRedraws({
				widg$pageScrollbar["adjustment"]["upper"] <- nPages+1
				widg$pageScrollbar["adjustment"]["value"] <- curPage
				widg$pageEntry["text"] <- as.character(curPage)
			})
			widg$pageScrollBox$show()
		} else {
			widg$pageScrollBox$hide()
		}
		# plot trellis object
		plot(result, packet.panel=packet.panel.page(curPage))
	}
	# run update actions on buttons
	blockRedraws({
		for (x in playState$tools) {
			playDevSet(playState)
			xUpd <- gObjectGetData(x, "post.plot.action")
			if (!is.null(xUpd)) {
				if (!is.function(xUpd)) {
					warning("post.plot.action not a function")
					next
				}
				xUpd(x, playState=playState)
			}
		}
	})
	invisible(result)
}

## Window signal handlers

window.focus.in_handler <- function(widget, event, playState) {
	#playDevSet(playState)
	return(FALSE)
}

window.focus.out_handler <- function(widget, event, playState) {
	#name <- user.data$name
	# revert to previous device
	#dev.set(playState$old.dev)
	return(FALSE)
}

devoff_handler <- function(widget, event, playState) {
	# destroy the window, but store a flag to avoid destroying twice
	if (any(playState$devoff)) return(FALSE)
	playState$devoff <- TRUE
	try(playState$win$destroy(), silent=TRUE)
	
	#name <- StateEnv$.current # TODO: get this from user.data
	# destroy the window, but store a flag to avoid destroying twice
	#if (any(playState$devoff)) return(FALSE)
	#playState$devoff <- TRUE
	#try(playState$win$destroy(), silent=TRUE)
	return(FALSE)
}

window.close_handler <- function(widget, event, playState) {
	restoreWin <- playState$restore.on.close
	if (!is.null(restoreWin) && inherits(restoreWin, "gtkWindow")) {
		try(restoreWin$present())
	}
	rm(list=playState$ID, envir=StateEnv)
	# TODO: does this happen on dev.off() ?
	StateEnv$.current <- if (length(StateEnv)) StateEnv[[ ls(StateEnv)[1] ]]
	return(FALSE)
}

## List of known Lattice (high-level) function names

latticeNames <- c("barchart", "bwplot", "cloud", "contourplot", "densityplot", 
	"dotplot", "histogram", "levelplot", "parallel", "qq", "qqmath", "rfs", 
	"splom", "stripplot", "tmd", "wireframe", "xyplot",
	# packages sp and latticeExtra
	"spplot", "bubble", "mapplot", "gplot", "ecdfplot", "rootogram")

## Automatic playwith support

autoplay <- function(on=NA, lattice.on=on, base.on=on, ask=FALSE) {
	if (all(is.na(c(lattice.on, base.on))))
		message("No action taken.")
	if (!is.na(lattice.on)) {
		library("lattice")
		if (packageDescription("lattice")$Version < package_version("0.17-1"))
			stop("this requires lattice package version >= 0.17")
		newFoo <- if (lattice.on) playwith.trellis else NULL
		lattice.options(print.function = newFoo)
		message("Automatic `playwith` for Lattice graphics is now ", 
			if (lattice.on) "ON." else "OFF.")
	}
	if (!is.na(base.on)) {
		newFoo <- if (base.on) list(playwith.plot.new) else NULL
		setHook("plot.new", newFoo, "replace")
		message("Automatic `playwith` for base graphics is now ",
			if (base.on) "ON." else "OFF.")
		if (base.on) {
			StateEnv$.autoplay.ask <- ask
			if (ask) message("User will be asked to choose base plot calls.")
		}
	}
	invisible()
}

# not to be called directly
playwith.trellis <- 
   function(x, position = NULL, split = NULL, more = FALSE, newpage = TRUE,
            packet.panel = packet.panel.default, draw.in = NULL, ...)
{
   dev.interactive2 <- function(orNone)
   {
       dev.interactive(orNone) ||
       (interactive() && .Device == "null device" &&
        getOption("device") == "Cairo")
   }
   new <- (newpage && is.null(draw.in) &&
           !lattice:::lattice.getStatus("print.more"))
   if (dev.interactive2(TRUE) && new) {
       ## starting a new plot on an interactive device
       eval(call("playwith", x$call, is.lattice=TRUE, envir=parent.frame(2)))
       return(invisible())
   }
   ## call `plot.trellis` from lattice package, as usual
   ocall <- sys.call()
   ocall[[1]] <- quote(plot)
   eval.parent(ocall)
}

# not to be called directly
playwith.plot.new <- function(...) {
	sysCallNames <- sapply(sys.calls(), function(x)
		ifelse(is.symbol(x[[1]]), toString(x[[1]]), ""))
	playing <- any(c("playReplot", "playNewPlot") 
		%in% sysCallNames)
	multifig <- !isTRUE(all.equal(par("mfrow"), c(1,1)))
	first <- isTRUE(all.equal(par("mfg")[1:2], c(1,1)))
	opar <- par(no.readonly=TRUE)
	if (dev.interactive() && !playing && first && !par("new")) {
		## starting a new plot on an interactive device
		frameNum <- which(sysCallNames != "")[1]
		if (any(StateEnv$.autoplay.ask)) {
			items <- make.unique(sapply(sys.calls(), function(x)
				toString(deparseOneLine(x), width=34)))
			myItem <- select.list(items, preselect=items[frameNum],
				title="Choose plot call for playwith:")
			frameNum <- match(myItem, items)
			if (is.na(frameNum)) return()
		}
		dev.off() # close screen device (from `plot.new`)
		parentFrame <- sys.frame(sys.parents()[frameNum])
		newCall <- call("playwith", sys.call(frameNum), 
			is.lattice=FALSE, envir=parentFrame)
		if (multifig) {
			newCall$buttons <- list("annotate")
			newCall$extra.buttons <- list()
		}
		# eval.args=FALSE ?
		eval(newCall)
		par(opar) # on the new device (redrawing now)
		.Internal(plot.new())
	}
	return()
}

## General utility functions

plotadd <- function(name, ..., add.stuff=expression()) {
	eval.parent(call(deparse(substitute(name)), ...))
	for (x in add.stuff) eval.parent(x)
}

deparseOneLine <- function(expr, width.cutoff=500, ...) {
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

xy.coords.call <- function(the.call, envir=parent.frame(), log=NULL, recycle=TRUE) {
	stopifnot(is.call(the.call))
	# put call into canonical form
	the.call <- match.call(eval(the.call[[1]], envir=envir), the.call)
	tmp.x <- eval(the.call$x, envir)
	tmp.y <- if ('y' %in% names(the.call)) eval(the.call$y, envir)
	if (inherits(tmp.x, "zoo") && is.null(tmp.y)) 
		return(xy.coords(stats::time(tmp.x), as.vector(tmp.x), log=log, recycle=recycle))
	xy.coords(tmp.x, tmp.y, log=log, recycle=recycle)
}

copyArgsIntoEnv <- function(the.call, envir=parent.frame(), newEnv, inherits=F, pattern=T, invert.match=F) {
	stopifnot(is.call(the.call) || is.list(the.call) || is.expression(the.call))
	isMatch <- !invert.match
	for (i in seq_along(the.call)) {
		if (is.call(the.call) && (i == 1)) next
		this.arg <- the.call[[i]]
		# skip literal symbol in "$" extractor
		if (is.call(this.arg) && this.arg[[1]] == as.symbol("$"))
			this.arg <- this.arg[[2]]
		
		if (mode(this.arg) %in% c("call", "(", "list", "expression")) {
			# call recursively...
			copyArgsIntoEnv(this.arg, envir=envir, newEnv=newEnv,
				inherits=inherits, pattern=pattern, 
				invert.match=invert.match)
		} else if (mode(this.arg) %in% "name") {
			this.name <- as.character(this.arg)
			if (!isTRUE(pattern) && 
				(any(grep(pattern, this.name))) != isMatch)
				next
			if (exists(this.name, envir=envir, inherits=inherits)
			&& !exists(this.name, envir=newEnv, inherits=F)) {
				assign(this.name, eval(this.arg, envir=envir),
					envir=newEnv)
			}
		}
		# leave constants alone
	}
}

evalCallArgs <- function(myCall, envir=parent.frame(), pattern=T) {
	for (i in seq(along=myCall)) {
		if ((mode(myCall) %in% "call") && (i == 1)) {
			next # don't eval function itself
		}
		if (isTRUE(pattern)) {
			myCall[[i]] <- eval(myCall[[i]], envir)
		} else if (any(grep(pattern, deparse(myCall[[i]])))) {
			myCall[[i]] <- eval(myCall[[i]], envir)
		} else if (mode(myCall[[i]]) %in% c("call","list")) {
			myCall[[i]] <- evalCallArgs(myCall[[i]], envir=envir,
				pattern=pattern)
		}
	}
	return(myCall)
}

recursive.as.list.call <- function(x) {
	stopifnot(is.call(x))
	x <- as.list(x)
	lapply(x, function(z) if (is.call(z))
		recursive.as.list.call(z) else z)
}

toIndexStr <- function(x) paste('[[', x ,']]', sep='', collapse='')

# based on grid::locator
deviceNPCToVp <- function(pos, unit="native", valueOnly=FALSE) {
	stopifnot(length(pos) == 2)
	# first find device size in inches
	# (note: par("din") is wrong, at least in my cairoDevice window)
	din <- grid:::grid.Call("L_currentViewport")[c("devwidthcm","devheightcm")]
	din <- convertX(unit(unlist(din),"cm"), "inches", valueOnly=T)
	location <- c(din * as.numeric(unlist(pos)), 1)
        transform <- solve(grid::current.transform())
        location <- (location %*% transform)
	location <- unit(location/location[3], "inches")
	list(x=convertX(location[1], unit, valueOnly=valueOnly), 
		y=convertY(location[2], unit, valueOnly=valueOnly))
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


