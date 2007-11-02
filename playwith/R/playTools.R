## plotAndPlayGTK: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

playApplicationTools <- list(
	"keep",
	"save",
	"copy",
	"print",
	"data",
	"--",
	"time.mode"
	"---",
	"settings",
)

playInteractionTools <- list(
	"options",
	"--",
	"expand",
	"identify",
	"annotate",
	"arrow",
	"edit.annotations",
	"clear",
	"--",
	"zoom",
	"zoomout",
	"zoomfit",
	"zero"
)

play3DTools <- list(
	"options",
	"--",
	"expand",
	"annotate",
	"arrow",
	"edit.annotations",
	"clear",
	"--",
	"zoomin.3d",
	"zoomout.3d", 
	"fly.left.3d",
	"fly.right.3d"
)

playSplomTools <- list(
	"options",
	"--",
	"expand",
	"annotate",
	"arrow",
	"edit.annotations",
	"clear",
	"--",
	"brush",
	"brush.region",
	"brush.drag"
)

toolConstructors <- list(
	`--` = function(...) gtkSeparatorToolItem(),
	`---` = function(...) {
		foo <- gtkSeparatorToolItem()
		foo$setExpand(TRUE)
		foo$setDraw(FALSE)
		foo
	}
)

## Convenient constructor function

quickTool <- function(
	playState,
	label = "", 
	icon.name = NULL, 
	tooltip = NULL, 
	f, 
	data = NULL, 
	post.plot.action = NULL,
	isToggle = F, 
	show = TRUE)
{
	x <- if (isToggle) gtkToggleToolButton(show=show)
		else gtkToolButton(show=show)
	x["label"] <- label
	x["icon-name"] <- icon.name
	if (!is.null(tooltip)) {
		result <- try(x$setTooltipText(tooltip), silent=TRUE)
		if (inherits(result, "try-error"))
			x$setTooltip(gtkTooltips(), tooltip) # deprecated
	}
	if (is.null(data)) data <- playState
	else data$playState <- playState
	gSignalConnect(x, "clicked", f, data=data)
	if (!is.null(post.plot.action))
		gObjectSetData(x, "post.plot.action", data=post.plot.action)
	x
}

## SETTINGS

toolConstructors$settings <- function(playState) {
	quickTool(playState,
		label = "Plot settings", 
		icon = "gtk-preferences", 
		tooltip = "Change the plot type and options",
		f = settings_handler)
}

settings_handler <- function(widget, playState) {
}

## KEEP

toolConstructors$keep <- function(playState) {
	quickTool(playState,
		label = "Keep plot", 
		icon = "gtk-leave-fullscreen", 
		tooltip = "Keep this window, do not replace it (open next plot in a new window)",
		f = keep_handler)
}

keep_handler <- function(widget, playState) {
}

## SAVE

toolConstructors$save <- function(playState) {
	saveButton <- gtkMenuToolButton(gtkImageNewFromStock("gtk-save-as", 
		size=GtkIconSize["small-toolbar"]), label="Save as")
	gSignalConnect(saveButton, "clicked", save_handler, 
		data=list(playState=playState))
	saveMenu <- gtkMenu()
	items <- list(
		pdf=gtkMenuItem("PDF"),
		png=gtkMenuItem("PNG (bitmap)"),
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

save_handler <- function(widget, user.data) {
	playState <- user.data$playState
	# disable toolbars until this is over
	playFreezeGUI(playState)
	on.exit(playThawGUI(playState))
	# get filename
	myExt <- if (!is.null(user.data$ext))
		user.data$ext else 'pdf'
	myDefault <- if (is.null(playState$title)) "plot" else playState$title
	myDefault <- paste(myDefault, myExt, sep=".")
	filename <- choose.file.save(myDefault, caption="Save plot (pdf/png/ps/etc)", 
		filters=Filters[c("pdf","png","ps","eps","svg","wmf","fig"),],
		index=match(myExt, c("pdf","png","ps","eps","svg","wmf","fig"))
	)
	if (is.na(filename)) return()
	ext <- tolower(get.extension(filename))
	if (ext == "") {
		filename <- paste(filename, '.pdf', sep='')
		ext <- 'pdf'
	}
	# save plot to file
	playDevSet(playState)
	mySize <- playState$widgets$drawingArea$getAllocation()
	myWidth <- mySize$width
	myHeight <- mySize$height
	myScale <- 1/72
	myWidth <- myWidth * myScale
	myHeight <- myHeight * myScale
	if (ext %in% "pdf") {
		dev.copy(pdf, file=filename, width=myWidth, height=myHeight)
		dev.off()
	}
	else if (ext %in% "ps") {
		dev.copy(postscript, file=filename, width=myWidth, height=myHeight)
		dev.off()
	}
	else if (ext %in% "eps") {
		dev.copy(postscript, file=filename, width=myWidth, height=myHeight,
			horizontal=FALSE, onefile=FALSE, paper="special")
		dev.off()
	}
	else if (ext %in% "png") {
		dev.copy(Cairo_png, file=filename, width=myWidth, height=myHeight)
		dev.off()
	}
	else if (ext %in% "svg") {
		dev.copy(Cairo_svg, file=filename, width=myWidth, height=myHeight)
		dev.off()
	}
	else if (ext %in% c("wmf", "emf")) {
		dev.copy(win.metafile, file=filename, width=myWidth, height=myHeight)
		dev.off()
	}
	else if (ext %in% "fig") {
		dev.copy(xfig, file=filename, width=myWidth, height=myHeight)
		dev.off()
	}
	else {
		errorDialog("Unrecognised filename extension")
		return()
	}
}

## COPY

toolConstructors$copy <- function(playState) {
	quickTool(playState,
		label = "Copy", 
		icon = "gtk-copy", 
		tooltip = "Copy this plot to the clipboard (as a bitmap)", 
		f = copy_handler)
}

copy_handler <- function(widget, playState) {
	# disable toolbars until this is over
	playFreezeGUI(playState)
	on.exit(playThawGUI(playState))
	# save plot to file
	filename <- paste(tempfile(), ".png", sep="")
	playDevSet(playState)
	mySize <- playState$widgets$drawingArea$getAllocation()
	myWidth <- mySize$width
	myHeight <- mySize$height
	myScale <- 1/72
	myWidth <- myWidth * myScale
	myHeight <- myHeight * myScale
	dev.copy(Cairo_png, file=filename, width=myWidth, height=myHeight)
	dev.off()
	im <- gdkPixbufNewFromFile(filename)$retval
	gtkClipboardGet("CLIPBOARD")$setImage(im)
	file.remove(filename)
}

## PRINT

toolConstructors$print <- function(playState) {
	quickTool(playState,
		label = "Print", 
		icon = "gtk-print", 
		f = print_handler)
}

print_handler <- function(widget, playState) {
	playDevSet(playState)
	isWindows <- (.Platform$OS.type == "windows")
	if (isWindows) dev.print(win.print)
	else dev.print()
}

## DATA

toolConstructors$data <- function(playState) {
	quickTool(playState,
		label = "Data...", 
		icon = "gtk-cdrom", 
		f = data_handler)
}

data_handler <- function(widget, playState) {
}


## TIME.MODE

toolConstructors$time.mode <- function(playState) {
	foo <- quickTool(playState,
		label = "Time mode", 
		icon = "gtk-media-forward-ltr",
		f = time.mode_handler, 
		isToggle = TRUE)
	# TODO: blockRedraws?
	foo["active"] <- playState$time.mode
	foo
}

time.mode_handler <- function(widget, playState) {
	playState$time.mode <- widget["active"]
	playState$widgets$timeScrollBox["visible"] <- playState$time.mode
}

## OPTIONS

toolConstructors$options <- function(playState) {
	myButton <- gtkButton("Options...")
	myMenu <- gtkMenu()
	# set label style
	labelStyleItem <- gtkMenuItem("Set label style...")
	myMenu$append(labelStyleItem)
	gSignalConnect(labelStyleItem, "activate", identify.setstyle_handler,
		data=playState)
	myMenu$append(gtkSeparatorMenuItem())
	# annotation mode
	myLabel <- gtkMenuItem("Annotation mode:")
	myLabel["sensitive"] <- FALSE
	myMenu$append(myLabel)
	styleItems <- list(
		figure=gtkCheckMenuItem(label="Place on figure (absolute)"),
		plot=gtkCheckMenuItem(label="Place on plot (relative)")
	)
	styleItems$figure["active"] <- TRUE
	# TODO...
	myMenu$append(gtkSeparatorMenuItem())
	# toolbar style
	myLabel <- gtkMenuItem("Toolbar style:")
	myLabel["sensitive"] <- FALSE
	myMenu$append(myLabel)
	styleItems <- list(
		icons=gtkCheckMenuItem(label="Icons"),
		text=gtkCheckMenuItem(label="Text"),
		both=gtkCheckMenuItem(label="Both"),
		`both-horiz`=gtkCheckMenuItem(label="Both Horizontal")
	)
	#for (x in names(styleItems)) {
	#	styleItems[[x]]["group"] <- styleItems[[1]]["group"]
	#}
	styleItems$both["active"] <- TRUE
	toolbarStyleHandler <- function(widget, user.data) {
		playState <- user.data$playState
		tb <- playState$widgets$userToolbar
		newStyle <- GtkToolbarStyle[user.data$style]
		if (widget["active"]) {
			tb["toolbar-style"] <- newStyle
			# hack because gtkRadioMenuItem is broken
			for (x in items) {
				if (!(x == widget)) x["active"] <- FALSE
			}
		}
	}
	for (x in names(styleItems)) {
		myMenu$append(styleItems[[x]])
		gSignalConnect(styleItems[[x]], "activate", toolbarStyleHandler, 
			data=list(playState=playState, style=x))
	}
	# attach the menu
	gSignalConnect(myButton, "button_press_event", 
		function(widget, event, menu) {
			menu$popup(button=event[["button"]], activate.time=event[["time"]])
		}, data=myMenu)
	foo <- gtkToolItem()
	foo$add(myButton)
	foo
}

identify.setstyle_handler <- function(widget, playState) {
	argsCall <- as.call(c(quote(list), playState$label.style))
	callTxt <- deparseOneLine(argsCall)
	
	# panel.text: cex, col, alpha, font, fontfamily, fontface, srt
	# text: cex, col, font, vfont, family, xpd, srt (90,180,270)
	repeat {
		newTxt <- guiTextInput(callTxt, title="Edit label style", 
			oneLiner=T, width.chars=54)
		if (is.null(newTxt)) break
		if (identical(newTxt, callTxt)) break
		callTxt <- newTxt
		tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
		# check whether there was a syntax error
		if (inherits(tmp, "error")) {
			errorDialog(paste("Error:", conditionMessage(tmp)))
		} else {
			playState$label.style <- eval(tmp)
			playReplot(playState)
			break
		}
	}
	playState$win$present()
}

## EXPAND

toolConstructors$expand <- function(playState) {
	if (!playState$is.lattice) return(NA)
	quickTool(playState,
		label = "Expand panel", 
		icon = "gtk-fullscreen", 
		tooltip = "Choose a panel to expand and focus (for further interaction)", 
		f = expand_handler, 
		post.plot.action = expand_postplot_action,
		isToggle = T)
}

expand_handler <- function(widget, playState) {
	playDevSet(playState)
	# check new expanded setting
	if (widget["active"]) {
		playPrompt(playState) <- "Click on a panel to expand (for further interaction)"
		newFocus <- trellis.focus()
		playPrompt(playState) <- NULL
		if (!any(newFocus)) return()
		playState$.old.call.layout <- playState$call$layout
		playState$call$layout <- c(0,1,1)
		playState$.old.page <- playState$page
		playState$page <- packet.number()
	} else {
		playState$call$layout <- playState$.old.call.layout
		playState$page <- playState$.old.page
	}
	playReplot(playState)
}

expand_postplot_action <- function(widget, playState) {
	widget["visible"] <- (widget["active"] ||
		(length(trellis.currentLayout() > 1)))
}

## IDENTIFY

toolConstructors$identify <- function(playState) {
	plot.call <- playState$call
	callName <- deparseOneLine(plot.call[[1]])
	if (playState$is.lattice &&
		!(callName %in% c("splom", "cloud", "levelplot",
			"contourplot", "wireframe", "parallel")) ) {
		# need this for correctly identifying points
		callArg(playState, subscripts) <- quote(T)
	}
	labels <- playState$.args$labels
	if (is.null(labels)) {
		if (is.null(playState$label.points)) {
			# try to construct labels from the plot call
			if (playState$is.lattice) {
				tmp.data <- NULL
				if ('data' %in% names(plot.call)) {
					tmp.data <- callArg(playState, data)
					labels <- makeLabels(tmp.data)
				}
				if (is.null(labels)) {
					# try to make labels from first argument
					tmp.x <- callArg(playState, 2)
					if (inherits(tmp.x, "formula")) {
						xObj <- if (length(tmp.x) == 2)
							tmp.x[[2]] else tmp.x[[3]]
						# get left-most term in formula
						while (is.call(xObj) && toString(xObj[[1]]) %in% 
							c("|", "*", "+"))
							xObj <- xObj[[2]]
						xObj <- eval(xObj, tmp.data, 
							environment(tmp.x))
						labels <- makeLabels(xObj)
					} else {
						labels <- makeLabels(tmp.x)
					}
				}
			} else {
				# base graphics
				if (length(plot.call) >= 2) {
					tmp.x <- callArg(playState, 2)
					if (inherits(tmp.x, "formula")) {
						xObj <- if (length(tmp.x) == 2)
							tmp.x[[2]] else tmp.x[[3]]
						xObj <- eval(xObj, environment(tmp.x), 
							playState$env)
						labels <- makeLabels(xObj)
					} else {
						labels <- makeLabels(tmp.x)
					}
				}
			}
		} else {
			# label.points were supplied
			labels <- makeLabels(playState$label.points)
		}
	}
	playState$labels <- labels
	# make the widget
	quickTool(playState,
		label = "Identify", 
		icon = "gtk-info",
		tooltip = "Identify data points by clicking on them",
		f = identify_handler,
		post.plot.action = identify_postplot_action)
}

makeLabels <- function(x) {
	labels <- row.names(x)
	if (inherits(x, "POSIXt"))
		labels <- format(x)
	if (inherits(x, "Date"))
		labels <- format(x)
	if (inherits(x, "ts") || inherits(x, "zoo"))
		labels <- rep(format(stats::time(x)), NCOL(x))
	labels
}

identify_handler <- function(widget, playState) {
	playDevSet(playState)
	# do identify
	if (!playState$is.lattice) {
		# base graphics plot
		idCall <- call("identify", 
			xy.coords.call(playState$call, env=playState$env))
		idCall <- as.call(c(as.list(idCall), playState$label.style))
		playPrompt(playState) <- paste("Identifying data points...",
			"Click the right mouse button to finish.")
		ids.new <- eval(idCall)
		# set identified points
		ids.old <- playState$ids$all # may be NULL
		playState$ids$all <- union(ids.old, ids.new)
		return()
	}
	# lattice plot
	idCall <- call("panel.identify")
	callName <- deparseOneLine(playState$call[[1]])
	if (callName == "qqmath") 
		identify.call <- call("panel.identify.qqmath")
	if (!is.null(playState$label.points)) 
		idCall[[2]] <- playState$label.points
	idCall <- as.call(c(as.list(idCall), playState$label.style))
	newFocus <- playFocus(clip.off=T)
	if (!any(newFocus > 0)) return()
	playPrompt(playState) <- paste("Identifying data points...",
		"Click the right mouse button to finish.")
	ids.new <- eval(idCall)
	# set identified points
	myPacket <- as.character(packet.number())
	ids.old <- playState$ids[[myPacket]] # may be NULL
	playState$ids[[myPacket]] <- union(ids.old, ids.new)
	trellis.unfocus()
}

identify.region_handler <- function(widget, playState) {
	playDevSet(playState)
	nav.x <- T
	nav.y <- T
	on.exit(playPrompt(playState) <- NULL)
	lowEdge <- "bottom-left corner"
	if (!nav.y) lowEdge <- "left edge"
	if (!nav.x) lowEdge <- "bottom edge"
	highEdge <- "top-right corner"
	if (!nav.y) highEdge <- "right edge"
	if (!nav.x) highEdge <- "top edge"
	# get region
	if (playState$is.lattice) {
		newFocus <- playFocus(clip.off=T)
		if (!any(newFocus)) return()
		playPrompt(playState) <- paste("Identifying data points in a region...",
			"click at the", lowEdge)
		xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
		ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
		# get lower limits
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) {
			trellis.unfocus()
			return()
		}
		clickLoc <- lapply(clickLoc, as.numeric)
		xlim.new <- if (nav.x) clickLoc$x else xlim[1]
		ylim.new <- if (nav.y) clickLoc$y else ylim[1]
		# draw lower bounds
		if (nav.x) panel.abline(v=xlim.new)
		if (nav.y) panel.abline(h=ylim.new)
		# get upper limits
		playPrompt(playState) <- paste("OK, now click at the", highEdge)
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) {
			playReplot(playState)
			return()
		}
		clickLoc <- lapply(clickLoc, as.numeric)
		xlim.new[2] <- if (nav.x) clickLoc$x else xlim[2]
		ylim.new[2] <- if (nav.y) clickLoc$y else ylim[2]
		# draw upper bounds
		if (nav.x) panel.abline(v=xlim.new[2])
		if (nav.y) panel.abline(h=ylim.new[2])
		xlim <- unlogX(xlim.new, playState$call)
		ylim <- unlogY(ylim.new, playState$call)
		# set identified points
		if ('x' %in% names(idCall <- playState$id.call)) {
			xy <- xy.coords.call(idCall, playState$env)
			subscripts <- seq_along(xy$x)
		} else {
			pargs <- trellis.panelArgs()
			xy <- xy.coords(pargs, recycle=T)
			# convert back from log scale if required
			xy$x <- unlogX(xy$x, playState$call)
			xy$y <- unlogY(xy$y, playState$call)
			subscripts <- pargs$subscripts
		}
		ids.new <- which(
			(min(xlim.new) < xy$x) & (xy$x < max(xlim.new)) &
			(min(ylim.new) < xy$y) & (xy$y < max(ylim.new))
		)
		ids.new <- subscripts[ids.new]
		myPacket <- as.character(packet.number())
		ids.old <- playState$ids[[myPacket]] # may be NULL
		playState$ids[[myPacket]] <- union(ids.old, ids.new)
	} else {
		# traditional graphics plot
		playPrompt(playState) <- paste("Identifying data points in a region...",
			"click at the", lowEdge)
		xlim <- par("usr")[1:2]
		ylim <- par("usr")[3:4]
		# get lower limits
		clickLoc <- locator(n=1)
		if (is.null(clickLoc)) return()
		xlim.new <- if (nav.x) clickLoc$x else xlim[1]
		ylim.new <- if (nav.y) clickLoc$y else ylim[1]
		if (nav.x) abline(v=xlim.new, col="red")
		if (nav.y) abline(h=ylim.new, col="red")
		# get upper limits
		playPrompt(playState) <- paste("OK, now click at the", highEdge)
		clickLoc <- locator(n=1)
		if (is.null(clickLoc)) {
			return()
		}
		xlim.new[2] <- if (nav.x) clickLoc$x else xlim[2]
		ylim.new[2] <- if (nav.y) clickLoc$y else ylim[2]
		if (nav.x) abline(v=xlim.new[2], col="red")
		if (nav.y) abline(h=ylim.new[2], col="red")
		xlim <- unlogX(xlim.new, playState$call, is.lattice=F)
		ylim <- unlogY(ylim.new, playState$call, is.lattice=F)
		xy <- xy.coords.call(playState$id.call, playState$env)
		ids.new <- which(
			(min(xlim.new) < xy$x) & (xy$x < max(xlim.new)) &
			(min(ylim.new) < xy$y) & (xy$y < max(ylim.new))
		)
		# set identified points
		ids.old <- playState$ids$all # may be NULL
		playState$ids$all <- union(ids.old, ids.new)
	}
	playReplot(playState)
}

identify_postplot_action <- function(widget, playState) {
	# draw persistent labels
	labels <- playState$labels
	if (playState$is.lattice) {
		packets <- trellis.currentLayout(which="packet")
		for (myPacket in names(playState$ids)) {
			whichOne <- which(packets == as.numeric(myPacket))
			if (length(whichOne) == 0) next
			myCol <- col(packets)[whichOne]
			myRow <- row(packets)[whichOne]
			trellis.focus("panel", myCol, myRow, highlight=F)
			# find which points are identified
			ids <- playState$ids[[myPacket]]
			if (!is.null(playState$label.points)) {
				xy <- xy.coords(label.points, recycle=T)
			} else {
				if (length(labels) == 0) labels <- subscripts
				if (length(labels) > length(subscripts))
					labels <- labels[subscripts]
				ids <- which(subscripts %in% ids)
			}
			if (length(labels) > 0) {
				do.call(panel.text, c(list(xy$x[ids], xy$y[ids], 
					labels=labels[ids], pos=1), 
					playState$label.style))
			}
			trellis.unfocus()
		}
	} else {
		# base graphics plot
		ids <- playState$ids$all
		if (length(ids) > 0) {
			if (!is.null(playState$label.points)) {
				xy <- xy.coords(label.points, recycle=T)
			} else {
				xy <- xy.coords.call(playState$call, playState$env)
			}
			if (length(labels) == 0) labels <- seq_along(xy$x)
			if (length(labels) > 0) {
				do.call(text, c(list(xy$x[ids], xy$y[ids], 
					labels=labels[ids], pos=1), 
					playState$label.style))
			}
		}
	}
}

## ANNOTATE

toolConstructors$annotate <- function(playState) {
	quickTool(playState,
		label = "Annotate", 
		icon = "gtk-italic",
		tooltip = "Add your own labels to the plot",
		f = annotate_handler,
		data = list(),
		post.plot.action = annotate_postplot_action)
}

annotate_handler <- function(widget, user.data) {
	playState <- user.data$playState
	isFigure <- (!missing(user.data) && isTRUE(user.data$figure))
	isArrow <- (!missing(user.data) && isTRUE(user.data$arrow))
	on.exit(playPrompt(playState) <- NULL)
	# get location
	myPacket <- "all"
	myPrompt <- "Click to place a label inside the plot region"
	if (isFigure) myPrompt <- "Click to place a label on the figure"
	if (isArrow) myPrompt <- "Click at the start of the arrow (\"from\")"
	nextPrompt <- "OK, now click at the end of the arrow (\"to\")"
	if (playState$is.lattice) {
		if (isFigure) {
			trellis.focus("toplevel", highlight=F)
		} else {
			newFocus <- playFocus(playState)
			if (!any(newFocus)) return()
			myPacket <- as.character(packet.number())
		}
		on.exit(trellis.unfocus(), add=T)
		playPrompt(playState) <- myPrompt
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) return()
		clickLoc <- lapply(clickLoc, as.numeric)
		clickLoc <- lapply(clickLoc, signif, 4)
		if (isArrow) {
			playPrompt(playState) <- nextPrompt
			clickLoc1 <- grid.locator()
			if (is.null(clickLoc1)) return()
			clickLoc1 <- lapply(clickLoc1, as.numeric)
			clickLoc1 <- lapply(clickLoc1, signif, 4)
			theCall <- call('panel.arrows', x0=clickLoc$x, y0=clickLoc$y,
				x1=clickLoc1$x, y1=clickLoc1$y, length=0.15)
		} else {
			myLabel <- placeLabelDialog()
			if (is.null(myLabel)) return()
			myAdj <- switch(as.character(myLabel$align[1]),
				`0`="left", `0.5`="centre", `1`="right")
			myAdj[2] <- switch(as.character(myLabel$align[2]),
				`0`="bottom", `0.5`="centre", `1`="top")
			theCall <- call('panel.text', myLabel$text, 
				x=clickLoc$x, y=clickLoc$y, adj=myAdj)
		}
		# add user-specified default style
		theCall <- as.call(c(as.list(theCall), playState$label.style))
		
	} else {
		# base graphics plot
		playPrompt(playState) <- myPrompt
		if (isFigure) op <- par(usr=rep(0:1,2), xpd=NA, xlog=F, ylog=F)
		clickLoc <- locator(n=1)
		if (is.null(clickLoc)) return()
		clickLoc <- lapply(clickLoc, signif, 4)
		if (isArrow) {
			playPrompt(playState) <- nextPrompt
			clickLoc1 <- locator(n=1)
			if (is.null(clickLoc1)) return()
			clickLoc1 <- lapply(clickLoc1, signif, 4)
			theCall <- call('arrows', x0=clickLoc$x, y0=clickLoc$y,
				x1=clickLoc1$x, y1=clickLoc1$y, length=0.15)
		} else {
			myLabel <- placeLabelDialog()
			if (is.null(myLabel)) return()
			theCall <- call('text', myLabel$text, 
				x=clickLoc$x, y=clickLoc$y, adj=myLabel$align)
		}
		# revert graphical settings
		if (isFigure) par(op)
		# add user-specified default style
		theCall <- as.call(c(as.list(theCall), playState$label.style))
		if (isFigure) {
			theCall <- bquote({
				op <- par(usr=rep(0:1,2), xpd=NA, xlog=F, ylog=F)
				.(theCall)
				par(op)
			})
		}
	}
	playDevSet(playState)
	# add the annotation
	eval(theCall, playState$env)
	playState$annotations[[myPacket]] <- 
		c(playState$annotations[[myPacket]], theCall)
	with(playState$tools, {
		if (!is.null(edit.annotations)) edit.annotations["visible"] <- TRUE
		if (!is.null(clear)) clear["visible"] <- TRUE
	})
}

placeLabelDialog <- function(text="", title="New label", prompt="", width.chars=-1) {
	editBox <- gtkDialog(title=title, NULL, NULL,
		"OK", GtkResponseType["ok"], "Cancel", GtkResponseType["cancel"],
		show = F)
	editBox$setDefaultResponse(GtkResponseType["ok"])
	if (nchar(prompt) > 0) {
		editBox[["vbox"]]$packStart(gtkLabel(prompt), expand=F, pad=2)
	}
	editEntry <- gtkEntry()
	editEntry['activates-default'] <- T
	editEntry['text'] <- text
	editEntry['width-chars'] <- width.chars
	editBox[["vbox"]]$packStart(editEntry, pad=10)
	alignHBox <- gtkHBox()
	alignHBox$packStart(gtkLabel("Position relative to point: "))
	alignTable <- gtkTable(rows=3, columns=3)
	alignRadios <- list(list(),list(),list())
	myGroup <- NULL
	for (col in 1:3) for (row in 1:3) {
		thisRadio <- gtkRadioButtonNewFromWidget(group=myGroup)
		if (is.null(myGroup)) myGroup <- thisRadio
		alignRadios[[col]][[row]] <- thisRadio
		alignTable$attachDefaults(thisRadio,
			left=col-1, right=col, top=row-1, bot=row) # xpadding ypadding 
	}
	alignRadios[[2]][[2]]['active'] <- T
	alignHBox$packStart(alignTable)
	editBox[["vbox"]]$packStart(alignHBox)
	editBox$showAll()
	result <- editBox$run() # make it modal
	newTxt <- editEntry['text']
	newAlign <- c(0,0)
	for (col in 1:3) for (row in 1:3) {
		if (alignRadios[[col]][[row]]['active'])
			newAlign <- c( (3-col)/2, (row-1)/2 )
	}
	editBox$destroy()
	if (result != GtkResponseType["ok"]) return(invisible(NULL))
	list(text=newTxt, align=newAlign)
}

annotate_postplot_action <- function(widget, playState) {
	# draw annotations
	if (playState$is.lattice) {
		packets <- trellis.currentLayout(which="packet")
		for (myPacket in names(playState$annotations)) {
			if (myPacket == "all") {
				trellis.focus("toplevel", highlight=F)
			} else {
				whichOne <- which(packets == as.numeric(myPacket))
				if (length(whichOne) == 0) next
				myCol <- col(packets)[whichOne]
				myRow <- row(packets)[whichOne]
				trellis.focus("panel", myCol, myRow, highlight=F)
			}
			for (expr in playState$annotations[[myPacket]]) {
				eval(expr, playState$env)
			}
			trellis.unfocus()
		}
	} else {
		# base graphics plot
		for (expr in playState$annotations$all) {
			eval(expr, playState$env)
		}
	}
}

## ARROW

toolConstructors$arrow <- function(playState) {
	quickTool(playState,
		label = "Arrow", 
		icon="gtk-connect", 
		tooltip="Add an arrow to the plot",
		f=annotate_handler,
		data=list(arrow=TRUE)
}

## EDIT.ANNOTATIONS

toolConstructors$edit.annotations <- function(playState) {
	quickTool(playState,
		label = "Edit annot.", 
		icon="gtk-edit", 
		tooltip="Edit annotations",
		f=edit.annotations_handler,
		show=length(playState$annotations) > 0)
}

edit.annotations_handler <- function(widget, playState) {
	theAnnots <- playState$annotations$all
	callTxt <- paste(unlist(lapply(theAnnots, deparse, 
		control=c("showAttributes"))), collapse="\n")
	repeat {
		newTxt <- guiTextInput(callTxt, title="Edit annotations", 
			prompt="", accepts.tab=F)
		if (is.null(newTxt)) break
		callTxt <- newTxt
		tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
		# check whether there was a syntax error
		if (inherits(tmp, "error")) {
			errorDialog(paste("Error:", conditionMessage(tmp)))
		} else {
			playState$annotations$all <- tmp
			playReplot(playState)
			break
		}
	}
	playState$win$present()
}

## CLEAR

toolConstructors$clear <- function(playState) {
	quickTool(playState,
		label = "Clear", 
		icon = "gtk-clear", 
		tooltip = "Remove labels and annotations", 
		f = clear_handler)
}

clear_handler <- function(widget, playState) {
	types <- c(
		if (length(playState$ids) > 0) "ids",
		if (length(playState$annotations) > 0) "annotations",
		if (length(playState$brushed) > 0) "brushed"
	)
	if (length(types) == 0) { widget$hide(); return() }
	clear.types <- types
	if (length(types) > 1) {
		clear.types <- select.list(types, preselect = types, multiple = TRUE, 
			title = "Clear what?")
	}
	for (x in clear.types)
		playState[[x]] <- list()
	if (length(clear.types) == length(types)) {
		# everything was cleared
		widget$hide()
	}
	if ("annotations" %in% clear.types) {
		editAnnTool <- playState$tools$edit.annotations
		if (!is.null(editAnnTool)) editAnnTool["visible"] <- FALSE
	}
	playReplot(playState)
}

## ZOOM

toolConstructors$zoom <- function(playState) {
	quickTool(playState,
		label = "Zoom...", 
		icon = "gtk-zoom-in", 
		tooltip = "Select a new plot region with the mouse",
		f = zoom_handler)
}

zoom_handler <- function(widget, playState) {
	nav.x <- TRUE
	nav.y <- !(playState$time.mode)
	on.exit(playPrompt(playState) <- NULL)
	lowEdge <- "bottom-left corner"
	if (!nav.y) lowEdge <- "left edge"
	if (!nav.x) lowEdge <- "bottom edge"
	highEdge <- "top-right corner"
	if (!nav.y) highEdge <- "right edge"
	if (!nav.x) highEdge <- "top edge"
	# set up masking
	maskGrob <- rectGrob(gp=gpar(col="transparent", 
		fill=rgb(0.5,0.5,0.5, alpha=0.25)), name="tmp.mask")
	# get new scales interactively
	if (playState$is.lattice) {
		# lattice plot
		newFocus <- playFocus(playState, clip.off=T)
		if (!any(newFocus)) return()
		# find existing scales
		xlim <- rawXLim(playState)
		ylim <- rawYLim(playState)
		playPrompt(playState) <- paste("Zooming to selected region...",
			"click at the", lowEdge)
		# get lower limits
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) {
			trellis.unfocus()
			return()
		}
		clickLoc <- lapply(clickLoc, as.numeric)
		xlim.new <- if (nav.x) clickLoc$x else xlim[1]
		ylim.new <- if (nav.y) clickLoc$y else ylim[1]
		# mask out lower regions
		grid.draw(editGrob(maskGrob, 
			x=unit(0,"npc"), width=unit(xlim.new[1] - xlim[1],"native"), 
			just="left"))
		grid.draw(editGrob(maskGrob,
			y=unit(0,"npc"), height=unit(ylim.new[1] - ylim[1],"native"),
			x=unit(1,"npc"), width=unit(xlim[2] - xlim.new[1],"native"),
			just=c("right", "bottom")))
		grid.lines(x=unit(xlim.new[1], "native"), gp=gpar(col="red"))
		grid.lines(y=unit(ylim.new[1], "native"), gp=gpar(col="red"))
		# get upper limits
		playPrompt(playState) <- paste("OK, now click at the", highEdge)
		clickLoc <- grid.locator()
		if (is.null(clickLoc)) {
			trellis.unfocus()
			playReplot(playState)
			return()
		}
		clickLoc <- lapply(clickLoc, as.numeric)
		xlim.new[2] <- if (nav.x) clickLoc$x else xlim[2]
		ylim.new[2] <- if (nav.y) clickLoc$y else ylim[2]
		# mask out upper regions
		grid.draw(editGrob(maskGrob, 
			x=unit(1,"npc"), width=unit(xlim[2] - xlim.new[2],"native"), 
			y=unit(1,"npc"), height=unit(ylim[2] - ylim.new[1],"native"),
			just=c("right", "top")))
		grid.draw(editGrob(maskGrob, 
			y=unit(1,"npc"),
			height=unit(ylim[2] - ylim.new[2],"native"),
			x=unit(xlim.new[2],"native"), 
			width=unit(xlim.new[2] - xlim.new[1],"native"),
			just=c("right", "top")))
		grid.lines(x=unit(xlim.new[2], "native"), gp=gpar(col="red"))
		grid.lines(y=unit(ylim.new[2], "native"), gp=gpar(col="red"))
	} else {
		# traditional graphics plot
		xlim <- rawXLim(playState)
		ylim <- rawYLim(playState)
		playPrompt(playState) <- paste("Zooming to selected region...",
			"click at the", lowEdge)
		# get lower limits
		clickLoc <- locator(n=1)
		if (is.null(clickLoc)) return()
		xlim.new <- if (nav.x) clickLoc$x else xlim[1]
		ylim.new <- if (nav.y) clickLoc$y else ylim[1]
		opar <- par(xpd=NA)
		if (nav.x) abline(v=xlim.new, col="red")
		if (nav.y) abline(h=ylim.new, col="red")
		# get upper limits
		playPrompt(playState) <- paste("OK, now click at the", highEdge)
		clickLoc <- locator(n=1)
		if (is.null(clickLoc)) {
			par(opar)
			return()
		}
		xlim.new[2] <- if (nav.x) clickLoc$x else xlim[2]
		ylim.new[2] <- if (nav.y) clickLoc$y else ylim[2]
		if (nav.x) abline(v=xlim.new[2], col="red")
		if (nav.y) abline(h=ylim.new[2], col="red")
		par(opar)
	}
	# this converts from raw numeric to original format (including unlog)
	if (nav.x) rawXLim(playState) <- xlim.new
	if (nav.y) rawYLim(playState) <- ylim.new
	playReplot(playState)
}

## ZOOMOUT

toolConstructors$zoomout <- function(playState) {
	quickTool(playState,
		label = "Zoom out", 
		icon = "gtk-zoom-out", 
		f = zoomout_handler)
}

zoomout_handler <- function(widget, playState) {
	nav.x <- TRUE
	nav.y <- !(playState$time.mode)
	# find existing scales
	xlim <- rawXLim(playState)
	ylim <- rawYLim(playState)
	# zoom out: make range twice the size
	if (nav.x) xlim <- xlim + diff(xlim) * c(-0.5, 0.5)
	if (nav.y) ylim <- ylim + diff(ylim) * c(-0.5, 0.5)
	# this converts from raw numeric to original format (including unlog)
	if (nav.x) rawXLim(playState) <- xlim
	if (nav.y) rawYLim(playState) <- ylim
	playReplot(playState)
}

## ZOOMFIT

toolConstructors$zoomfit <- function(playState) {
	quickTool(playState,
		label = "Fit data",
		icon = "gtk-zoom-fit", 
		f = zoomfit_handler,
		post.plot.action = zoomfit_postplot_action)
}

zoomfit_handler <- function(widget, playState) {
	nav.x <- TRUE
	nav.y <- !(playState$time.mode)
	# update scales
	if (nav.x) callArg(playState, xlim) <- NULL
	if (nav.y) callArg(playState, ylim) <- NULL
	playReplot(playState)
}

zoomfit_postplot_action <- function(widget, playState) {
	nav.x <- TRUE
	nav.y <- !(playState$time.mode)
	nonfit <- FALSE
	if (nav.x && !is.null(callArg(playState, xlim))) nonfit <- TRUE
	if (nav.y && !is.null(callArg(playState, ylim))) nonfit <- TRUE
	widget["visible"] <- nonfit
}

## ZERO

toolConstructors$zero <- function(playState) {
	quickTool(playState,
		label = "Full scale", 
		icon = "gtk-goto-bottom", 
		tooltip = "Show the full scale starting from zero", 
		f = zero_handler,
		post.plot.action = zero_postplot_action)
}

zero_handler <- function(widget, playState) {
	trans.x <- !(playState$time.mode)
	trans.y <- TRUE
	if (trans.x) {
		xlim <- rawXLim(playState)
		if (min(xlim) > 0) {
			xlim[which.min(xlim)] <- 0 - 0.07 * max(abs(xlim))
		} else if (max(xlim) < 0) {
			xlim[which.max(xlim)] <- 0 + 0.07 * max(abs(xlim))
		}
		callArg(playState, xlim) <- signif(xlim, 4)
	}
	if (trans.y) {
		ylim <- rawYLim(playState)
		if (min(ylim) > 0) {
			ylim[which.min(ylim)] <- 0 - 0.07 * max(abs(ylim))
		} else if (max(ylim) < 0) {
			ylim[which.max(ylim)] <- 0 + 0.07 * max(abs(ylim))
		}
		callArg(playState, ylim) <- signif(ylim, 4)
	}
	playReplot(playState)
}

zero_postplot_action <- function(widget, playState) {
	trans.x <- !(playState$time.mode)
	trans.y <- TRUE
	nonzero <- FALSE
	if (trans.x) {
		xlim <- rawXLim(playState)
		if (min(xlim) > 0) nonzero <- TRUE
		if (max(xlim) < 0) nonzero <- TRUE
	}
	if (trans.y) {
		ylim <- rawYLim(playState)
		if (min(ylim) > 0) nonzero <- TRUE
		if (max(ylim) < 0) nonzero <- TRUE
	}
	widget["visible"] <- nonzero
}

# HELP

toolConstructors$help <- function(playState) {
	quickTool(playState,
		label = "Help", 
		icon = "gtk-dialog-question", 
		tooltip = "Show help page for this plot function", 
		f = help_handler)
}

help_handler <- function(widget, playState) {
	if (!is.symbol(playState$call[[1]])) {
		errorDialog("Do not know the name of the plot function.")
		return()
	}
	callName <- deparse(playState$call[[1]])
	# work out which (S3) method was called, if any
	methNames <- methods(callName)
	if ((length(methNames) > 0) && length(playState$call > 1)) {
		firstArg <- playState$call[[2]]
		if (is.symbol(firstArg) || (is.call(firstArg) &&
			firstArg[[1]] == quote(`~`))) {
			myClass <- class(callArg(playState, 1))
			myMeth <- paste(callName, myClass, sep=".")
			ok <- (myMeth %in% methNames)
			if (any(ok)) callName <- myMeth[ok][1]
		}
	}
	print(help(callName))
}
	
# EDIT.CALL

edit.call.inline_handler <- function(widget, playState) {
	# the original call -- this should match the code in plotAndPlayUpdate!
	callTxt <- deparseOneLine(playState$call, control="showAttributes")
	newTxt <- widget["text"]
	if (identical(newTxt, callTxt)) return()
	if (identical(newTxt, "")) return()
	tmp <- tryCatch(parse(text=newTxt), error=function(e)e)
	# check whether there was a syntax error
	if (inherits(tmp, "error")) {
		errorDialog(paste("Error:", conditionMessage(tmp)))
	} else {
		# if more than one call, wrap them in braces
		playState$call <- if (length(tmp) > 1)
			as.call(c(as.symbol("{"), tmp)) else tmp[[1]]
		playInitPlot(playState)
		playReplot(playState)
	}
	playState$win$present()
}

edit.call_handler <- function(widget, playState) {
	theCall <- playState$call
	callTxt <- paste(deparse(theCall, control=c("showAttributes")), 
		collapse="\n")
	repeat {
		newTxt <- guiTextInput(callTxt, title="Edit plot call", 
			prompt="", accepts.tab=F)
		if (is.null(newTxt)) break
		callTxt <- newTxt
		tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
		# check whether there was a syntax error
		if (inherits(tmp, "error")) {
			errorDialog(paste("Error:", conditionMessage(tmp)))
		} else {
			# if more than one call, wrap them in braces
			playState$call <- if (length(tmp) > 1)
				as.call(c(as.symbol("{"), tmp)) else tmp[[1]]
			playInitPlot(playState)
			playReplot(playState)
			break
		}
	}
	playState$win$present()
}

## BRUSH

toolConstructors$brush <- function(playState) {
	quickTool(playState,
		label = "Brush points", 
		icon = "gtk-media-record", 
		f = brush_handler,
		post.plot.action = brush_postplot_action)
}
toolConstructors$brush.region <- function(playState) {
	quickTool(playState,
		label = "Brush region", 
		icon = "gtk-media-record", 
		f = brush.region_handler)
}
toolConstructors$brush.drag <- function(playState) {
	quickTool(playState,
		label = "Brush region (drag)",
		icon = "gtk-media-record", 
		f = brush.drag_handler)
}

brush_handler <- function(widget, playState) {
	if (!playState$is.lattice) {
		errorDialog("Brushing only works with lattice::splom")
		return()
	}
	# do brushing
	newFocus <- playFocus(playState)
	if (!any(newFocus)) return()
	playPrompt(playState) <- paste("Brushing data points...",
		"Click the right mouse button to finish.")
	brushed.new <- panel.brush.splom()
	playPrompt(playState) <- NULL
	myPacket <- as.character(packet.number())
	brushed.old <- playState$brushed[[myPacket]]
	if (!is.null(brushed.old)) brushed.new <- union(brushed.new, brushed.old)
	playState$brushed[[myPacket]] <- brushed.new
	trellis.unfocus()
	with(playState$tools, {
		if (!is.null(clear)) clear["visible"] <- TRUE
	})
}

brush.drag_handler <- function(widget, playState) {
	if (!playState$is.lattice) {
		errorDialog("Brushing only works with lattice::splom")
		return()
	}
	# do brushing
	newFocus <- playFocus(playState)
	if (!any(newFocus)) return()
	playPrompt(playState) <- paste("Brushing data points in a region...",
		"click and drag!")
	pargs <- trellis.panelArgs()
	nvars <- length(pargs$z)
	devicePos <- getGraphicsEvent(prompt="",
		onMouseDown=function(buttons, x, y) {
			list(x=x, y=y)
		}
	)
	## which subpanel
	panelPos <- deviceNPCToVp(devicePos, unit="npc", valueOnly=T)
	colpos <- ceiling(panelPos$x * nvars)
	rowpos <- ceiling(panelPos$y * nvars)
	if (rowpos == colpos) {
		trellis.unfocus()
		return()
	}
	subpanel.name <- paste("subpanel", colpos, rowpos, sep = ".")
	## get to that viewport, so we can convert units
	depth <- downViewport(subpanel.name)
	## coordinates of click in subpanel
	startPos <- deviceNPCToVp(devicePos, unit="native", valueOnly=T)
	datax <- pargs$z[, colpos]
	datay <- pargs$z[, rowpos]
	
	playState$tmp.brushed <- F
	getGraphicsEvent(prompt="",
		onMouseMove=function(buttons, x, y) {
			#if (length(buttons)==0) return(TRUE) # 'buttons' unimplemented?
			nowPos <- deviceNPCToVp(c(x, y), unit="native", valueOnly=T)
			xx <- c(startPos$x, nowPos$x)
			yy <- c(startPos$y, nowPos$y)
			brushed <- (
				(min(xx) < datax) & (datax < max(xx)) &
				(min(yy) < datay) & (datay < max(yy))
			)
			brushed.new <- brushed & !playState$tmp.brushed
			playState$tmp.brushed <- brushed |
				playState$tmp.brushed
			panel.points(datax[brushed.new], datay[brushed.new], 
				col='black', pch=16)
			NULL
		},
		onMouseUp=function(buttons, x, y) TRUE,
		onMouseDown=function(buttons, x, y) TRUE
	)
	upViewport(depth)
	brushed.new <- which(playState$tmp.brushed)
	splom.drawBrushed(brushed.new)
	playState$tmp.brushed <- NULL
	if (!is.null(pargs$subscripts)) {
		brushed.new <- pargs$subscripts[brushed.new]
	}
	myPacket <- as.character(packet.number())
	brushed.old <- playState$brushed[[myPacket]]
	if (!is.null(brushed.old)) brushed.new <- union(brushed.new, brushed.old)
	playState$brushed[[myPacket]] <- brushed.new
	trellis.unfocus()
	with(playState$tools, {
		if (!is.null(clear)) clear["visible"] <- TRUE
	})
}

brush.region_handler <- function(widget, playState) {
	nav.x <- TRUE
	nav.y <- !(playState$time.mode)
	if (!playState$is.lattice) {
		errorDialog("Brushing only works with lattice::splom")
		return()
	}
	on.exit(playPrompt(playState) <- NULL)
	on.exit(trellis.unfocus(), add=T)
	lowEdge <- "bottom-left corner"
	if (!nav.y) lowEdge <- "left edge"
	if (!nav.x) lowEdge <- "bottom edge"
	highEdge <- "top-right corner"
	if (!nav.y) highEdge <- "right edge"
	if (!nav.x) highEdge <- "top edge"
	# set up masking
	maskGrob <- rectGrob(gp=gpar(col="grey", 
		fill=rgb(0.5,0.5,0.5, alpha=0.5)), name="tmp.mask")
	# do brushing
	newFocus <- playFocus(playState)
	if (!any(newFocus)) return()
	playPrompt(playState) <- paste("Brushing data points in a region...",
		"click at the", lowEdge)
	ll <- grid.locator(unit = "npc")
	if (is.null(ll)) return()
	pargs <- trellis.panelArgs()
	nvars <- length(pargs$z)
	## which subpanel
	colpos <- ceiling(convertUnit(ll$x, "npc", valueOnly = TRUE) * nvars)
	rowpos <- ceiling(convertUnit(ll$y, "npc", valueOnly = TRUE) * nvars)
	if (rowpos == colpos) return()
	subpanel.name <- paste("subpanel", colpos, rowpos, sep = ".")
	## coordinates of click in subpanel
	ll$x <- nvars * (ll$x - unit((colpos-1) / nvars, "npc"))
	ll$y <- nvars * (ll$y - unit((rowpos-1) / nvars, "npc"))
	## get to that viewport, so we can convert units
	depth <- downViewport(subpanel.name)
	xlim.new <- convertX(ll$x, "native", TRUE)
	ylim.new <- convertY(ll$y, "native", TRUE)
	# draw lower bounds
	if (nav.x) panel.abline(v=xlim.new)
	if (nav.y) panel.abline(h=ylim.new)
	# get upper bounds
	playPrompt(playState) <- paste("OK, now click at the", highEdge)
	ll <- grid.locator(unit = "npc")
	if (is.null(ll)) {
		playReplot(playState)
		return()
	}
	ll$x <- nvars * (ll$x - unit((colpos-1) / nvars, "npc"))
	ll$y <- nvars * (ll$y - unit((rowpos-1) / nvars, "npc"))
	xlim.new[2] <- convertX(ll$x, "native", TRUE)
	ylim.new[2] <- convertY(ll$y, "native", TRUE)
	# draw upper bounds
	if (nav.x) panel.abline(v=xlim.new[2])
	if (nav.y) panel.abline(h=ylim.new[2])
	datax <- pargs$z[, colpos]
	datay <- pargs$z[, rowpos]
	brushed.new <- which(
		(min(xlim.new) < datax) & (datax < max(xlim.new)) &
		(min(ylim.new) < datay) & (datay < max(ylim.new))
	)
	if (!is.null(pargs$subscripts)) {
		brushed.new <- pargs$subscripts[brushed.new]
	}
	myPacket <- as.character(packet.number())
	brushed.old <- playState$brushed[[myPacket]]
	if (!is.null(brushed.old)) brushed.new <- union(brushed.new, brushed.old)
	playState$brushed[[myPacket]] <- brushed.new
	if (!is.null(playState$tools$clear)) 
		playState$tools$clear["visible"] <- TRUE
	playReplot(playState)
}

brush_postplot_action <- function(widget, playState) {
	if (playState$is.lattice) {
		packets <- trellis.currentLayout(which="packet")
		# draw persistent brushing
		for (myPacket in names(playState$brushed)) {
			whichOne <- which(packets == as.numeric(myPacket))
			if (length(whichOne) == 0) next
			myCol <- col(packets)[whichOne]
			myRow <- row(packets)[whichOne]
			trellis.focus("panel", myCol, myRow, highlight=F)
			# find which points are identified
			pargs <- trellis.panelArgs()
			ids <- playState$brushed[[myPacket]]
			# next line same as: which(pargs$subscripts %in% ids)
			# TODO: ok?
			ids.sub <- findInterval(ids, pargs$subscripts)
			splom.drawBrushed(ids.sub)
			trellis.unfocus()
		}
	}
}

splom.drawBrushed <- function(ids, pargs=trellis.panelArgs(), threshold=18, col='black', pch=16, cex=1, ...) {
	nvars <- length(pargs$z)
	for (row in 1:nvars)
        for (column in 1:nvars)
            if (row != column)
            {
                subpanel.name <-
                    paste("subpanel",
                          column, row, sep = ".")
                depth <- downViewport(subpanel.name)
                panel.points(x = pargs$z[ids, column],
                             y = pargs$z[ids, row],
                             pch = pch, col = col, cex = cex,
                             ...)
                upViewport(depth)
            }
}

## NAV 3D

toolConstructors$zoomin.3d <- function(playState) {
	if (is.null(callArg(playState, zoom))) 
		callArg(playState, zoom) <- 1
	quickTool(playState,
		label = "Zoom in", 
		icon = "gtk-zoom-in", 
		f = zoomin3d_handler)
}

toolConstructors$zoomout.3d <- function(playState) {
	quickTool(playState,
		label = "Zoom out", 
		icon = "gtk-zoom-out", 
		f = zoomout3d_handler)
}

toolConstructors$fly.left.3d <- function(playState) {
	if (is.null(callArg(playState, screen))) 
		callArg(playState, screen) <- quote(list(z=40, x=-60))
	quickTool(playState,
		label = "Fly left", 
		icon = "gtk-media-rewind-ltr", 
		f = flyleft3d_handler)
}

toolConstructors$fly.right.3d <- function(playState) {
	quickTool(playState,
		label = "Fly right", 
		icon = "gtk-media-rewind-rtl", 
		f = flyright3d_handler)
}

zoomin3d_handler <- function(widget, playState) {
	zoom <- callArg(playState, zoom)
	callArg(playState, zoom) <- signif(zoom * 1.5, 4)
	playReplot(playState)
}

zoomout3d_handler <- function(widget, playState) {
	zoom <- callArg(playState, zoom)
	callArg(playState, zoom) <- signif(zoom / 1.5, 4)
	playReplot(playState)
}

flyleft3d_handler <- function(widget, playState) {
	screen <- callArg(playState, screen)
	if (names(screen)[1] == 'z') screen[[1]] <- screen[[1]] + 45
	else screen <- c(z = 45, screen)
	# convert list to call so that deparse is pretty
	callArg(playState, screen) <- as.call(c(quote(list), screen))
	playReplot(playState)
}

flyright3d_handler <- function(widget, playState) {
	screen <- callArg(playState, screen)
	if (names(screen)[1] == 'z') screen[[1]] <- screen[[1]] - 45
	else screen <- c(z = -45, screen)
	# convert list to call so that deparse is pretty
	callArg(playState, screen) <- as.call(c(quote(list), screen))
	playReplot(playState)
}

