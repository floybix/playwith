## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

## Tools

playApplicationTools <- list(
	"options",
	"stayontop",
	"keep",
	"save",
	"copy",
	"print",
	"data",
	"--",
	"settings",
	"theme",
	"---",
	"time.mode"
)

playInteractionTools <- list(
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
	"zero",
	"---",
	"coords"
)

play3DTools <- list(
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
	"expand",
	"annotate",
	"arrow",
	"edit.annotations",
	"clear",
	"--",
	"brush"
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
	f = NULL, 
	data = NULL, 
	post.plot.action = NULL,
	isToggle = FALSE, 
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
	if (!is.null(f)) {
		if (is.null(data)) data <- playState
		else data$playState <- playState
		gSignalConnect(x, "clicked", f, data=data)
	}
	if (!is.null(post.plot.action))
		gObjectSetData(x, "post.plot.action", data=post.plot.action)
	x
}

## SETTINGS

toolConstructors$settings <- function(playState) {
	callFun <- eval(playState$call[[1]])
	if ((typeof(callFun) != "closure") || is.null(formals(callFun))) {
		# call does not take arguments
		return(NA)
	}
	quickTool(playState,
		label = "Plot settings",  # or "Edit plot"?
		icon = "gtk-preferences", 
		tooltip = "Change the plot type and settings",
		f = settings_handler)
}
# note: settings_handler can be found in plotSettingsGui.R

## KEEP

toolConstructors$keep <- function(playState) {
	widget <- quickTool(playState,
		label = "Keep plot", 
		icon = "gtk-media-stop", 
		tooltip = "Keep this window, do not replace it (open next plot in a new window)",
		f = keep_handler,
		isToggle = TRUE)
	if (isTRUE(playState$keep)) widget["active"] <- TRUE
	widget
}

keep_handler <- function(widget, playState) {
	playState$keep <- widget["active"]
}

## STAYONTOP

toolConstructors$stayontop <- function(playState) {
	widget <- quickTool(playState,
		label = "Stay on top", 
		icon = "gtk-leave-fullscreen", 
		tooltip = "Always show this window",
		f = stayontop_handler,
		isToggle = TRUE)
	if (isTRUE(playState$stay.on.top)) widget["active"] <- TRUE
	widget
}

stayontop_handler <- function(widget, playState) {
	playState$stay.on.top <- widget["active"]
	playState$win$setKeepAbove(widget["active"])
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

save_handler <- function(widget, user.data) {
	playState <- user.data$playState
	# disable toolbars until this is over
	playFreezeGUI(playState)
	on.exit(playThawGUI(playState))
	# get filename
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
	# save plot to file
	playDevSet(playState)
	# note: baseViewports will be corrupted if device size changes
	# so need to keep the same size with dev.copy()...
	da <- playState$widgets$drawingArea
	w.px <- da$getAllocation()$width
	h.px <- da$getAllocation()$height
	# assuming 96 d.p.i. in both dimensions
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

## COPY

toolConstructors$copy <- function(playState) {
	quickTool(playState,
		label = "Copy", 
		icon = "gtk-copy", 
		tooltip = "Copy this plot to the clipboard (as a bitmap)", 
		f = copy_handler)
}

copy_handler <- function(widget, playState) {
	# TODO: option to copy as bitmap or WMF
	# disable toolbars until this is over
	playFreezeGUI(playState)
	on.exit(playThawGUI(playState))
	# save plot to file
	filename <- paste(tempfile(), ".png", sep="")
	playDevSet(playState)
	da <- playState$widgets$drawingArea
	w.px <- da$getAllocation()$width
	h.px <- da$getAllocation()$height
	w.in <- w.px / 96
	h.in <- h.px / 96
	dev.copy(Cairo_png, file=filename, width=w.in, height=h.in)
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
	# TODO: print at current size, otherwise figure annotations might move!
	isWindows <- (.Platform$OS.type == "windows")
	if (isWindows) dev.print(win.print)
	else dev.print()
}

## DATA

toolConstructors$data <- function(playState) {
	quickTool(playState,
		label = "Data...", 
		icon = "gtk-cdrom", 
		tooltip = "View and edit attached data objects",
		f = data_handler,
		show = length(playState$env) > 0)
}

data_handler <- function(widget, playState) {
	lstObjects <- function(envir= .GlobalEnv) {
		objlist <- ls(envir=envir)
		objclass <- sapply(objlist, function(objName) {
			obj <- get(objName, envir=envir)
			class(obj)[1]
		})
		data.frame(Name = I(objlist), Class = I(objclass))
	}
	browseEnv2 = function(envir = .GlobalEnv) {
		listOfObjects <- lstObjects(envir=envir)
		gtable(listOfObjects, container = gwindow("Data objects (double-click to edit)"),
		handler = function(h,...) {
			myName <- svalue(h$obj)
			oldObj <- get(myName, envir=envir)
			newObj <- edit(oldObj)
			if (identical(newObj, oldObj)) return()
			assign(myName, newObj, envir=envir)
			gmessage(paste("Edited object", myName, 
				"-- you might want to reload the plot."))
		})
	}
	browseEnv2(playState$env)
}

## THEME

toolConstructors$theme <- function(playState) {
	if (!playState$is.lattice) return(NA)
	myMenu <- gtkMenu()
	myLabel <- gtkMenuItem("Lattice theme:")
	myLabel["sensitive"] <- FALSE
	myMenu$append(myLabel)
	themeItems <- list(
		whitebg=gtkMenuItem(label="White BG (general)"),
		standard.screen=gtkMenuItem(label="Dark BG (for screen)"),
		standard.print=gtkMenuItem(label="Greyscale (for print)")
	)
	for (x in names(themeItems)) {
		myMenu$append(themeItems[[x]])
		gSignalConnect(themeItems[[x]], "activate", theme_handler, 
			data=list(playState=playState, theme=x))
	}
	widget <- quickTool(playState,
		label = "Theme", 
		icon = "gtk-select-color", 
		tooltip = "Choose Lattice theme"
	)
	# attach the menu
	gSignalConnect(widget, "clicked", 
		function(widget, menu) {
			menu$popup(button=0, activate.time=gtkGetCurrentEventTime())
		}, data=myMenu)
	widget
}

theme_handler <- function(widget, user.data) {
	playState <- user.data$playState
	switch(user.data$theme,
		whitebg=trellis.par.set(theme=col.whitebg()),
		standard.screen=trellis.par.set(theme=standard.theme("X11")),
		standard.print=trellis.par.set(theme=standard.theme("postscript"))
	)
	playReplot(playState)
}

## COORDS

toolConstructors$coords <- function(playState) {
	coordsLabel <- gtkLabel()
	playState$widgets$coordsLabel <- coordsLabel
	if (is.null(playState$widgets$plotClickEventSig)) {
		playState$widgets$plotClickEventSig <- 
			gSignalConnect(playState$widgets$drawingArea, 
			"button-press-event", coords_click_handler, data=playState)
	}
	widget <- gtkToolItem()
	widget$add(coordsLabel)
	widget
}

coords_click_handler <- function(widget, event, playState) {
	if (playState$.need.reconfig) generateSpaces(playState)
	x <- event$x
	y <- event$y
	coordsTxt <- ""
	space <- whichSpace(playState, x, y)
	if (space != "page") {
		xy <- deviceCoordsToSpace(playState, x, y, space=space)
		xyx <- format(xy$x, nsmall=4)
		xyy <- format(xy$y, nsmall=4)
		xyx <- substr(xyx, 1, 4)
		xyy <- substr(xyy, 1, 4)
		coordsTxt <- paste("<tt>x ", xyx, "\ny ", xyy, "</tt>", sep="")
	}
	playState$widgets$coordsLabel$setMarkup(coordsTxt)
	return(FALSE)
}

## TIME.MODE

toolConstructors$time.mode <- function(playState) {
	with (playState$widgets, {
		timeScrollbar["sensitive"] <- playState$time.mode
		timeEntry["sensitive"] <- playState$time.mode
	})
	if (!is.null(playState$index.time)) {
		if (is.null(playState$env$cur.index)) {
			playState$env$cur.index <- 
				if (!is.null(playState$cur.index))
					playState$cur.index else 1
		} # TODO else if (!is.null(playState$env$cur.time)) 
		playState$env$cur.time <- playState$index.time[
			playState$env$cur.index]
	}
	else {
		# x-axis scrolling mode
		callFun <- eval(playState$call[[1]])
		if ((typeof(callFun) != "closure") || is.null(formals(callFun))) {
			# call does not take arguments
			return(NA)
		}
	}
	quickTool(playState,
		label = "Time mode", 
		icon = "gtk-media-forward-ltr",
		tooltip = "Time mode: scroll along the x-axis",
		f = time.mode_handler, 
		post.plot.action = time.mode_postplot_action,
		isToggle = TRUE)
}

time.mode_handler <- function(widget, playState) {
	playState$time.mode <- widget["active"]
	blockRedraws(with (playState$widgets, {
		timeScrollBox["visible"] <- TRUE
		timeScrollbar["sensitive"] <- playState$time.mode
		timeEntry["sensitive"] <- playState$time.mode
	}))
	# store data range and class
	if (playState$time.mode) {
		if (is.null(playState$index.time)) {
			xy <- xyData(playState, space="page")
			playState$time.mode.x.range <- extendrange(as.numeric(xy$x))
			playState$time.mode.x.attr <- attributes(xy$x)
		}
	}
	# update scrollbar etc
	time.mode_postplot_action(widget, playState)
}

time.mode_postplot_action <- function(widget, playState) {
	if (playState$time.mode == FALSE) return()
	if (widget["active"] == FALSE) {
		widget["active"] <- TRUE # triggers update
		return()
	}
	blockRedraws({
		widg <- playState$widgets
		if (!is.null(playState$index.time)) {
			x.pos <- playState$env$cur.index
			x.max <- length(playState$index.time)
			x.jump <- round(log2(x.max))
			cur.time <- playState$env$cur.time
			widg$timeEntry["text"] <- toString(cur.time)
			widg$timeScrollbar["adjustment"] <- gtkAdjustment(
				value=x.pos, lower=1, upper=x.max+1,
				step.incr=1, page.incr=x.jump, page.size=1)
			widg$timeScrollbar$setValue(x.pos) # need this (bug?)
			return()
		}
		x.range <- playState$time.mode.x.range
		x.lim <- rawXLim(playState)
		x.page <- abs(diff(x.lim))
		x.page <- min(x.page, abs(diff(x.range)))
		x.pos <- min(x.lim)
		x.pos <- max(x.pos, min(x.range))
		x.pos <- min(x.pos, max(x.range))
		# format x limits for text box
		xlim <- signif(x.lim, 4)
		mostattributes(x.lim) <- playState$time.mode.x.attr
		widg$timeEntry["text"] <- paste(format(x.lim), collapse=" to ")
		# set up scrollbar
		widg$timeScrollbar["adjustment"] <- gtkAdjustment(
			value=x.pos, lower=min(x.range), upper=max(x.range),
			step.incr=x.page/2, page.incr=x.page, page.size=x.page)
		widg$timeScrollbar$setValue(x.pos) # need this (bug?)
	})
}

time.mode_scrollbar_handler <- function(widget, playState) {
	newLim <- widget$getValue()
	if (!is.null(playState$index.time)) {
		newLim <- round(newLim)
		playState$env$cur.index <- newLim
		playState$env$cur.time <- playState$index.time[newLim]
		playReplot(playState)
		return()
	}
	newLim[2] <- newLim + widget["adjustment"]["page-size"]
	if (widget["adjustment"]["page-size"] == 0) stop()
	#oldLim <- rawXLim(playState)
	#if (min(oldLim) == min(newLim)) return()
	newLim <- signif(newLim, 4)
	rawXLim(playState) <- newLim
	playReplot(playState)
}

time.mode_entry_handler <- function(widget, playState) {
	if (!is.null(playState$index.time)) {
		newLim <- widget["text"]
		index.time <- playState$index.time
		max.x <- length(index.time)
		cls <- class(index.time)
		if ("POSIXt" %in% cls) newLim <- try(as.POSIXct(newLim))
		else if ("Date" %in% cls) newLim <- try(as.Date(newLim))
		if (inherits(newLim, "try-error")) {
			# treat it as an index into index.time
			cur.index <- as.integer(widget["text"])
		} else {
			newLim <- as.numeric(newLim)
			cur.index <- findInterval(newLim, index.time)
		}
		cur.index <- max(1, min(max.x, cur.index))
		playState$env$cur.index <- cur.index
		playState$env$cur.time <- index.time[cur.index]
		playReplot(playState)
		return()
	}
	newLim <- strsplit(widget["text"], " to ")[[1]]
	if ((length(newLim) != 2)) {
		gmessage.error("Give bounds in form \"LOWER to UPPER\".")
		return()
	}
	x.attr <- playState$time.mode.x.attr
	cls <- x.attr$class
	if ("POSIXt" %in% cls) newLim <- as.POSIXct(newLim)
	else if ("Date" %in% cls) newLim <- as.Date(newLim)
	else if ("integer" %in% cls) newLim <- as.integer(newLim)
	callArg(playState, xlim) <- as.numeric(newLim)
	playReplot(playState)
}

## OPTIONS

toolConstructors$options <- function(playState) {
	myButton <- gtkButton("Options...")
	myMenu <- gtkMenu()
	
	# OPTIONS: set label style
	labelStyleItem <- gtkMenuItem("Set label style...")
	myMenu$append(labelStyleItem)
	gSignalConnect(labelStyleItem, "activate", set.label.style_handler,
		data=playState)
	myMenu$append(gtkSeparatorMenuItem())
	
	# OPTIONS: annotation mode
	myLabel <- gtkMenuItem("Annotation mode:")
	myLabel["sensitive"] <- FALSE
	myMenu$append(myLabel)
	annModeItems <- list(
		plot=gtkCheckMenuItem(label="Place on plot (relative)"),
		page=gtkCheckMenuItem(label="Place on page (absolute)")
	)
	pageAnnotation <- (identical(playState$annotation.mode, "page"))
	annModeItems$plot["active"] <- !pageAnnotation
	annModeItems$page["active"] <- pageAnnotation
	annotationModeHandler <- function(widget, user.data) {
		if (widget["active"] == FALSE) return()
		playState <- user.data$playState
		playState$annotation.mode <- user.data$mode
		# hack because gtkRadioMenuItem is broken
		for (x in annModeItems) {
			if (!(x == widget)) x["active"] <- FALSE
		}
	}
	for (x in names(annModeItems)) {
		myMenu$append(annModeItems[[x]])
		gSignalConnect(annModeItems[[x]], "activate", annotationModeHandler, 
			data=list(playState=playState, mode=x))
	}
	myMenu$append(gtkSeparatorMenuItem())
	
	# OPTIONS: clip annotations
	clipItem <- gtkCheckMenuItem("Clip annotations")
	clipItem["active"] <- !identical(playState$clip.annotations, FALSE)
	myMenu$append(clipItem)
	gSignalConnect(clipItem, "activate", function(widget, playState)
	playState$clip.annotations <- widget["active"], data=playState)
	myMenu$append(gtkSeparatorMenuItem())
	
	# OPTIONS: toolbar style
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
		if (widget["active"] == FALSE) return()
		playState <- user.data$playState
		newStyle <- user.data$style
		blockRedraws(with (playState$widgets, {
			hStyle <- if (newStyle == "both-horiz") "both" else newStyle
			topToolbar["toolbar-style"] <- GtkToolbarStyle[hStyle]
			leftToolbar["toolbar-style"] <- GtkToolbarStyle[newStyle]
			bottomToolbar["toolbar-style"] <- GtkToolbarStyle[hStyle]
			rightToolbar["toolbar-style"] <- GtkToolbarStyle[newStyle]
		}))
		# hack because gtkRadioMenuItem is broken
		for (x in styleItems) {
			if (!(x == widget)) x["active"] <- FALSE
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

set.label.style_handler <- function(widget, playState) {
	style <- playState$label.style
	if (is.null(playState$label.style)) {
		style <- do.call(gpar, trellis.par.get("add.text"))
	}
	if (inherits(style, "gpar")) 
		style <- as.call(c(quote(gpar), style))
	
	callTxt <- deparseOneLine(style)
	
	repeat {
		newTxt <- NA#ginput("Edit label style", text=callTxt, width=60)
		editbox <- gedit(callTxt, width=120, container=ggroup())
		gbasicdialog("Edit label style", widget=editbox, action=environment(), 
			handler=function(h, ...) {
				h$action$newTxt <- svalue(editbox)
			})
		if (is.na(newTxt)) break
		if (newTxt == "") break
		if (identical(newTxt, callTxt)) break
		callTxt <- newTxt
		tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
		# check whether there was a syntax error
		if (inherits(tmp, "error")) {
			gmessage.error(conditionMessage(tmp))
		} else {
			playState$label.style <- eval(tmp)
			break
		}
	}
	playState$win$present()
}

## EXPAND

toolConstructors$expand <- function(playState) {
	if (!playState$is.lattice) return(NA)
	quickTool(playState,
		label = "Panel", 
		icon = "gtk-fullscreen", 
		tooltip = "Choose a panel to expand and focus (for further interaction)", 
		f = expand_handler, 
		post.plot.action = expand_postplot_action,
		show = FALSE,
		isToggle = T)
}

expand_handler <- function(widget, playState) {
	playDevSet(playState)
	# check new expanded setting
	if (widget["active"]) {
		playPrompt(playState, 
			"Click on a panel to expand. (Right-click to cancel)")
		on.exit(playPrompt(playState, NULL))
		newFocus <- trellis.focus()
		if (!any(newFocus)) {
			widget["active"] <- FALSE
			return()
		}
		playState$.old.call.layout <- playState$call$layout
		playState$call$layout <- c(0,1,1)
		playState$.old.page <- playState$page
		playState$page <- packet.number()
	} else {
		if (is.null(playState$.old.page)) return()
		playState$call$layout <- playState$.old.call.layout
		playState$page <- playState$.old.page
		rm(.old.call.layout, .old.page, envir=playState)
	}
	playReplot(playState)
}

expand_postplot_action <- function(widget, playState) {
	widget["visible"] <- (widget["active"] ||
		(length(trellis.currentLayout()) > 1))
}

## IDENTIFY

toolConstructors$identify <- function(playState) {
	if (is.null(playState$data.points)) {
		callFun <- eval(playState$call[[1]])
		if ((typeof(callFun) != "closure") || is.null(formals(callFun))) {
			# call does not take arguments
			return(NA)
		}
	}
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
		if (is.null(playState$data.points)) {
			# try to construct labels from the plot call
			if (playState$is.lattice) {
				tmp.data <- NULL
				if ('data' %in% names(plot.call)) {
					tmp.data <- callArg(playState, data)
					labels <- makeLabels(tmp.data)
				}
				if (is.null(labels)) {
					# try to make labels from first argument
					tmp.x <- callArg(playState, 1)
					if (inherits(tmp.x, "formula")) {
						xObj <- if (length(tmp.x) == 2)
							tmp.x[[2]] else tmp.x[[3]]
						# get left-most term in formula
						while (is.call(xObj) && toString(xObj[[1]]) %in% 
							c("|", "*", "+"))
							xObj <- xObj[[2]]
						xObj <- eval(xObj, tmp.data, 
							environment(tmp.x))
						labels <- makeLabels(xObj, orSeq=T)
					} else {
						labels <- makeLabels(tmp.x, orSeq=T)
					}
				}
			} else {
				# base graphics
				if (length(plot.call) >= 2) {
					tmp.x <- callArg(playState, 1)
					if (inherits(tmp.x, "formula")) {
						xObj <- if (length(tmp.x) == 2)
							tmp.x[[2]] else tmp.x[[3]]
						xObj <- eval(xObj, environment(tmp.x), 
							playState$env)
						labels <- makeLabels(xObj, orSeq=T)
					} else {
						labels <- makeLabels(tmp.x, orSeq=T)
					}
				}
			}
		} else {
			# data.points were supplied
			labels <- makeLabels(playState$data.points, orSeq=T)
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

makeLabels <- function(x, orSeq=FALSE) {
	labels <- row.names(x)
	if (inherits(x, "POSIXt"))
		labels <- format(x)
	if (inherits(x, "Date"))
		labels <- format(x)
	if (inherits(x, "ts") || inherits(x, "zoo"))
		labels <- rep(format(stats::time(x)), NCOL(x))
	if (orSeq && is.null(labels)) labels <- seq_along(x)
	labels
}

drawLabels <- function(playState, which, space="plot", pos=1) {
	playDevSet(playState)
	xy <- xyCoords(playState, space=space)
	x <- xy$x[which]
	y <- xy$y[which]
	labels <- playState$labels
	if (playState$is.lattice && (length(labels) > length(xy$subscripts)))
		labels <- labels[ xy$subscripts ]
	labels <- labels[which]
	style <- eval(playState$label.style)
	if (is.null(playState$label.style)) {
		# default style is taken (at plot time) from lattice settings
		style <- do.call(gpar, trellis.par.get("add.text"))
	}
	annots <- expression()
	pos <- rep(pos, length=length(labels))
	# TODO: do this without a loop
	for (i in seq_along(labels)) {
		ux <- unit(x[i], "native")
		uy <- unit(y[i], "native")
		offset <- 0.5
		if (pos[i] == 1) {
		    uy <- uy - unit(offset, "char")
		    adj <- c(0.5, 1)
		}
		else if (pos[i] == 2) {
		    ux <- ux - unit(offset, "char")
		    adj <- c(1, 0.5)
		}
		else if (pos[i] == 3) {
		    uy <- uy + unit(offset, "char")
		    adj <- c(0.5, 0)
		}
		else if (pos[i] == 4) {
		    ux <- ux + unit(offset, "char")
		    adj <- c(0, 0.5)
		}
		annots[[i]] <- call("grid.text", labels[i], x=ux, y=uy, 
			just=adj, gp=style)
	}
	playDo(playState, eval(annots), space=space, 
		clip.off=identical(playState$clip.annotations, FALSE))
}

identify_handler <- function(widget, playState) {
	# TODO qqmath? - or just pass in data.points
	repeat {
		foo <- playSelectData(playState, 
			"Click or drag to identify points. Right-click to end.")
		if (is.null(foo)) break
		if (length(foo$which) == 0) next
		with(foo, {
			if (!is.click) pos <- 1
			# store newly identified points in playState
			ids.new <- data.frame(which=which, pos=pos)
			ids.old <- playState$ids[[space]] # may be NULL
			if (is.null(ids.old)) ids.old <- ids.new
			else ids.new <- rbind(ids.old, ids.new)
			playState$ids[[space]] <- ids.new
			# draw them
			drawLabels(playState, which=which, space=space, pos=pos)
		})
	}
}

identify_postplot_action <- function(widget, playState) {
	# draw persistent labels
	for (space in names(playState$ids)) {
		idInfo <- playState$ids[[space]]
		drawLabels(playState, which=idInfo$which, space=space, 
			pos=idInfo$pos)
	}
}

## ANNOTATE

toolConstructors$annotate <- function(playState) {
	quickTool(playState,
		label = "Annotate", 
		icon = "gtk-italic",
		tooltip = "Add your own labels to the plot",
		f = annotate_handler,
		post.plot.action = annotate_postplot_action)
}

annotate_handler <- function(widget, playState) {
	pageAnnotation <- identical(playState$annotation.mode, "page")
	foo <- playRectInput(playState, prompt=
		"Click or drag to place an annotation. (Right-click to cancel)")
	if (is.null(foo)) return()
	if (is.null(foo$coords)) pageAnnotation <- TRUE
	space <- foo$space
	if (pageAnnotation) space <- "page"
	myXY <- if (space == "page") foo$ndc else foo$coords
	myXY$x <- signif(myXY$x, 4)
	myXY$y <- signif(myXY$y, 4)
	if (foo$is.click) {
		myX <- mean(myXY$x)
		myY <- mean(myXY$y)
	}
	
	playFreezeGUI(playState)
	on.exit(playThawGUI(playState))
	
	# pop up dialog to create label
	dialog <- gwindow(title="New annotation")
	wingroup <- ggroup(horizontal=FALSE, container=dialog)
	wid <- list()
	
	# TEXT AND JUST
	labgroup <- gframe("<b>Label text</b>", markup=TRUE,
		horizontal=FALSE, container=wingroup)
	lay <- glayout(container=labgroup, spacing=2)
	wid$label <- gtext(width=200, height=50)
	wid$label.expr <- gcheckbox("plotmath")
	lay[1,1:2] <- wid$label
	lay[2,1] <- wid$label.expr
	lay[3,1] <- if (foo$is.click) "Position of text \nrelative to click:"
		else "Justification of \ntext inside box:"
	justgroup <- ggroup(horizontal=FALSE)
	wid$hjust <- gradio(c("left", "centre", "right"), selected=2, horizontal=TRUE)
	wid$vjust <- gradio(c("top", "centre", "bottom"), selected=2, horizontal=TRUE)
	add(justgroup, wid$hjust)
	add(justgroup, wid$vjust)
	lay[3,2] <- justgroup
	lay[4,1] <- if (foo$is.click) "Offset from point (chars):"
		else "Offset from box edge"
	wid$offset <- gedit("0", width=5, coerce.with=as.numeric)
	lay[4,2] <- wid$offset
	 if (!foo$is.click) {
		# it was a drag, defining a rectangle
		# option to draw box border
		wid$drawbox <- gcheckbox("Draw box")
		lay[5,1:2] <- wid$drawbox
		# TODO: fit to box?
	}
	visible(lay) <- TRUE
	focus(wid$label) <- TRUE
	
	# STYLE
	stylegroup <- gframe("<b>Style</b>", markup=TRUE,
		horizontal=FALSE, container=wingroup)
	lay <- glayout(container=stylegroup, spacing=2)
	refStyle <- playState$label.style
	if (is.null(playState$label.style)) {
		# default style is taken from lattice settings
		refStyle <- do.call(gpar, trellis.par.get("add.text"))
	}
	# col
	colList <- palette()
	colList <- c(colList, trellis.par.get("superpose.symbol")$col)
	wid$col <- gdroplist(colList, selected=0, editable=TRUE)
	wid$alpha <- gspinbutton(value=1, from=0, to=1, by=0.05, digits=2)
	if (!is.null(refStyle$col)) svalue(wid$col) <- refStyle$col
	if (!is.null(refStyle$alpha)) svalue(wid$alpha) <- refStyle$alpha
	lay[1,1] <- "Text color:"
	lay[1,2] <- wid$col
	lay[2,1] <- "Alpha (opacity):"
	lay[2,2] <- wid$alpha
	# cex, fontsize
	wid$cex <- gedit("1.0", width=5, coerce.with=as.numeric)
	wid$fontsize <- gedit("", width=5, coerce.with=as.numeric)
	if (!is.null(refStyle$cex)) svalue(wid$cex) <- refStyle$cex
	if (!is.null(refStyle$fontsize)) svalue(wid$fontsize) <- refStyle$fontsize
	lay[3,1] <- "Expansion factor:"
	lay[3,2] <- wid$cex
	lay[4,1] <- "Font size, points:"
	lay[4,2] <- wid$fontsize
	# fontfamily, fontface
	familyList <- c("serif", "sans", "mono", "symbol",
		"HersheySerif", "HersheySans", "HersheyScript",
		"HersheyGothicEnglish", "HersheyGothicGerman", "HersheyGothicItalian",
		"HersheySymbol", "HersheySansSymbol")
	faceList <- c("plain", "bold", "italic", "bold.italic",
		"cyrillic", "cyrillic.oblique", "EUC")
	wid$fontfamily <- gdroplist(familyList, selected=0)
	wid$fontface <- gdroplist(faceList, selected=0)
	if (!is.null(refStyle$fontfamily)) svalue(wid$fontfamily) <- refStyle$fontfamily
	if (!is.null(refStyle$fontface)) svalue(wid$fontface) <- refStyle$fontface
	lay[5,1] <- "Font family:"
	lay[5,2] <- wid$fontfamily
	lay[6,1] <- "Font face:"
	lay[6,2] <- wid$fontface
	# lineheight (as multiple of text height)
	wid$lineheight <- gedit("1.0", width=5, coerce.with=as.numeric)
	if (!is.null(refStyle$lineheight)) svalue(wid$lineheight) <- refStyle$lineheight
	lay[7,1] <- "Line height factor:"
	lay[7,2] <- wid$lineheight
	# rot (rotation angle)
	wid$rot <- gdroplist(c("-90","-45","-30","0","30","45","90"), selected=4,
		editable=TRUE, coerce.with=as.numeric)
	lay[8,1] <- "Rotation angle:"
	lay[8,2] <- wid$rot
	visible(lay) <- TRUE
	
	# OPTIONS
	#optionsgroup <- gframe("Options", horizontal=FALSE, container=wingroup)
	# TODO: option to set as default style
	
	showingPreview <- FALSE
	
	annot_handler <- function(h, ...) {
		# note: playState is accessed from the function environment!
		
		# TEXT AND JUST
		argExpr <- function(wid, expr.wid) {
			newVal <- svalue(wid)
			if (newVal == "") return(NULL)
			if (svalue(expr.wid)) 
				newVal <- parse(text=newVal, srcfile=NULL)
			newVal
		}
		isExpr <- svalue(wid$label.expr)
		labelVal <- argExpr(wid$label, wid$label.expr)
		if (!isExpr) {
			labelVal <- gsub("\\", "\\\\", labelVal, fixed=TRUE)
		}
		just <- c(svalue(wid$hjust), svalue(wid$vjust))
		
		# COORDINATES
		if (foo$is.click) {
			# justification flipped -- at point rather than inside rect
			just[1] <- switch(just[1], 
				left="right", right="left", centre="centre")
			just[2] <- switch(just[2], 
				top="bottom", bottom="top", centre="centre")
		}
		else {
			# it was a drag, defining a rectangle
			# choose side of rect to align to
			x.leftright <- sort(myXY$x)
			if (is.unsorted(rawXLim(playState, space=space)))
				x.leftright <- rev(x.leftright)
			y.bottop <- sort(myXY$y)
			if (is.unsorted(rawYLim(playState, space=space)))
				y.bottop <- rev(y.bottop)
			myX <- switch(just[1],
				left=x.leftright[1],
				right=x.leftright[2],
				centre=mean(x.leftright))
			myY <- switch(just[2],
				bottom=y.bottop[1],
				top=y.bottop[2],
				centre=mean(y.bottop))
		}
		#myX <- signif(myX, 4)
		#myY <- signif(myY, 4)
		if ((svalue(wid$offset) != 0) && any(just != "centre")) {
			if (just[1] != "centre") {
				myPad <- svalue(wid$offset)
				if (just[1] == "right") myPad <- 0 - myPad
				myX <- substitute(unit(x, "native") + unit(pad, "char"),
					list(x=myX, pad=myPad))
			}
			if (just[2] != "centre") {
				myPad <- svalue(wid$offset)
				if (just[2] == "top") myPad <- 0 - myPad
				myY <- substitute(unit(y, "native") + unit(pad, "char"),
					list(y=myY, pad=myPad))
			}
		}
		
		# STYLE
		#print("cex")
		#str(svalue(wid$cex))
		#print("col")
		#str(svalue(wid$col))
		#print("family")
		#str(svalue(wid$fontfamily))
		#str(svalue(wid$fontfamily, index=TRUE))
		hasCol <- (svalue(wid$col) != "")
		hasAlpha <- (svalue(wid$alpha) != 1)
		hasCex <- (svalue(wid$cex) != 1.0)
		hasSize <- (!is.na(svalue(wid$fontsize)))
		hasFamily <- !is.null(svalue(wid$fontfamily)) && !isExpr
		hasFace <- !is.null(svalue(wid$fontface)) && !isExpr
		hasHeight <- (svalue(wid$lineheight) != 1.0)
		hasRot <- (svalue(wid$rot) != 0)
		#gpc <- refStyle
		#if (inherits(gpc, "gpar"))
		#	gpc <- as.call(c(quote(gpar), gpc))
		gpc <- call("gpar")
		if (hasCol) gpc$col <- svalue(wid$col)
		if (hasAlpha) gpc$alpha <- svalue(wid$alpha)
		if (hasCex) gpc$cex <- svalue(wid$cex)
		if (hasSize) gpc$fontsize <- svalue(wid$fontsize)
		if (hasFamily) gpc$fontfamily <- svalue(wid$fontfamily)
		if (hasFace) gpc$fontface <- svalue(wid$fontface)
		if (hasHeight) gpc$lineheight <- svalue(wid$lineheight)
		
		# CREATE THE CALL
		annot <- call("grid.text", labelVal, x=myX, y=myY)
		if (!all(just == "centre")) annot$just <- just
		if (space != "page") annot$default.units <- "native"
		annot$gp <- if (length(gpc) > 1) gpc
		if (hasRot) annot$rot <- svalue(wid$rot)
		
		# add box?
		if (!foo$is.click && svalue(wid$drawbox)) {
			dobox <- call("grid.rect", x=mean(myXY$x), y=mean(myXY$y),
				width=abs(diff(myXY$x)), height=abs(diff(myXY$y)))
			if (space != "page") dobox$default.units <- "native"
			annot <- call("{", dobox, annot)
		}
		
		if (h$action == "preview") {
			print(annot)
			# preview only (refresh, grid.draw)
			#annot$name <- "tmp.preview"
			# refresh to remove any old preview
			#grid.refresh()
			#playRefresh(playState)
			if (showingPreview)
				playReplot(playState)
			# draw it
			# engine.display.list / grid.display.list
			#foo <- grid.grabExpr(eval(annot))
			#foo2 <- quote(grid.draw(foo, recording=FALSE))
			#foo2 <- quote(grid:::drawGrob(foo))
			playDo(playState, eval(annot), space=space)
			# and remove without redraw
			#grid.gremove("tmp.preview", redraw=FALSE)
			showingPreview <<- TRUE
			return()
		}
		# draw it
		playDo(playState, eval(annot), space=space)
		# store it
		playState$annotations[[space]] <- 
			c(playState$annotations[[space]], annot)
		# update other tool states
		with(playState$tools, {
			if (exists("edit.annotations", inherits=F))
				edit.annotations["visible"] <- TRUE
			if (exists("clear", inherits=F)) 
				clear["visible"] <- TRUE
		})
		
		dispose(h$obj)
		playState$win$present()
	}
	
	buttgroup <- ggroup(container=wingroup)
	addSpring(buttgroup)
	okbutt <- gbutton("OK", handler=annot_handler, 
		action="ok", container=buttgroup)
	prebutt <- gbutton("Preview", handler=annot_handler, 
		action="preview", container=buttgroup)
	canbutt <- gbutton("Cancel", handler=function(...) dispose(dialog), 
		container=buttgroup)
	size(okbutt) <- size(prebutt) <- size(canbutt) <- c(80, 30)
	return()
}

annotate_postplot_action <- function(widget, playState) {
	# draw annotations
	for (space in names(playState$annotations)) {
		playDo(playState, 
			lapply(playState$annotations[[space]], eval), 
			space=space, 
			clip.off=identical(playState$clip.annotations, FALSE))
	}
}

## ARROW

toolConstructors$arrow <- function(playState) {
	if (is.null(playState$arrow.arrow))
		playState$arrow.arrow <- quote(arrow(length=unit(0.15, "inches")))
	quickTool(playState,
		label = "Arrow", 
		icon = "gtk-connect", 
		tooltip = "Add an arrow to the plot",
		f = arrow_handler)
}

arrow_handler <- function(widget, playState) {
	pageAnnotation <- identical(playState$annotation.mode, "page")
	foo <- playLineInput(playState, prompt=
		"Click and drag to draw an arrow. (Right-click to cancel)")
	if (is.null(foo)) return()
	if (is.null(foo$coords)) pageAnnotation <- TRUE
	if (foo$is.click) return()
	space <- foo$space
	if (pageAnnotation) space <- "page"
	myXY <- if (space == "page") foo$ndc else foo$coords
	myXY$x <- signif(myXY$x, 4)
	myXY$y <- signif(myXY$y, 4)
	annot <- call("grid.lines", x=myXY$x, y=myXY$y)
	if (space != "page") annot$default.units <- "native"
	annot$arrow <- playState$arrow.arrow
	style <- eval(playState$arrow.style)
	if (inherits(style, "gpar"))
		style <- as.call(c(quote(gpar), style))
	if (is.null(playState$arrow.style)) {
		# default style is taken (at plot time) from lattice settings
		style <- quote(do.call(gpar, trellis.par.get("add.line")))
	}
	if (!is.null(style)) annot$gp <- style
	# draw it
	playDo(playState, eval(annot), space=space)
	# store it
	playState$annotations[[space]] <- 
		c(playState$annotations[[space]], annot)
	# update other tool states
	with(playState$tools, {
		if (exists("edit.annotations", inherits=F))
			edit.annotations["visible"] <- TRUE
		if (exists("clear", inherits=F)) 
			clear["visible"] <- TRUE
	})
}

## EDIT.ANNOTATIONS

toolConstructors$edit.annotations <- function(playState) {
	quickTool(playState,
		label = "Edit ann.", 
		icon = "gtk-edit", 
		tooltip = "Edit annotations (including arrows)",
		f = edit.annotations_handler,
		show = length(playState$annotations) > 0)
}

edit.annotations_handler <- function(widget, playState) {
	annotSpaces <- names(playState$annotations)
	if (length(annotSpaces) == 0) return()
	if (length(annotSpaces) == 1) {
		space <- annotSpaces[1]
	}
	else if (length(annotSpaces) > 1) {
		space <- select.list(annotSpaces, 
			multiple = FALSE, title = "Choose annotation space")
		playState$win$present()
		if (space == "") return()
	}
	annots <- playState$annotations[[space]]
	callTxt <- paste(unlist(lapply(annots, deparse, control="showAttributes")), collapse="\n")
	repeat {
		newTxt <- NULL
		txtBox <- gtext(callTxt, font.attr=c(family="monospace"), wrap=FALSE, width=600)
		gbasicdialog(title="Edit annotations", widget=txtBox,
				action=environment(), handler=function(h, ...)
				assign("newTxt", svalue(h[[1]]), env=h$action)
		)
		if (is.null(newTxt)) break
		callTxt <- newTxt
		tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
		# check whether there was a syntax error
		if (inherits(tmp, "error")) {
			gmessage.error(conditionMessage(tmp))
		} else {
			playState$annotations[[space]] <- tmp
			playReplot(playState)
			break
		}
	}
	playState$win$present()
}

## CLEAR

toolConstructors$clear <- function(playState) {
	if ((length(playState$call) == 1) && 
		identical(playState$call[[1]], quote(`{`))) {
		# do not know the call; it cannot be redrawn
		return(NA)
	}
	types <- c(
		if (length(playState$ids) > 0) "ids",
		if (length(playState$annotations) > 0) "annotations",
		if (length(playState$brushed) > 0) "brushed"
	)
	quickTool(playState,
		label = "Clear", 
		icon = "gtk-clear", 
		tooltip = "Remove labels and annotations", 
		f = clear_handler,
		show = (length(types) > 0))
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
	callFun <- eval(playState$call[[1]])
	if ((typeof(callFun) != "closure") || is.null(formals(callFun))) {
		# call does not take arguments
		return(NA)
	}
	quickTool(playState,
		label = "Zoom...", 
		icon = "gtk-zoom-in", 
		tooltip = "Select a new plot region with the mouse",
		f = zoom_handler)
}

zoom_handler <- function(widget, playState) {
	nav.x <- TRUE
	nav.y <- !(playState$time.mode)
	foo <- playRectInput(playState, prompt=
		"Click and drag to define the new plot region. (Right-click to cancel)")
	if (is.null(foo)) return()
	if (is.null(foo$coords)) return()
	if (foo$is.click) return()
	# draw rectangle for feedback, before redrawing the plot
	#with(foo$ndc, {
	#	grid.polygon(x=c(x[1], x[1], x[2], x[2]),
	#		y=c(y[1], y[2], y[2], y[1]),
	#		gp=gpar(col="red", lty="dashed"))
	#})
	xlim <- range(foo$coords$x)
	ylim <- range(foo$coords$y)
	# reverse axis scales if needed
	if (is.unsorted(rawXLim(playState, space=foo$space))) xlim <- rev(xlim)
	if (is.unsorted(rawYLim(playState, space=foo$space))) ylim <- rev(ylim)
	# this converts from raw numeric to original format (including unlog)
	if (nav.x) rawXLim(playState) <- xlim
	if (nav.y) rawYLim(playState) <- ylim
	playReplot(playState)
}

## ZOOMOUT

toolConstructors$zoomout <- function(playState) {
	callFun <- eval(playState$call[[1]])
	if ((typeof(callFun) != "closure") || is.null(formals(callFun))) {
		# call does not take arguments
		return(NA)
	}
	quickTool(playState,
		label = "Zoom out", 
		tooltip = "Zoom out to show 4x plot area",
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
	callFun <- eval(playState$call[[1]])
	if ((typeof(callFun) != "closure") || is.null(formals(callFun))) {
		# call does not take arguments
		return(NA)
	}
	quickTool(playState,
		label = "Fit data",
		icon = "gtk-zoom-fit", 
		f = zoomfit_handler,
		post.plot.action = zoomfit_postplot_action)
}

zoomfit_handler <- function(widget, playState) {
	nav.x <- TRUE
	nav.y <- TRUE #!(playState$time.mode)
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
	callFun <- eval(playState$call[[1]])
	if ((typeof(callFun) != "closure") || is.null(formals(callFun))) {
		# call does not take arguments
		return(NA)
	}
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
	callFun <- eval(playState$call[[1]])
	if ((typeof(callFun) != "closure") || is.null(formals(callFun))) {
		# call does not take arguments
		gmessage.error("Do not know the name of the plot function.")
		return()
	}
	callName <- deparseOneLine(playState$call[[1]])
	# work out which (S3) method was called, if any
	methNames <- methods(callName)
	if ((length(methNames) > 0) && length(playState$call > 1)) {
		myClass <- class(callArg(playState, 1))
		myMeth <- paste(callName, myClass, sep=".")
		ok <- (myMeth %in% methNames)
		if (any(ok)) callName <- myMeth[ok][1]
	}
	print(help(callName))
}
	
# EDIT.CALL

edit.call.inline_handler <- function(widget, playState) {
	# the original call -- this should match the code in playReplot!
	callTxt <- deparseOneLine(playState$call, control="showAttributes")
	newTxt <- widget["text"]
	if (identical(newTxt, callTxt)) return()
	if (identical(newTxt, "")) return()
	tmp <- tryCatch(parse(text=newTxt), error=function(e)e)
	# check whether there was a syntax error
	if (inherits(tmp, "error")) {
		gmessage.error(conditionMessage(tmp))
	} else {
		# if more than one call, wrap them in braces
		playState$call <- if (length(tmp) > 1)
			as.call(c(as.symbol("{"), tmp)) else tmp[[1]]
		playNewPlot(playState)
	}
	playState$win$present()
}

edit.call_handler <- function(widget, playState) {
	theCall <- playState$call
	callTxt <- paste(deparse(theCall, control="showAttributes"), collapse="\n")
	repeat {
		newTxt <- NULL
		txtBox <- gtext(callTxt, font.attr=c(family="monospace"), wrap=FALSE, width=600)
		gbasicdialog(title="Edit plot call", widget=txtBox,
				action=environment(), handler=function(h, ...)
				assign("newTxt", svalue(h[[1]]), env=h$action)
		)
		if (is.null(newTxt)) break
		callTxt <- newTxt
		tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
		# check whether there was a syntax error
		if (inherits(tmp, "error")) {
			gmessage.error(conditionMessage(tmp))
		} else {
			# if more than one call, wrap them in braces
			playState$call <- if (length(tmp) > 1)
				as.call(c(as.symbol("{"), tmp)) else tmp[[1]]
			playNewPlot(playState)
			break
		}
	}
	playState$win$present()
}

## BRUSH

toolConstructors$brush <- function(playState) {
	quickTool(playState,
		label = "Brush", 
		icon = "gtk-media-record", 
		tooltip = "Brush (highlight) data points",
		f = brush_handler,
		post.plot.action = brush_postplot_action)
}

brush_handler <- function(widget, playState) {
	if (!playState$is.lattice) {
		gmessage.error("Brushing only works with lattice::splom")
		return()
	}
	# do brushing
	newFocus <- playFocus(playState)
	if (!any(newFocus)) return()
	playPrompt(playState, paste("Brushing data points...",
		"Click the right mouse button to finish."))
	brushed.new <- panel.brush.splom()
	playPrompt(playState, NULL)
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
		gmessage.error("Brushing only works with lattice::splom")
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
		gmessage.error("Brushing only works with lattice::splom")
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

