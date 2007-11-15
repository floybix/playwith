## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

settings_handler <- function(widget, playState) {
	playState$tools$settings["sensitive"] <- FALSE
	on.exit(playState$tools$settings["sensitive"] <- TRUE)
	wingroup <- ggroup(horizontal=FALSE)
	wid <- list()
	
	# convenience extractor
	arg <- function(x) do.call(callArg, list(playState, substitute(x)))
	
	# TODO: LEGEND / KEY
	
	# TITLES
	labgroup <- gframe("Titles", horizontal=FALSE, container=wingroup)
	lay <- glayout(container=labgroup)
	rownum <- 1
	for (nm in c("main", "sub", "xlab", "ylab")) {
		argVal <- if (playState$is.lattice) playState$trellis[[nm]]
			else callArg(playState, name=nm)
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
	
	# AXES
	axisgroup <- gframe("Axes", horizontal=FALSE, container=wingroup)
	lay <- glayout(container=axisgroup)
	wid$xaxis.show <- gcheckbox("visible", checked=!(
		any(arg(scales$x$draw) == FALSE) ||
		any(arg(scales$draw) == FALSE) ||
		any(arg(axes) == FALSE) ||
		any(arg(xaxt) == "n")
	))
	wid$yaxis.show <- gcheckbox("visible", checked=!(
		any(arg(scales$y$draw) == FALSE) ||
		any(arg(scales$draw) == FALSE) ||
		any(arg(axes) == FALSE) ||
		any(arg(yaxt) == "n")
	))
	wid$xaxis.log <- gcheckbox("logarithmic", checked=(
		any(as.character(arg(scales$x$log)) != "FALSE") ||
		any(as.character(arg(scales$log)) != "FALSE") ||
		any(grep("x", arg(log)))
	))
	wid$yaxis.log <- gcheckbox("logarithmic", checked=(
		any(as.character(arg(scales$y$log)) != "FALSE") ||
		any(as.character(arg(scales$log)) != "FALSE") ||
		any(grep("y", arg(log)))
	))
	wid$aspect.iso <- gcheckbox("isometric scale", checked=(
		any(arg(aspect) == "iso") ||
		any(arg(asp) == 1)
	))
	lay[1,1] <- "x-axis:"
	lay[1,2] <- wid$xaxis.show
	lay[1,3] <- wid$xaxis.log
	lay[2,1] <- "y-axis:"
	lay[2,2] <- wid$yaxis.show
	lay[2,3] <- wid$yaxis.log
	visible(lay) <- TRUE
	add(axisgroup, wid$aspect.iso)
	
	# DECORATIONS
	decogroup <- gframe("Decorations", horizontal=FALSE, container=wingroup)
	enabled(decogroup) <- FALSE
	# reference lines
	linegroup <- ggroup(container=decogroup)
	wid$abline_h <- gcheckbox("horizontal", checked=F)
	wid$abline_v <- gcheckbox("vertical", checked=F)
	wid$abline_d <- gcheckbox("diagonal", checked=F)
	add(linegroup, glabel("Reference line (origin):"))
	add(linegroup, wid$abline_h)
	add(linegroup, wid$abline_v)
	add(linegroup, wid$abline_d)
	# grid
	gridgroup <- ggroup(container=decogroup)
	wid$grid_h <- gcheckbox("horizontal", checked=F)
	wid$grid_v <- gcheckbox("vertical", checked=F)
	add(gridgroup, glabel("Grid:"))
	add(gridgroup, wid$grid_h)
	add(gridgroup, wid$grid_v)
	# rug
	wid$rug <- gcheckbox("Rug (marginal distribution)", checked=F)
	add(decogroup, wid$rug)
	
	# panel.lmline()
	# panel.loess()
	
	# STYLE
	stylegroup <- gframe("Style", horizontal=FALSE, container=wingroup)
	enabled(stylegroup) <- FALSE
	# lattice theme
	# TODO color (white bg) / color (dark bg) / greyscale
	# type
	arg_type <- callArg(playState, type)
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
	# cex
	wid$cex <- gedit(toString(callArg(playState, cex)), width=5, 
		coerce.with=as.numeric)
	lay[1,1] <- "Expansion factor:"
	lay[1,2] <- wid$cex
	# pch
	pchList <- list(
		`open circle`=21,
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
	which_pch <- which(sapply(pchList, identical, callArg(playState, pch)))
	if (length(which_pch) == 0) which_pch <- 0
	wid$pch <- gdroplist(names(pchList), selected=which_pch)
	lay[2,1] <- "Plot symbol:"
	lay[2,2] <- wid$pch
	# col
	colList <- palette()
	arg_col <- callArg(playState, col)
	which_col <- if (is.numeric(arg_col)) which(arg_col == seq_along(colList))
		else which(sapply(colList, identical, arg_col))
	if (length(which_col) == 0) which_col <- 0
	wid$col <- gdroplist(colList, selected=which_col)
	lay[3,1] <- "Color (foreground):"
	lay[3,2] <- wid$col
	# lty
	ltyList <- c("solid", "dashed", "dotted", "dotdash", "longdash")
	# lwd TODO
	# fontface / fontfamily / font
	familyList <- list("sans", "serif", "mono")
	fontList <- list(plain=1, bold=2, italic=3, bolditalic=4)
	visible(lay) <- TRUE
	
	# lattice
	# panel.grid()
	# panel.rug()
	# panel.abline()
	# panel.lmline()
	# panel.loess()
	
	# plot.default (panel.first)
	# grid()
	# rug()
	# abline()
	# lines(lm())
	
	# grid / panel.grid()
	# rug
	# axis lines abline(h=0), abline(v=0)
	# regression line panel.lmline / type="r"
	# smooth panel.smooth
	
	# OTHER
	
	# superpose
	
	# layers
	
	# TODO: make the dialog not modal
	
	gbasicdialog("Plot settings", widget=wingroup, action=playState, 
	handler=function(h, ...) {
		playState <- h$action
		playDevSet(playState)
		
		# TITLES
		argExpr <- function(wid, expr.wid) {
			newVal <- svalue(wid)
			if (newVal == "") return(NULL)
			if (svalue(expr.wid)) 
				newVal <- parse(text=newVal, srcfile=NULL)
			newVal
		}
		callArg(playState, main) <- argExpr(wid$main, wid$main.expr)
		callArg(playState, sub) <- argExpr(wid$sub, wid$sub.expr)
		callArg(playState, xlab) <- argExpr(wid$xlab, wid$xlab.expr)
		callArg(playState, ylab) <- argExpr(wid$ylab, wid$ylab.expr)
		
		# AXES
		if (playState$is.lattice) {
			newXdraw <- if (svalue(wid$xaxis.show)) NULL else FALSE
			newYdraw <- if (svalue(wid$yaxis.show)) NULL else FALSE
			callArg(playState, scales$x$draw) <- newXdraw
			callArg(playState, scales$y$draw) <- newYdraw
			newXlog <- if (svalue(wid$xaxis.log)) TRUE else NULL
			newYlog <- if (svalue(wid$yaxis.log)) TRUE else NULL
			callArg(playState, scales$x$log) <- newXlog
			callArg(playState, scales$y$log) <- newYlog
			newAspect <- if (svalue(wid$aspect.iso)) "iso" else NULL
			callArg(playState, aspect) <- newAspect
		} else {
			# base graphics plot
			newXaxt <- if (svalue(wid$xaxis.show)) NULL else "n"
			newYaxt <- if (svalue(wid$yaxis.show)) NULL else "n"
			callArg(playState, xaxt) <- newXaxt
			callArg(playState, yaxt) <- newYaxt
			newLog <- paste(c(if (svalue(wid$xaxis.log)) "x",
					if (svalue(wid$yaxis.log)) "y"), 
					collapse="")
			if (newLog == "") newLog <- NULL
			callArg(playState, log) <- newLog
			newAsp <- if (svalue(wid$aspect.iso)) 1 else NULL
			callArg(playState, asp) <- newAsp
		}
		
		# DECORATIONS
		wid$abline_h
		wid$abline_v
		wid$abline_d
		wid$grid_h
		wid$grid_v
		wid$rug
		
		# dispose(h$obj)
		playReplot(playState)
	})
	playState$win$present()
}


makeLayersMenuButton <- function() {
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
		# store evaluated list in call
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
				# TODO: make lists pretty
			}
			itemNames[i] <- itemName
			itemIDs[i] <- i
			itemStates[i] <- !identical(the.call$sp.layout[[i]]$which, 0)
		}
	} else if ('panel' %in% names(the.call)) {
		layerType <- "panel"
		# skip if this is not an inline function
		if (is.symbol(the.call$panel)) return(NA)
		panelBody <- body(eval(the.call$panel, StateEnv[[name]]$env))
		# treat any call to panel.* or grid.* or sp.* as a layer
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
		itemIDs <- lapply(itemIDs, eval) # convert from `call` to vector
		itemIDsStr <- sapply(itemIDs, toIndexStr)
		itemNames <- sapply(paste('panelBody',itemIDsStr,sep=''), 
			function(s) deparse(eval(parse(text=s)))[1] )
		# work out whether each item is turned off with `if (FALSE)`
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
		# tmp <- quote({ print(x); while (x) { print(panel.list('a','b')); x <- something(grid.lines()) } })
	}
	itemNames <- sapply(itemNames, toString, width=34)
	# omit the button if only one layer
	if (length(itemIDs) <= 1) return(NA)
	# store layers info
	StateEnv[[name]]$layers.names <- itemNames
	StateEnv[[name]]$layers.ids <- itemIDs
	# make button menu
	layersMenu <- gtkMenu()
	for (i in seq_along(itemIDs)) {
		menuItem <- gtkCheckMenuItem(itemNames[i]) #gtkMenuItem(itemName)
		menuItem['active'] <- itemStates[i]
		layersMenu$append(menuItem)
		gSignalConnect(menuItem, "activate", .plotAndPlay_layers_event, 
			data=list(index=i, ID=itemIDs[[i]], layerType=layerType))
	}
	layersButton$setMenu(layersMenu)
	# set main button handler
	gSignalConnect(layersButton, "clicked", .plotAndPlay_layers_event,
		data=list(menu=layersMenu, layerType=layerType))
	layersButton
}


.plotAndPlay_layers_event <- function(widget, user.data=NULL) {
	name <- StateEnv$.current
	itemIdx <- user.data$index
	itemID <- user.data$ID
	layerType <- user.data$layerType
	# disable other plot buttons until this is over
	plotAndPlayGetToolbar()$setSensitive(F)
	on.exit(plotAndPlayGetToolbar()$setSensitive(T))
	
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
		if (all(newStates == FALSE)) return() # might be 'cancel'
		StateEnv[[name]]$skip.updates <- T
		for (i in seq_along(newStates)) {
			menuItems[[i]]['active'] <- newStates[i]
		}
		StateEnv[[name]]$skip.updates <- F
		plotAndPlayUpdate()
		return()
	}
	# a single menu item toggled
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
		# TODO: store existing `which`
		if (isActive) {
			StateEnv[[name]]$call$sp.layout[[itemIdx]]$which <- NULL
		} else {
			StateEnv[[name]]$call$sp.layout[[itemIdx]]$which <- 0
		}
	} else if (layerType == "panel") {
		# make sure panel function in call has been evaluated
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
	plotAndPlayUpdate()
}


