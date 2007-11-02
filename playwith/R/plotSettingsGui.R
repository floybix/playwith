


.plotAndPlay_logscale_event <- function(widget, user.data) {
	name <- StateEnv$.current
	trans.x <- ("x" %in% StateEnv[[name]]$trans.scales)
	trans.y <- ("y" %in% StateEnv[[name]]$trans.scales)
	# get new log scale setting
	logScale <- widget$getActive()
	# make change and re-draw plot
	if (StateEnv[[name]]$is.lattice) {
		if (trans.x && trans.y) {
			# apply to both scales
			StateEnv[[name]]$call$scales$log <- logScale
		} else {
			if (trans.x) StateEnv[[name]]$call$scales$x$log <- logScale
			if (trans.y) StateEnv[[name]]$call$scales$y$log <- logScale
		}
	} else {
		logSpec <- "xy"
		if (!trans.y) logSpec <- "x"
		if (!trans.x) logSpec <- "y"
		StateEnv[[name]]$call$log <- if (logScale) logSpec
	}
	plotAndPlayUpdate()
}

.plotAndPlay_greyscale_event <- function(widget, user.data) {
	name <- StateEnv$.current
	# get new greyscale setting
	greyscale <- widget$getActive()
	# make change and re-draw plot
	if (greyscale) {
		trellis.device(new=F, color=F)
	} else {
		trellis.device(new=F)
	}
	plotAndPlayUpdate()
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


