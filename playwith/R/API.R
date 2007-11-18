## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

## API functions

playDevCur <- function() StateEnv$.current # may be NULL

playDevList <- function() {
	foo <- as.list(StateEnv)
	names(foo) <- lapply(foo, function(x) toString(x$win["title"]))
	foo
}

playDevSet <- function(playState) {
	StateEnv$.current <- playState
	playState$old.dev <- dev.cur()
	dev.set(playState$dev)
	playState$win$present()
}

playDevOff <- function(playState = playDevCur()) {
	try(playState$win$destroy(), silent=TRUE)
	cleanupStateEnv()
}

cleanupStateEnv <- function() {
	for (ID in ls(StateEnv)) {
		if (!inherits(StateEnv[[ID]]$win, "GtkWindow")) {
			# window is defunct
			rm(list=ID, envir=StateEnv)
		}
	}
	if (!inherits(StateEnv$.current$win, "GtkWindow")) {
		StateEnv$.current <- if (length(ls(StateEnv))) 
			StateEnv[[ ls(StateEnv)[1] ]] else NULL
	}
}

callArg <- function(playState, arg, name=NULL) {
	if (missing(arg) && missing(name)) stop("give 'arg' or 'name'")
	arg <- if (missing(arg)) parse(text=name)[[1]]
		else substitute(arg)
	getx <- if (is.numeric(arg)) paste("[[", arg+1, "]]", sep="")
		else if (is.symbol(arg)) paste('[["', arg, '", exact=TRUE]]', sep="")
		else paste("$", deparseOneLine(arg), sep="")
	expr <- eval(parse(text=paste("playState$call", getx, sep="")))
	if (mode(expr) == "expression") return(expr)
	eval(expr, envir=playState$env, enclos=parent.frame())
}

"callArg<-" <- function(playState, arg, name=NULL, value) {
	if (missing(arg) && missing(name)) stop("give 'arg' or 'name'")
	arg <- if (missing(arg)) parse(text=name)[[1]]
		else substitute(arg)
	if (is.null(arg)) return()
	# instantiate implicit lists as language objects (so deparse is pretty)
	xbits <- strsplit(deparseOneLine(arg), "$", fixed=TRUE)[[1]]
	for (i in seq_len(length(xbits) - 1)) {
		implicitList <- paste(xbits[1:i], collapse="$")
		if (is.null(callArg(playState, name=implicitList))) {
			callArg(playState, name=implicitList) <- quote(list())
		}
	}
	getx <- if (is.numeric(arg)) paste("[[", arg+1, "]]", sep="")
		else paste("$", deparseOneLine(arg), sep="")
	expr <- parse(text=paste("playState$call", getx, sep=""))[[1]]
	expr <- call("<-", expr, quote(value))
	eval(expr, enclos=parent.frame())
	playState
}

playFreezeGUI <- function(playState)
	playSetFreezeGUI(playState, TRUE)

playThawGUI <- function(playState)
	playSetFreezeGUI(playState, FALSE)

playSetFreezeGUI <- function(playState, frozen) {
	with(playState$widgets, {
		topToolbar["sensitive"] <- !frozen
		leftToolbar["sensitive"] <- !frozen
		bottomToolbar["sensitive"] <- !frozen
		rightToolbar["sensitive"] <- !frozen
		pageScrollBox["sensitive"] <- !frozen
		timeScrollBox["sensitive"] <- !frozen
		#callToolbar["sensitive"] <- !frozen
	})
	playState$win$getWindow()$setCursor(
		if (frozen) gdkCursorNew("watch") else NULL)
}

blockRedraws <- function(expr, playState = playDevCur()) {
	oval <- playState$skip.redraws
	playState$skip.redraws <- TRUE
	da <- playState$widgets$drawingArea
	da$setSizeRequest(da$getAllocation()$width, da$getAllocation()$height)
	#playState$win$setGeometryHints(da, list(max.width=myW, min.width=myW, 
	#	max.height=myH, min.height=myH))
	da$window$freezeUpdates()
	eval.parent(substitute(expr))
	da$window$thawUpdates()
	#playState$win$setGeometryHints(da, list())
	da$setSizeRequest(-1, -1)
	playState$skip.redraws <- oval
}

playPrompt <- function(playState, text=NULL) {
	with(playState$widgets, {
		if (is.null(text)) {
			# remove the prompt widget
			promptLabel$setMarkup("")
			callToolbar$show()
			promptBox$hide()
			playThawGUI(playState)
			return()
		}
		if (promptBox["visible"] == FALSE) {
			# create the prompt widget
			promptBox["height-request"] <- 
				callToolbar$getAllocation()$height
			promptBox$show()
			callToolbar$hide()
			playFreezeGUI(playState)
		}
		# set the prompt text
		promptLabel$setMarkup(paste("<big><b>", 
			toString(text), "</b></big>"))
	})
}

rawXLim <- function(playState, space="plot") {
	rawXYLim(playState, space=space)$x
}

rawYLim <- function(playState, space="plot") {
	rawXYLim(playState, space=space)$y
}

rawXYLim <- function(playState, space="plot") {
	playDevSet(playState)
	if (playState$is.lattice && (space == "plot")) {
		# if space does not specify a panel, just pick one
		space <- packet.number()
		if (length(space) == 0) {
			packets <- trellis.currentLayout(which="packet")
			space <- packets[packets > 0][1]
		}
		space <- paste("packet", space)
	}
	playDo(playState, space=space, list(
		x=convertX(unit(0:1, "npc"), "native", valueOnly=TRUE),
		y=convertY(unit(0:1, "npc"), "native", valueOnly=TRUE)))
}

"rawXLim<-" <- function(playState, value) {
	setRawXYLim(playState, value, "x")
	playState
}

"rawYLim<-" <- function(playState, value) {
	setRawXYLim(playState, value, "y")
	playState
}

setRawXYLim <- function(playState, x, x.or.y=c("x", "y")) {
	playDevSet(playState)
	x.or.y <- match.arg(x.or.y)
	# convert back from log scale if required
	x <- unlogXY(x, playState$call, playState$is.lattice, x.or.y=x.or.y)
	if (playState$is.lattice) {
		# TODO: this really sucks
		x.panel <- xyData(playState, space="page")[[x.or.y]]
		# convert new scale to appropriate date time class if required
		#if (inherits(x.panel, "Date") || inherits(x.panel, "POSIXt")) {
		#	mostattributes(x) <- attributes(x.panel)
		#}
		# convert new scale to factor levels if required
		if (is.factor(x.panel)) {
			if (is.null(callArg(playState, scales[[x.or.y]]$labels))) {
				callArg(playState, scales[[x.or.y]]$labels) <- levels(x.panel)
				callArg(playState, scales[[x.or.y]]$at) <- 1:nlevels(x.panel)
			}
		}
	}
	if (is.numeric(x)) x <- signif(x, 4)
	if (x.or.y == "x") callArg(playState, xlim) <- x
	if (x.or.y == "y") callArg(playState, ylim) <- x
}

# deprecated: use class(xyData()$x)
xClass <- function(playState) {
	if (playState$is.lattice && is.null(playState$data.points)) {
		foo <- playState$trellis$x.limits
	} else {
		foo <- xyCoordsWithClass(playState)$x
	}
	if (inherits(foo, "AsIs")) {
		cls <- setdiff(class(foo), "AsIs")
		if (length(cls)) return(cls)
		else return(class(unclass(foo)))
	}
	class(foo)
}

# deprecated: use class(xyData()$y)
yClass <- function(playState) {
	if (playState$is.lattice && is.null(playState$data.points)) {
		foo <- playState$trellis$y.limits
	} else {
		foo <- xyCoordsWithClass(playState)$y
	}
	if (inherits(foo, "AsIs")) {
		cls <- setdiff(class(foo), "AsIs")
		if (length(cls)) return(cls)
		else return(class(unclass(foo)))
	}
	class(foo)
}

# note space="page" means the root viewport
playDo <- function(playState, expr, space="plot", clip.off=FALSE) {
	playDevSet(playState)
	cur.vp <- current.vpPath()
	upViewport(0) # go to root viewport
	on.exit(expression()) # placeholder
	if (space != "page") {
		# user / plot coordinates
		if (!is.null(playState$viewport)) {
			# grid graphics plot
			depth <- downViewport(playState$viewport[[space]])
			if (inherits(depth, "try-error")) {
				stop(paste("Viewport", playState$viewport, "not found"))
			}
			on.exit(upViewport(depth), add=TRUE)
		}
		else if (playState$is.lattice) {
			# lattice plot
			packets <- trellis.currentLayout(which="packet")
			if (space == "plot") {
				space <- packet.number()
				if (length(space) == 0) {
					if (sum(packets > 0) > 1)
						stop("space not well specified")
					space <- packets[packets > 0][1]
				}
				space <- paste("packet", space)
			}
			packet <- as.numeric(sub("packet ", "", space))
			whichOne <- which(packets == packet)
			if (length(whichOne) == 0) return()
			myCol <- col(packets)[whichOne]
			myRow <- row(packets)[whichOne]
			myVp <- trellis.vpname("panel", myCol, myRow, clip.off=clip.off)
			depth <- downViewport(myVp)
			on.exit(upViewport(depth), add=TRUE)
			#trellis.focus("panel", myCol, myRow, highlight=FALSE)
			#on.exit(trellis.unfocus())
		}
		else {
			# base graphics
			space <- "plot"
			if (clip.off) space <- "plot.clip.off"
			depth <- downViewport(playState$baseViewports[[space]])
			on.exit(upViewport(depth), add=TRUE)
		}
	}
	if (!is.null(cur.vp)) on.exit(downViewport(cur.vp), add=TRUE)
	# do the stuff and return the result
	#if (is.list(stuff)) lapply(stuff, eval, parent.frame(), playState$env)
	#else eval(stuff, parent.frame(), playState$env)
	eval(substitute(expr), parent.frame(), playState$env)
}

playSelectData <- function(playState, prompt="Click or drag to select data points.") {
	foo <- playRectInput(playState, prompt=prompt)
	if (is.null(foo)) return(NULL)
	if (is.null(foo$coords)) return(NULL)
	coords <- foo$coords
	data <- xyCoords(playState, space=foo$space)
	ok <- ((min(coords$x) <= data$x) & (data$x <= max(coords$x))
		& (min(coords$y) <= data$y) & (data$y <= max(coords$y)))
	#if (!any(ok)) return(FALSE)
	which <- which(ok)
	pos <- NULL
	if (foo$is.click) {
		x <- mean(coords$x)
		y <- mean(coords$y)
		if (length(which) > 1) {
			# TODO: need to do this in device coords, not user coords!
			pdists <- sqrt((data$x[ok] - x)^2 + (data$y[ok] - y)^2)
			which <- which[which.min(pdists)]
		}
		# pos argument to text
		if (length(which)) pos <- lattice:::getTextPosition(
			x=(x - data$x[which]), y=(y - data$y[which]))
	}
	list(which=which, space=foo$space, 
		x=data$x[ok], y=data$y[ok], 
		pos=pos, is.click=foo$is.click)
}

playPointInput <- function(playState, prompt="Click on the plot") {
	playDevSet(playState)
	playPrompt(playState, prompt)
	on.exit(playPrompt(playState, NULL))
	cur.vp <- current.vpPath()
	upViewport(0)
	if (!is.null(cur.vp)) on.exit(downViewport(cur.vp), add=TRUE)
	dc <- grid.locator()
	if (is.null(dc)) return(NULL)
	ndc <- list(x=convertX(dc$x, "npc"), y=convertY(dc$y, "npc"))
	dc <- lapply(dc, as.numeric)
	ndc <- lapply(ndc, as.numeric)
	coords <- NULL
	space <- whichSpace(playState, dc$x, dc$y)
	if (space != "page") {
		coords <- deviceCoordsToSpace(playState, dc$x, dc$y, space=space)
	}
	list(coords=coords, space=space, dc=dc, ndc=ndc)
}

playPolyInput <- function(playState, prompt="Click points to define a region; right-click to end") {
	playDevSet(playState)
	playPrompt(playState, prompt)
	on.exit(playPrompt(playState, NULL))
	vp <- current.vpPath()
	upViewport(0)
	on.exit(if (!is.null(vp)) downViewport(vp), add=TRUE)
	# wait for click
	xy0 <- grid.locator()
	if (is.null(xy0)) return(NULL)
	xy0 <- lapply(xy0, as.numeric)
	# TODO...
	repeat {
	}
}

playLineInput <- function(playState, prompt="Click and drag to define a line") {
	playDevSet(playState)
	playPrompt(playState, prompt)
	on.exit(playPrompt(playState, NULL))
	vp <- current.vpPath()
	upViewport(0)
	on.exit(if (!is.null(vp)) downViewport(vp), add=TRUE)
	# wait for click
	xy0 <- grid.locator()
	if (is.null(xy0)) return(NULL)
	xy0 <- lapply(xy0, as.numeric)
	playClickOrDrag(playState, x0=xy0$x, y=xy0$y, shape="line")
}

playRectInput <- function(playState, prompt="Click and drag to define a rectangular region") {
	playDevSet(playState)
	playPrompt(playState, prompt)
	on.exit(playPrompt(playState, NULL))
	vp <- current.vpPath()
	upViewport(0)
	on.exit(if (!is.null(vp)) downViewport(vp), add=TRUE)
	# wait for click
	xy0 <- grid.locator()
	if (is.null(xy0)) return(NULL)
	xy0 <- lapply(xy0, as.numeric)
	playClickOrDrag(playState, x0=xy0$x, y=xy0$y, shape="rect")
}

# assumes that the mouse button has already been pressed
# converts into user coordinates
playClickOrDrag <- function(playState, x0, y0, shape=c("rect", "line")) {
	shape <- match.arg(shape)
	da <- playState$widgets$drawingArea
	foo <- handleClickOrDrag(da, x0=x0, y0=y0, shape=shape)
	if (is.null(foo)) return(NULL)
	dc <- foo$dc
	coords <- NULL
	space <- whichSpace(playState, dc$x[1], dc$y[1])
	if (space == "page") space <- whichSpace(playState, dc$x[2], dc$y[2])
	if (space == "page") {
		# corners of drag not inside a defined space: try the mid-point
		space <- whichSpace(playState, mean(dc$x), mean(dc$y))
	}
	if (space != "page") {
		xy0 <- deviceCoordsToSpace(playState, dc$x[1], dc$y[1], space=space)
		xy1 <- deviceCoordsToSpace(playState, dc$x[2], dc$y[2], space=space)
		coords <- list(x=c(xy0$x, xy1$x), y=c(xy0$y, xy1$y))
	}
	foo$coords <- coords
	foo$space <- space
	foo
}

# assumes that the mouse button has already been pressed
handleClickOrDrag <- function(da, x0, y0, shape=c("rect", "line")) {
	shape <- match.arg(shape)
	px0 <- list(x=x0, y=y0)
	da.w <- da$getAllocation()$width
	da.h <- da$getAllocation()$height
	buf <- gdkPixbufGetFromDrawable(src=da$window, src.x=0, src.y=0, 
		dest.x=0, dest.y=0, width=da.w, height=da.h)
	if (is.null(buf)) stop("Could not make pixbuf")
	gc <- gdkGCNew(da$window)
	gc$copy(da[["style"]][["blackGc"]])
	gc$setRgbFgColor(gdkColorParse("black")$color)
	gc$setRgbBgColor(gdkColorParse("white")$color)
	gc$setLineAttributes(line.width=1, line.style=GdkLineStyle["double-dash"],
		cap.style=GdkCapStyle["butt"], join.style=GdkJoinStyle["miter"])
	gc$setDashes(c(8, 4))
	px00 <- px0
	px00.prev <- px0
	release_handler <- function(widget, event, env) {
		# mouse button was released
		env$px1 <- list(x=event$x, y=event$y)
		return(TRUE)
	}
	expose_handler <- function(widget, event, env) {
		area <- event$area
		gdkDrawPixbuf(event$window, pixbuf=env$buf, 
			src.x=area$x, src.y=area$y, dest.x=area$x, dest.y=area$y,
			width=area$width, height=area$height)
		xx <- range(c(env$px0$x, env$px00$x))
		yy <- range(c(env$px0$y, env$px00$y))
		wd <- xx[2] - xx[1]
		ht <- yy[2] - yy[1]
		switch(env$shape,
			line=gdkDrawLine(event$window, gc=env$gc, 
				x1=env$px0$x, y1=env$px0$y, 
				x2=env$px00$x, y2=env$px00$y),
			rect=gdkDrawRectangle(event$window, gc=env$gc, filled=FALSE, 
				x=xx[1], yy[1], width=wd, height=ht)
		)
		return(TRUE) # stop event here
	}
	tmpSigE <- gSignalConnect(da, "expose-event", expose_handler, data=environment())
	tmpSigR <- gSignalConnect(da, "button-release-event", release_handler, data=environment())
	repeat {
		if (exists("px1", inherits=FALSE)) break
		px00.prev <- px00
		px00 <- da$window$getPointer()
		if (is.null(px00$retval)) break
		if ((as.flag(px00$mask) & GdkModifierType["button1-mask"]) == 0) {
			# mouse button was released
			px1 <- px00
			break
		}
		xx <- range(c(px0$x, px00$x, px00.prev$x))
		yy <- range(c(px0$y, px00$y, px00.prev$y))
		wd <- xx[2] - xx[1] + 2
		ht <- yy[2] - yy[1] + 2
		da$window$invalidateRect(list(x=xx[1], y=yy[1], width=wd, height=ht),
			invalidate.children=FALSE)
	}
	gSignalHandlerDisconnect(da, tmpSigR)
	gSignalHandlerDisconnect(da, tmpSigE)
	da$window$invalidateRect(invalidate.children=FALSE)
	if (!exists("px1", inherits=FALSE)) return(NULL)
	dc <- list(x=c(px0$x, px1$x), y=c(px0$y, px1$y))
	# was it a click or a drag? (threshold 3 pixels) - TODO: better to use timing
	is.click <- ((abs(diff(dc$x)) <= 3) && (abs(diff(dc$y)) <= 3))
	if (is.click) {
		# coords are +/- 10 pixels
		dc$x <- dc$x[1] + c(-10, 10)
		dc$y <- dc$y[1] + c(-10, 10)
	}
	ndc <- list(x=dc$x / da.w, y=(da.h - dc$y) / da.h)
	list(dc=dc, ndc=ndc, is.click=is.click)
}

xyCoords <- function(playState, space="plot") {
	foo <- xyData(playState, space=space)
	foo$x <- as.numeric(foo$x)
	foo$y <- as.numeric(foo$y)
	foo
}

xyData <- function(playState, space="plot") {
	if (!is.null(playState$data.points)) {
		return(xy.coords_with_class(playState$data.points))
	}
	if (playState$is.lattice) {
		if (space == "page") {
			# data from all packets
			return(do.call(rbind, 
				lapply(playState$trellis$panel.args, as.data.frame)
			))
		}
		if (space == "plot") {
			space <- packet.number()
			if (length(space) == 0) {
				packets <- trellis.currentLayout(which="packet")
				if (sum(packets > 0) > 1) stop("space not well specified")
				space <- packets[packets > 0][1]
			}
			space <- paste("packet", space)
		}
		packet <- as.numeric(sub("packet ", "", space))
		foo <- trellis.panelArgs(playState$trellis, packet.number=packet)
		if (length(foo$x) != length(foo$y)) {
			if ((nx <- length(foo$x)) < (ny <- length(foo$y))) 
				foo$x <- rep(foo$x, length.out = ny)
			else foo$y <- rep(foo$y, length.out = nx)
		}
		return(foo)
	}
	x <- callArg(playState, 1)
	y <- callArg(playState, name="y")
	xy.coords_with_class(x, y)
}

# adapted from grDevices::xy.coords
xy.coords_with_class <- function(x, y=NULL, recycle=TRUE) {
	if (is.null(y)) {
		if (is.language(x)) {
			if (inherits(x, "formula") && length(x) == 3) {
				y <- eval(x[[2]], environment(x), playState$env)
				x <- eval(x[[3]], environment(x), playState$env)
			}
			else stop("invalid first argument")
		}
		else if (inherits(x, "zoo")) {
			y <- if (is.matrix(x)) x[, 1] else x
			x <- stats::time(x)
		}
		else if (inherits(x, "ts")) {
			y <- if (is.matrix(x)) x[, 1] else x
			x <- stats::time(x)
		}
		else if (is.complex(x)) {
			y <- Im(x)
			x <- Re(x)
		}
		else if (is.matrix(x) || is.data.frame(x)) {
			x <- data.matrix(x)
			if (ncol(x) == 1) {
				y <- x[, 1]
				x <- seq_along(y)
			}
			else {
				colnames <- dimnames(x)[[2]]
				y <- x[, 2]
				x <- x[, 1]
			}
		}
		else if (is.list(x)) {
			y <- x[["y"]]
			x <- x[["x"]]
		}
		else {
			if (is.factor(x)) x <- as.numeric(x)
			y <- x
			x <- seq_along(x)
		}
	}
	if (inherits(x, "zoo")) {
		# and y is not null
		x <- as.numeric(x)
		y <- as.numeric(y)
	}
	if (inherits(x, "POSIXt")) x <- as.POSIXct(x)
	if (length(x) != length(y)) {
		if (recycle) {
			if ((nx <- length(x)) < (ny <- length(y))) 
				x <- rep(x, length.out = ny)
			else y <- rep(y, length.out = nx)
		} else stop("'x' and 'y' lengths differ")
	}
	list(x=x, y=y)
}

unlogX <- function(x, the.call, is.lattice=T) {
	unlogXY(x, the.call, is.lattice, x.or.y="x")
}

unlogY <- function(x, the.call, is.lattice=T) {
	unlogXY(x, the.call, is.lattice, x.or.y="y")
}

unlogXY <- function(x, the.call, is.lattice=T, x.or.y=c("x", "y")) {
	x.or.y <- match.arg(x.or.y)
	scalesArg <- the.call$scales
	if (is.lattice) {
		if (!is.null(scalesArg[[x.or.y]]$log)) {
			logBase <- latticeLogBase(scalesArg[[x.or.y]]$log)
			if (!is.null(logBase)) x <- logBase ^ x
		} else {
			logBase <- latticeLogBase(scalesArg$log)
			if (!is.null(logBase)) x <- logBase ^ x
		}
	} else {
		# traditional graphics plot
		if (par(paste(x.or.y, "log", sep=""))) x <- 10 ^ x
	}
	x
}

latticeLogBase <- function(x) {
	x <- eval(x)
	if (is.null(x) || identical(x, FALSE)) return(NULL)
	if (isTRUE(x)) return(10)
	if (identical(x, "e")) return(exp(1))
	x
}

whichSpace <- function(playState, x.device, y.device) {
	for (space in names(playState$deviceToSpace)) {
		spaceFun <- playState$deviceToSpace[[space]]
		xy <- spaceFun(x.device, y.device)
		if (xy$inside) return(space)
	}
	# return "page" if not inside any of the viewports
	return("page")
}

deviceCoordsToSpace <- function(playState, x.device, y.device, space = "plot") {
	if (space == "page") return(list(x=x.device, y=y.device))
	if (playState$is.lattice && (space == "plot")) {
		space <- packet.number()
		if (length(space) == 0) {
			packets <- trellis.currentLayout(which="packet")
			if (sum(packets > 0) > 1) stop("space not well specified")
			space <- packets[packets > 0][1]
		}
		space <- paste("packet", space)
	}
	spaceFun <- playState$deviceToSpace[[space]]
	spaceFun(x.device, y.device)
}

# this is called by playReplot
# makes a function to transform device native x, y (i.e. pixels)
# into plot coordinates (current viewport native coords if is.grid=T)
deviceToUserCoordsFunction <- function(is.grid = TRUE) {
	# get device size in pixels
	vp <- current.vpPath()
	upViewport(0) # go to root viewport
	dpx <- abs(c(diff(convertX(unit(0:1, "npc"), "native", valueOnly=TRUE)),
		diff(convertY(unit(0:1, "npc"), "native", valueOnly=TRUE))))
	# pixels per inch
	dpi <- convertX(unit(1, "inches"), "native", valueOnly=TRUE)
	if (!is.null(vp)) downViewport(vp)
	# device size in inches
	din <- dpx / dpi
	if (is.grid) {
		# using current viewport
		xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
		ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
		plot.in <-    convertX(unit(1, "npc"), "inches", valueOnly=T)
		plot.in[2] <- convertY(unit(1, "npc"), "inches", valueOnly=T)
		transform <- solve(grid::current.transform())
		function(x.px, y.px) {
			n <- max(length(x.px), length(y.px))
			x.px <- rep(x.px, length=n)
			y.px <- rep(y.px, length=n)
			y.px <- dpx[2] - y.px # y scale origin at bottom
			# TODO: handle vectors directly, this loop is a hack
			x_npc <- y_npc <- numeric(0)
			for (i in seq_along(x.px)) {
				pos.ndc <- c(x.px[i], y.px[i]) / dpx
				pos.din <- c(din * pos.ndc, 1)
				pos.in <- (pos.din %*% transform)
				pos.in <- (pos.in / pos.in[3])
				x_npc[i] <- pos.in[1] / plot.in[1]
				y_npc[i] <- pos.in[2] / plot.in[2]
			}
			x <- xlim[1] + x_npc * diff(xlim)
			y <- ylim[1] + y_npc * diff(ylim)
			inside <- ((min(xlim) <= x) & (x <= max(xlim))
				& (min(ylim) <= y) & (y <= max(ylim)))
			# TODO: unlog
			list(x=x, y=y, inside=inside)
		}
	} else {
		xlim <- par("usr")[1:2]
		ylim <- par("usr")[3:4]
		xmar <- (par("mai") + par("omi"))[c(2,4)]
		ymar <- (par("mai") + par("omi"))[c(1,3)]
		plot.px <- par("pin") * dpi
		xmar.px <- xmar * dpi
		ymar.px <- ymar * dpi
		xlog <- par("xlog")
		ylog <- par("ylog")
		function(x.px, y.px) {
			n <- max(length(x.px), length(y.px))
			x.px <- rep(x.px, length=n)
			y.px <- rep(y.px, length=n)
			y.px <- dpx[2] - y.px # y scale origin at bottom
			x_npc <- (x.px - xmar.px[1]) / plot.px[1]
			y_npc <- (y.px - ymar.px[1]) / plot.px[2]
			x <- xlim[1] + x_npc * diff(xlim)
			y <- ylim[1] + y_npc * diff(ylim)
			inside <- ((min(xlim) <= x) & (x <= max(xlim))
				& (min(ylim) <= y) & (y <= max(ylim)))
			# unlog
			if (xlog) x <- 10 ^ x
			if (ylog) y <- 10 ^ y
			list(x=x, y=y, inside=inside)
		}
	}
}

gmessage.error <- function(message, title="Error", icon="error", ...)
	gmessage(message, title=title, icon=icon, ...)

Filters <- matrix(c(
	"R or S files (*.R,*.q,*.ssc,*.S)", "*.R;*.q;*.ssc;*.S",
	"Postscript files (*.ps)",          "*.ps",             
	"Encapsulated Postscript (*.eps)",  "*.eps",            
	"PDF files (*.pdf)",                "*.pdf",            
	"Png files (*.png)",                "*.png",            
	"Jpeg files (*.jpeg,*.jpg)",        "*.jpeg;*.jpg",     
	"Text files (*.txt)",               "*.txt",            
	"R images (*.RData,*.rda)",         "*.RData;*.rda",    
	"Zip files (*.zip)",                "*.zip",            
	"SVG files (*.svg)",                "*.svg",            
	"Windows Metafiles (*.wmf,*.emf)",  "*.wmf;*.emf",      
	"xfig files (*.fig)",               "*.fig",            
	"All files (*.*)",                  "*.*"), ncol=2, byrow=T,
	dimnames=list(c('R','ps','eps','pdf','png','jpeg','txt',
	'RData','zip','svg','wmf','fig','All'),NULL))

get.extension <- function(path)
{
  ## Extract and return the extension part of a filename
  
  parts <- strsplit(path, "\\.")[[1]]
  if (length(parts) > 1)
    last <- parts[length(parts)]
  else
    last <- ""
  last
}

