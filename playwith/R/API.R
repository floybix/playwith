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
	try(playState$win$destroy())
	cleanupStateEnv()
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
	myW <- da$getAllocation()$width
	myH <- da$getAllocation()$height
	da$setSizeRequest(da$getAllocation()$width, da$getAllocation()$height)
	#playState$win$setGeometryHints(da, list(max.width=myW, min.width=myW, 
	#	max.height=myH, min.height=myH))
	#da["window"]$freezeUpdates()
	eval.parent(substitute(expr))
	#da["window"]$thawUpdates()
	#playState$win$setGeometryHints(da, list())
	da$setSizeRequest(-1, -1)
	playState$skip.redraws <- oval
}

playFocus <- function(playState, highlight=TRUE, ...) {
	playDevSet(playState)
	if (sum(trellis.currentLayout() > 0) > 1) 
		playPrompt(playState) <- "First, choose a panel"
	else highlight <- FALSE
	result <- trellis.focus(highlight=highlight, ...)
	if (!any(result > 0)) playPrompt(playState) <- NULL
	result
}

"playPrompt<-" <- function(playState, value) {
	with(playState$widgets, {
		if (is.null(value)) {
			# remove the prompt widget
			topToolbar$show()
			promptBox$hide()
			playThawGUI(playState)
			return()
		}
		if (promptBox["visible"] == FALSE) {
			# create the prompt widget
			mySize <- topToolbar$getAllocation()
			if (mySize$height > 1)
				promptBox["height-request"] <- mySize$height
			promptBox$show()
			topToolbar$hide()
			playFreezeGUI(playState)
		}
		# set the prompt text
		promptLabel$setMarkup(paste('<big><b>', 
			toString(value), '</b></big>'))
	})
	playState
}

rawXLim <- function(playState) {
	rawXYLim(playState)$x
}

rawYLim <- function(playState) {
	rawXYLim(playState)$y
}

rawXYLim <- function(playState) {
	playDevSet(playState)
	if (!is.null(playState$viewport)) {
		# grid graphics plot
		depth <- try(downViewport(playState$viewport))
		if (inherits(depth, "try-error")) {
			stop(paste("Viewport", playState$viewport, "not found"))
		}
		xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
		ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
		upViewport(depth)
	} else if (playState$is.lattice) {
		# lattice plot
		if (!any(panel.number())) {
			okPnl <- which(trellis.currentLayout() > 0, arr=T)[1,]
			trellis.focus("panel", okPnl['col'], okPnl['row'], highlight=F)
			on.exit(trellis.unfocus())
		}
		xlim <- convertX(unit(0:1, "npc"), "native", valueOnly=T)
		ylim <- convertY(unit(0:1, "npc"), "native", valueOnly=T)
	} else {
		# traditional graphics plot
		xlim <- par("usr")[1:2]
		ylim <- par("usr")[3:4]
	}
	list(x=xlim, y=ylim)
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
		if (!any(panel.number())) {
			okPnl <- which(trellis.currentLayout() > 0, arr=T)[1,]
			trellis.focus("panel", okPnl['col'], okPnl['row'], highlight=F)
			on.exit(trellis.unfocus())
		}
		x.panel <- trellis.panelArgs()[[x.or.y]]
		# convert new scale to appropriate date time class if required
		if (inherits(x.panel, "Date") || inherits(x.panel, "POSIXt")) {
			mostattributes(x) <- attributes(x.panel)
		}
		# TODO: class "dates", "times"? do they come through to panelArgs?
		
		# convert new scale to factor levels if required
		if (is.factor(x.panel)) {
			if (is.null(callArg(playState, scales[[x.or.y]]$labels))) {
				callArg(playState, scales[[x.or.y]]$labels) <- levels(x.panel)
				callArg(playState, scales[[x.or.y]]$at) <- 1:nlevels(x.panel)
			}
		}
	}
	if ("numeric" %in% class(x)) x <- signif(x, 4)
	if (x.or.y == "x") callArg(playState, xlim) <- x
	if (x.or.y == "y") callArg(playState, ylim) <- x
}

xRange <- function(playState) {
	xyRange(playState, "x")
}

yRange <- function(playState) {
	xyRange(playState, "y")
}

xyRange <- function(playState, x.or.y=c("x", "y")) {
	x.or.y <- match.arg(x.or.y)
	lim <- NULL
	if (playState$is.lattice) {
		lim <- range(unlist(lapply(playState$trellis$panel.args, 
			function(pargs)
				range(as.numeric(pargs[[x.or.y]]), finite=TRUE)
			)))
	} else {
		lim <- try(range(xy.coords.call(playState$call, 
			playState$env)[[x.or.y]], finite=TRUE))
		#lim <- try(range(xyCoords(playState)[[x.or.y]], finite=TRUE)
		if (inherits(lim, "try-error")) lim <- c(0,0)
	}
	lim
}

xClass <- function(playState) {
	if (playState$is.lattice) {
		foo <- playState$trellis$x.limits
	} else {
		foo <- xyCoords(playState)$x
	}
	if (inherits(foo, "AsIs")) {
		cls <- setdiff(class(foo), "AsIs")
		if (length(cls)) return(cls)
		else return(class(unclass(foo)))
	}
	class(foo)
}

yClass <- function(playState) {
	if (playState$is.lattice) {
		foo <- playState$trellis$y.limits
	} else {
		foo <- xyCoords(playState)$y
	}
	if (inherits(foo, "AsIs")) {
		cls <- setdiff(class(foo), "AsIs")
		if (length(cls)) return(cls)
		else return(class(unclass(foo)))
	}
	class(foo)
}

xyCoords <- function(playState) {
	if (playState$is.lattice) {
		warning("only data limits returned")
		return(list(x=playState$trellis$x.limits,
			y=playState$trellis$y.limits))
	}
	x <- callArg(playState, 1)
	y <- callArg(playState, name="y")
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
		if ((nx <- length(x)) < (ny <- length(y))) 
			x <- rep(x, length.out = ny)
		else y <- rep(y, length.out = nx)
	}
	list(x=x, y=y)
}

untransformXlim <- function(x, the.call, is.lattice=T) {
	untransformXYlim(x, the.call, is.lattice, x.or.y="x")
}

untransformYlim <- function(x, the.call, is.lattice=T) {
	untransformXYlim(x, the.call, is.lattice, x.or.y="y")
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

unlogX <- function(x, the.call, is.lattice=T) {
	unlogXY(x, the.call, is.lattice, x.or.y="x")
}

unlogY <- function(x, the.call, is.lattice=T) {
	unlogXY(x, the.call, is.lattice, x.or.y="y")
}

latticeLogBase <- function(x) {
	x <- eval(x)
	if (is.null(x) || identical(x, FALSE)) return(NULL)
	if (isTRUE(x)) return(10)
	if (identical(x, "e")) return(exp(1))
	x
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

