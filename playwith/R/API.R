## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

## API functions

playDevCur <- function() StateEnv$.current # may be NULL

playDevSet <- function(playState) {
	StateEnv$.current <- playState
	playState$old.dev <- dev.cur()
	dev.set(playState$dev)
	playState$win$present()
}

playDevOff <- function(playState = playDevCur()) {
	playState$win$destroy()
}

callArg <- function(playState, arg, name=NULL) {
	if (missing(arg) && missing(name)) stop("give 'arg' or 'name'")
	arg <- if (missing(arg)) as.symbol(name) else substitute(arg)
	getx <- if (is.numeric(arg)) paste("[[", arg+1, "]]", sep="")
		else if (is.symbol(arg)) paste('[["', arg, '", exact=TRUE]]', sep="")
		else paste("$", deparse(arg), sep="")
	expr <- eval(parse(text=paste("playState$call", getx, sep="")))
	if (mode(expr) == "expression") return(expr)
	eval(expr, envir=playState$env, enclos=parent.frame())
}

"callArg<-" <- function(playState, arg, name=NULL, value) {
	if (missing(arg) && missing(name)) stop("give 'arg' or 'name'")
	x <- if (missing(arg)) as.symbol(name) else substitute(arg)
	getx <- if (is.numeric(arg)) paste("[[", arg+1, "]]", sep="")
		else paste("$", deparse(x), sep="")
	expr <- parse(text=paste("playState$call", getx, sep=""))[[1]]
	expr <- call("<-", expr, value)
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
	if (!is.null(playState$vp)) {
		# grid graphics plot
		depth <- try(downViewport(playState$vp))
		if (inherits(depth, "try-error")) {
			stop(paste("Viewport", playState$vp, "not found"))
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
			#offset <- 0
			#limTerm <- the.call[[paste(x.or.y, "lim", sep="")]]
			#if (is.character(sublevels <- try(eval(limTerm)))) {
			#	offset <- 1 - match(sublevels[1], levels(x.panel))
			#}
			#newlevels.i <- pmax(0, -offset + seq(ceiling(min(x)), max(x)))
			#x <- levels(x.panel)[newlevels.i]
			
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

