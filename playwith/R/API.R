## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

## API functions

playDevCur <- function() StateEnv$.current # may be NULL

playDevSet <- function(playState) {
	#if (!(name %in% ls(StateEnv)))
	#	stop(dQuote(name), " is not the one of the current plots: ",
	#		paste(dQuote(ls(StateEnv)), sep=", "))
	StateEnv$.current <- playState
	playState$old.dev <- dev.cur()
	dev.set(playState$dev)
	playState$win$present()
}

playDevOff <- function(playState = playDevCur()) {
	playState$win$destroy()
}

callArg <- function(playState=playDevCur(), x) {
	x <- substitute(x)
	getx <- if (is.numeric(x)) paste("[[", x+1, "]]", sep="")
		else paste("$", deparse(x), sep="")
	expr <- parse(text=paste("playState$call", getx, sep=""))[[1]]
	eval(expr, envir=playState$env, enclos=parent.frame())
	#name <- StateEnv$.current
	#x <- substitute(x)
	#x <- parse(text=paste("call", deparse(x), sep="$"))[[1]]
	# TODO: need parent.frame() too!
	#eval(x, playState, playState$env)
}

"callArg<-" <- function(playState=playDevCur(), x, value) {
	x <- substitute(x)
	getx <- if (is.numeric(x)) paste("[[", x+1, "]]", sep="")
		else paste("$", deparse(x), sep="")
	expr <- parse(text=paste("call", getx, sep=""))[[1]]
	expr <- call("<-", expr, value)
	eval(x, envir=playState, enclos=parent.frame())
	#name <- StateEnv$.current
	#x <- substitute(x)
	#x <- parse(text=paste("call", deparse(x), sep="$"))[[1]]
	#expr <- call("<-", x, value)
	#eval(expr, playState, parent.frame())
}

playFreezeGUI <- function(playState = playDevCur())
	playSetFreezeGUI(playState, TRUE)

playThawGUI <- function(playState = playDevCur())
	playSetFreezeGUI(playState, FALSE)

playSetFreezeGUI <- function(playState = playDevCur(), frozen) {
	with(playState$widgets, {
		topToolbar["sensitive"] <- !frozen
		leftToolbar["sensitive"] <- !frozen
		bottomToolbar["sensitive"] <- !frozen
		rightToolbar["sensitive"] <- !frozen
		callToolbar["sensitive"] <- !frozen
		pageScrollBox["sensitive"] <- !frozen
		timeScrollBox["sensitive"] <- !frozen
	})
	playState$win$getWindow()$setCursor(
		if (frozen) gdkCursorNew("watch") else NULL)
}

blockRedraws <- function(expr, playState = playDevCur()) {
	oval <- playState$skip.redraws
	playState$skip.redraws <- TRUE
	da <- playState$widgets$drawingArea
	da$setSizeRequest(da["allocation"]$width, da["allocation"]$height)
	eval.parent(substitute(expr))
	da$setSizeRequest(-1, -1)
	playState$skip.redraws <- oval
}

playFocus <- function(playState = playDevCur(), highlight=TRUE, ...) {
	if (sum(trellis.currentLayout() > 0) > 1) 
		playPrompt(playState) <- "First, choose a panel"
	else highlight <- FALSE
	result <- trellis.focus(highlight=highlight, ...)
	if (!any(result > 0)) playPrompt(playState) <- NULL
	result
}

"playPrompt<-" <- function(playState = playDevCur(), prompt.text) {
	with(playState$widgets, {
		if (is.null(prompt.text)) {
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
			toString(prompt.text), '</b></big>'))
	})
}

rawXLim <- function(playState = playDevCur()) {
	rawXYLim(playState)$x
}

rawYLim <- function(playState = playDevCur()) {
	rawXYLim(playState)$y
}

rawXYLim <- function(playState = playDevCur()) {
	if (playState$is.lattice) {
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

"rawXLim<-" <- function(playState = playDevCur(), x) {
	setRawXYLim(playState, x, "x")
}

"rawYLim<-" <- function(playState = playDevCur(), x) {
	setRawXYLim(playState, x, "y")
}

setRawXYLim <- function(playState = playDevCur(), x, x.or.y=c("x", "y")) {
	x.or.y <- match.arg(x.or.y)
	# convert back from log scale if required
	x <- unlogXY(x, playState$call, playState$is.lattice, x.or.y=x.or.y)
	if (playState$is.lattice) {
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
			
			if (is.null(playState$call$scales[[x.or.y]]$labels) {
				playCallArg(scales[[x.or.y]]$labels) <- levels(x.panel)
				playCallArg(scales[[x.or.y]]$at) <- 1:nlevels(x.panel)
			}
		}
	}
	if ("numeric" %in% class(x)) x <- signif(x, 4)
	if (x.or.y == "x") playCallArg(xlim) <- x
	if (x.or.y == "y") playCallArg(ylim) <- x
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

errorDialog <- function(..., isMarkup=F) {
	guiMessageDialog(type="error", ..., isMarkup=isMarkup)
}

infoDialog <- function(..., isMarkup=F) {
	guiMessageDialog(type="info", ..., isMarkup=isMarkup)
}

questionDialog <- function(..., isMarkup=F) {
	guiMessageDialog(type="question", ..., isMarkup=isMarkup)
}

guiMessageDialog <- function(type="info", ..., isMarkup=F) {
	myString <- paste(sep='', ...)
	myButtons <- switch(type,
		error="close",
		info="ok",
		question="yes-no"
	)
	dialog <- gtkMessageDialogNew(NULL, NULL, type, myButtons, myString)
	if (isMarkup) {
		dialog$setMarkup(myString)
	}
	result <- dialog$run() # make it modal
	dialog$destroy()
	if (result == GtkResponseType["yes"]) {
		return("yes")
	} else {
		return(invisible(NULL))
	}
}

guiTextInput <- function(text="", title="Text Input", prompt="", oneLiner=F, 
	accepts.tab=T, wrap.mode=c("none", "char", "word", "word_char"), 
	size=c(600, 320), width.chars=-1, focus.on.ok=!oneLiner) {
	
	wrap.mode <- match.arg(wrap.mode)
	# construct dialog
	editBox <- gtkDialog(title=title, NULL, NULL,
		"OK", GtkResponseType["ok"], "Cancel", GtkResponseType["cancel"],
		show = F)
	editBox$setDefaultResponse(GtkResponseType["ok"])
	if (nchar(prompt) > 0) {
		editBox[["vbox"]]$packStart(gtkLabel(prompt), expand=F, pad=2)
	}
	if (oneLiner) {
		editEntry <- gtkEntry()
		editEntry['activates-default'] <- T
		editEntry['text'] <- text
		editEntry['width-chars'] <- width.chars
		editBox[["vbox"]]$packStart(editEntry, pad=10)
	} else {
		editBox$setDefaultSize(size[1], size[2])
		editTV <- gtkTextView()
		setTextviewMonospace(editTV)
		editTV$setWrapMode(GtkWrapMode[wrap.mode])
		editTV$setAcceptsTab(accepts.tab)
		setTextview(editTV, text)
		scroller <- gtkScrolledWindow()
		scroller$add(editTV)
		scroller$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
		editBox[["vbox"]]$packStart(scroller)
	}
	# put focus on the OK button
	if (focus.on.ok) editBox[["actionArea"]]$getChildren()[[2]]$grabFocus()
	result <- editBox$run() # make it modal
	newTxt <- if (oneLiner) editEntry['text'] else getTextviewText(editTV)
	editBox$destroy()
	if (result != GtkResponseType["ok"]) return(invisible(NULL))
	newTxt
}

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

# returns character string, or NA if cancelled
choose.file.save <- function(default="", caption="Save File", filters=Filters[c("All"),], index=0) {
	dialog <- gtkFileChooserDialog(caption, NULL, "save",
		"gtk-cancel", GtkResponseType["cancel"],
		"gtk-save", GtkResponseType["accept"])
	dialog$setCurrentName(default)
	
	if (length(filters)==2) {
		filters <- matrix(filters, nrow=1, ncol=2)
	}
	
	for (i in seq(1, nrow(filters))) {
		ff <- gtkFileFilterNew()
		ff$setName(filters[i,1])
		for (x in strsplit(filters[i,2], ';')[[1]]) {
			ff$addPattern(x)
		}
		dialog$addFilter(ff)
		if (i == index) dialog$setFilter(ff)
	}
	
	#dialog$setDoOverwriteConfirmation(T) crap, appears behind filechooser
	if (dialog$run() == GtkResponseType["accept"]) {
		filename <- dialog$getFilename()
		if (file.exists(filename)) {
			if (is.null(questionDialog("Replace existing file?"))) {
				filename <- NA
			}
		}
		dialog$destroy()
		return(filename)
	} else {
		dialog$destroy()
		return(NA)
	}
}

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

