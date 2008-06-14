## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### OPTIONS

toolConstructors$options <- function(playState)
{
    myButton <- gtkButton("Options...")
    myMenu <- gtkMenu()

    ## OPTIONS: set label style
    labelStyleItem <- gtkMenuItem("Set label style...")
    myMenu$append(labelStyleItem)
    gSignalConnect(labelStyleItem, "activate", set.label.style_handler,
                   data=playState)
    myMenu$append(gtkSeparatorMenuItem())

    ## OPTIONS: annotation mode
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
    annotationModeHandler <- function(widget, user.data)
    {
        if (widget["active"] == FALSE) return()
        playState <- user.data$playState
        playState$annotation.mode <- user.data$mode
        ## hack because gtkRadioMenuItem is broken
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

    ## OPTIONS: clip annotations
    clipItem <- gtkCheckMenuItem("Clip annotations")
    clipItem["active"] <- !identical(playState$clip.annotations, FALSE)
    myMenu$append(clipItem)
    gSignalConnect(clipItem, "activate", function(widget, playState)
                   playState$clip.annotations <- widget["active"], data=playState)
    myMenu$append(gtkSeparatorMenuItem())

    ## OPTIONS: toolbar style
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
    toolbarStyleHandler <- function(widget, user.data)
    {
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
        ## hack because gtkRadioMenuItem is broken
        for (x in styleItems) {
            if (!(x == widget)) x["active"] <- FALSE
        }
    }
    for (x in names(styleItems)) {
        myMenu$append(styleItems[[x]])
        gSignalConnect(styleItems[[x]], "activate", toolbarStyleHandler,
                       data=list(playState=playState, style=x))
    }

    ## attach the menu
    gSignalConnect(myButton, "button_press_event",
                   function(widget, event, menu) {
                       menu$popup(button=event[["button"]], activate.time=event[["time"]])
                   }, data=myMenu)
    foo <- gtkToolItem()
    foo$add(myButton)
    foo
}

set.label.style_handler <- function(widget, playState)
{
    style <- playState$label.style
    if (is.null(playState$label.style)) {
        style <- do.call(gpar, trellis.par.get("add.text"))
    }
    if (inherits(style, "gpar"))
        style <- as.call(c(quote(gpar), style))

    callTxt <- deparseOneLine(style)

    repeat {
        newTxt <- NA#ginput("Edit label style", text=callTxt, width=60)
        editbox <- gedit(callTxt, width=120)#, container=ggroup())
        gbasicdialog("Edit label style", widget=editbox, action=environment(),
                     handler=function(h, ...) {
                         h$action$newTxt <- svalue(editbox)
                     })
        if (is.na(newTxt)) break
        if (newTxt == "") break
        if (identical(newTxt, callTxt)) break
        callTxt <- newTxt
        tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
        ## check whether there was a syntax error
        if (inherits(tmp, "error")) {
            gmessage.error(conditionMessage(tmp))
        } else {
            playState$label.style <- eval(tmp)
            break
        }
    }
    playState$win$present()
}
