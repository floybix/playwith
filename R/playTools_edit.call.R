## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### EDIT.CALL

edit.call.inline_handler <- function(widget, playState)
{
    ## the original call -- this should match the code in playReplot!
    callTxt <- deparseOneLine(playState$call, control="showAttributes")
    newTxt <- widget["text"]
    if (identical(newTxt, callTxt)) return()
    if (identical(newTxt, "")) return()
    tmp <- tryCatch(parse(text=newTxt), error=function(e)e)
    ## check whether there was a syntax error
    if (inherits(tmp, "error")) {
        gmessage.error(conditionMessage(tmp))
    } else {
        ## if more than one call, wrap them in braces
        playState$call <- if (length(tmp) > 1)
            as.call(c(as.symbol("{"), tmp)) else tmp[[1]]
        playNewPlot(playState)
    }
    playState$win$present()
}

edit.call_handler <- function(widget, playState)
{
    theCall <- playState$call
    callTxt <- paste(deparse(theCall, control="showAttributes"), collapse="\n")
    repeat {
        newTxt <- guiTextInput(callTxt, title="Edit plot call",
                               prompt="", accepts.tab=F)
        ## possible with gWidgets, but way too slow.
        #txtBox <- gtext(callTxt, font.attr=c(family="monospace"), wrap=FALSE, width=600)
        #gbasicdialog(title="Edit plot call", widget=txtBox,
        #             action=environment(), handler=function(h, ...)
        #             assign("newTxt", svalue(h[[1]]), env=h$action)
        #             )
        if (is.null(newTxt)) break
        callTxt <- newTxt
        tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
        ## check whether there was a syntax error
        if (inherits(tmp, "error")) {
            gmessage.error(conditionMessage(tmp))
        } else {
            ## if more than one call, wrap them in braces
            playState$call <- if (length(tmp) > 1)
                as.call(c(as.symbol("{"), tmp)) else tmp[[1]]
            playNewPlot(playState)
            break
        }
    }
    playState$win$present()
}
