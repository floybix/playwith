## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### EDIT.ANNOTATIONS

toolConstructors$edit.annotations <- function(playState)
{
    quickTool(playState,
              label = "Edit ann.",
              icon = "gtk-edit",
              tooltip = "Edit annotations (including arrows)",
              f = edit.annotations_handler,
              show = length(playState$annotations) > 0)
}

edit.annotations_handler <- function(widget, playState)
{
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
        newTxt <- guiTextInput(callTxt, title="Edit annotations",
                               prompt="", accepts.tab=F)
        if (is.null(newTxt)) break
        callTxt <- newTxt
        tmp <- tryCatch(parse(text=callTxt), error=function(e)e)
        ## check whether there was a syntax error
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
