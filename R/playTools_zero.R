## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### ZERO

toolConstructors$zero <- function(playState)
{
    if (playState$accepts.arguments == FALSE) return(NA)
    ## this tool does not currently work with "splom" or 3D plots
    callName <- deparseOneLine(mainCall(playState)[[1]])
    if (callName %in% c("splom", "cloud", "wireframe"))
        return(NA)

    quickTool(playState,
              label = "Full scale",
              icon = "gtk-goto-bottom",
              tooltip = "Show the full scale starting from zero",
              f = zero_handler,
              post.plot.action = zero_postplot_action)
}

zero_handler <- function(widget, playState)
{
    yonly <- playState$time.mode && is.null(playState$time.vector)
    trans.x <- !yonly
    trans.y <- TRUE
    if (trans.x) {
        xlim <- rawXLim(playState)
        if (min(xlim) > 0) {
            xlim[which.min(xlim)] <- 0 - 0.07 * max(abs(xlim))
        } else if (max(xlim) < 0) {
            xlim[which.max(xlim)] <- 0 + 0.07 * max(abs(xlim))
        }
        callArg(playState, "xlim") <- signif(xlim, 4)
    }
    if (trans.y) {
        ylim <- rawYLim(playState)
        if (min(ylim) > 0) {
            ylim[which.min(ylim)] <- 0 - 0.07 * max(abs(ylim))
        } else if (max(ylim) < 0) {
            ylim[which.max(ylim)] <- 0 + 0.07 * max(abs(ylim))
        }
        callArg(playState, "ylim") <- signif(ylim, 4)
    }
    playReplot(playState)
}

zero_postplot_action <- function(widget, playState)
{
    yonly <- playState$time.mode && is.null(playState$time.vector)
    trans.x <- !yonly
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
