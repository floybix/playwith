## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

plotActionGroup <- function(playState)
{
    entries <-
        list( ## : name, stock icon, label, accelerator, tooltip, callback
             list("PlotSettings", "gtk-preferences", "Plot _settings", "<Ctrl>I", "Change the plot type and settings", plot.settings_handler),
             list("Zoomfit", "gtk-zoom-fit", "_Fit data", "<Ctrl>space", "Revert to default plot region", zoomfit_handler),
             list("ZeroY", "gtk-goto-bottom", "Full y scale", "<Ctrl>Return", "Show the full y (response) scale starting from zero", zero.y_handler),
             list("ZeroX", "gtk-goto-first", "Full x scale", "<Ctrl>BackSpace", "Show the full x (domain) scale starting from zero", zero.x_handler)
             )

    toggleEntries <-
        list( ## : name, stock icon, label, accelerator, tooltip, callback, active?
             list("Expand", "gtk-fullscreen", "_Panel", NULL, "Choose a panel to expand to fill the figure (for further interaction)", expand_handler, FALSE)
             )

    ## construct action group with playState passed to callbacks
    aGroup <- gtkActionGroupNew("PlotActions")
    aGroup$addActions(entries, playState)
    aGroup$addToggleActions(toggleEntries, playState)
    aGroup
}

## TODO: create PlotSettings dialog once only?
initPlotSettingsDialog <- function(playState) {}

updatePlotActions <- function(playState)
{
    aGroup <- playState$actionGroups[["PlotActions"]]
    hasArgs <- playState$accepts.arguments
    isLatt <- playState$is.lattice
    isSplom <- (playState$callName %in% c("splom"))
    isLatt3D <- isLatt && !is.null(playState$trellis$panel.args.common$scales.3d)
    ## PlotSettings
    aGroup$getAction("PlotSettings")$setVisible(hasArgs)
    ## Zoomfit
    nonFit <- hasArgs && (!is.null(callArg(playState, "xlim")) ||
                          !is.null(callArg(playState, "ylim")))
    if (isLatt3D) nonFit <- (nonFit ||
                             !is.null(callArg(playState, "zlim")) ||
                             !is.null(callArg(playState, "zoom")) ||
                             !is.null(callArg(playState, "screen")) ||
                             !is.null(callArg(playState, "R.mat")))
    aGroup$getAction("Zoomfit")$setVisible(nonFit)
    ## ZeroY
    eps <- .Machine$double.eps * 2
    nonZeroY <- FALSE
    if (hasArgs && !isSplom) {
        ylim <- rawYLim(playState)
        if (isLatt) {
            ylim <- playState$trellis$y.limits
            if (is.list(ylim)) ylim <- ylim[[1]]
            if (is.character(ylim)) ylim <- c(0, 0)
        }
        if (isLatt3D)
            ylim <- playState$trellis$panel.args.common$zlim
        nonZeroY <- (min(ylim) > eps) || (max(ylim) < -eps)
    }
    aGroup$getAction("ZeroY")$setVisible(nonZeroY)
    ## ZeroX
    nonZeroX <- FALSE
    if (hasArgs && !isSplom) {
        xlim <- rawXLim(playState)
        if (isLatt) {
            xlim <- playState$trellis$x.limits
            if (is.list(xlim)) xlim <- xlim[[1]]
            if (is.character(xlim)) xlim <- c(0, 0)
        }
        if (isLatt3D)
            xlim <- playState$trellis$panel.args.common$xlim
        nonZeroX <- (min(xlim) > eps) || (max(xlim) < -eps)
    }
    aGroup$getAction("ZeroX")$setVisible(nonZeroX)
    ## Expand
    hasPanels <- isLatt && (length(trellis.currentLayout()) > 1)
    expandActive <- aGroup$getAction("Expand")$getActive()
    aGroup$getAction("Expand")$setVisible(expandActive || hasPanels)
}

zoomfit_handler <- function(widget, playState)
{
    isLatt <- playState$is.lattice
    isLatt3D <- isLatt && !is.null(playState$trellis$panel.args.common$scales.3d)
    ## update scales
    callArg(playState, "xlim") <- NULL
    callArg(playState, "ylim") <- NULL
    if (isLatt3D) {
        callArg(playState, "zlim") <- NULL
        callArg(playState, "zoom") <- NULL
        callArg(playState, "screen") <- NULL
        callArg(playState, "R.mat") <- NULL
    }
    playReplot(playState)
}

zero.x_handler <- function(widget, playState)
{
    if (playState$is.lattice) {
        is3D <- !is.null(playState$trellis$panel.args.common$scales.3d)
        if (is3D) {
            ## in 3D case, zero both domain variables ("x" and "y")
            xlim <- playState$trellis$panel.args.common$xlim
            ylim <- playState$trellis$panel.args.common$ylim
            xlim[which.min(abs(xlim))] <- 0
            ylim[which.min(abs(ylim))] <- 0
            callArg(playState, "xlim") <- signif(xlim, 7)
            callArg(playState, "ylim") <- signif(ylim, 7)
            playReplot(playState)
            return()
        } else {
            xlim <- playState$trellis$x.limits
            if (is.list(xlim)) {
                ## TODO
            }
        }
    } else {
        xlim <- rawXLim(playState)
    }
    xlim[which.min(abs(xlim))] <- 0
    rawXLim(playState) <- xlim
    playReplot(playState)
}

zero.y_handler <- function(widget, playState)
{
    if (playState$is.lattice) {
        is3D <- !is.null(playState$trellis$panel.args.common$scales.3d)
        if (is3D) {
            ## in 3D case, zero the response variable ("z")
            zlim <- playState$trellis$panel.args.common$zlim
            zlim[which.min(abs(zlim))] <- 0
            callArg(playState, "zlim") <- signif(zlim, 7)
            playReplot(playState)
            return()
        } else {
            ylim <- playState$trellis$y.limits
            if (is.list(ylim)) {
                ## TODO
            }
        }
    } else {
        ylim <- rawYLim(playState)
    }
    ylim[which.min(abs(ylim))] <- 0
    rawYLim(playState) <- ylim
    playReplot(playState)
}

expand_handler <- function(widget, playState)
{
    playDevSet(playState)
    ## check new expanded setting
    if (widget["active"]) {
        playPrompt(playState,
                   paste("Click on a panel to expand;",
                         "Right-click or Esc to cancel."))
        on.exit(playPrompt(playState, NULL))
        newFocus <- trellis.focus()
        if (is.null(newFocus) || all(newFocus == 0)) {
            widget["active"] <- FALSE
            return()
        }
        playState$tmp$old.call.layout <- callArg(playState, "layout")
        callArg(playState, "layout") <- c(0,1,1)
        playState$tmp$old.page <- playState$page
        playState$page <- packet.number()
        playState$tmp$old.pages <- playState$pages
    } else {
        if (is.null(playState$tmp$old.page)) return()
        callArg(playState, "layout") <- playState$tmp$old.call.layout
        playState$page <- playState$tmp$old.page
        playState$pages <- playState$tmp$old.pages
        playState$tmp$old.page <- NULL
    }
    playReplot(playState)
}
