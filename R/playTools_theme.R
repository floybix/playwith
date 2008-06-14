## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### THEME

toolConstructors$theme <- function(playState)
{
    if (!playState$is.lattice) return(NA)
    myMenu <- gtkMenu()
    myLabel <- gtkMenuItem("Lattice theme:")
    myLabel["sensitive"] <- FALSE
    myMenu$append(myLabel)
    themeItems <- list(
                       whitebg=gtkMenuItem(label="White BG (general)"),
                       standard.screen=gtkMenuItem(label="Dark BG (for screen)"),
                       standard.print=gtkMenuItem(label="Greyscale (for print)")
                       )
    for (x in names(themeItems)) {
        myMenu$append(themeItems[[x]])
        gSignalConnect(themeItems[[x]], "activate", theme_handler,
                       data=list(playState=playState, theme=x))
    }
    widget <- quickTool(playState,
                        label = "Theme",
                        icon = "gtk-select-color",
                        tooltip = "Choose Lattice theme"
                        )
    ## attach the menu
    gSignalConnect(widget, "clicked",
                   function(widget, menu) {
                       menu$popup(button=0, activate.time=gtkGetCurrentEventTime())
                   }, data=myMenu)
    widget
}

theme_handler <- function(widget, user.data)
{
    playState <- user.data$playState
    switch(user.data$theme,
           whitebg=trellis.par.set(theme=col.whitebg()),
           standard.screen=trellis.par.set(theme=standard.theme("X11")),
           standard.print=trellis.par.set(theme=standard.theme("postscript"))
           )
    playReplot(playState)
}
