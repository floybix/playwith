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
    themeItems <-
        list(
             standard.pdf = gtkMenuItem(label="General default"),
             whitebg = gtkMenuItem(label="White BG"),
             standard.screen = gtkMenuItem(label="Dark BG (for screen)"),
             standard.print = gtkMenuItem(label="Greyscale (for print)"),
             custom.theme.1 = gtkMenuItem(label="ColorBrewer 1")
             )
    theme_handler <- function(widget, theme)
    {
        switch(theme,
               standard.pdf = trellis.par.set(standard.theme("pdf")),
               whitebg = trellis.par.set(col.whitebg()),
               standard.screen = trellis.par.set(standard.theme("X11")),
               standard.print = trellis.par.set(standard.theme("postscript")),
               custom.theme.1 = trellis.par.set(custom.theme())
               )
        playReplot(playState)
    }

    for (x in names(themeItems)) {
        myMenu$append(themeItems[[x]])
        gSignalConnect(themeItems[[x]], "activate", theme_handler,
                       data=x)
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
