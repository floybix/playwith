## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### PRINT

toolConstructors$print <- function(playState)
{
    quickTool(playState,
              label = "Print",
              icon = "gtk-print",
              f = print_handler)
}

print_handler <- function(widget, playState)
{
    playDevSet(playState)
    ## TODO: print at current size, otherwise figure annotations might move!
    isWindows <- (.Platform$OS.type == "windows")
    if (isWindows) dev.print(win.print)
    else dev.print()
}

