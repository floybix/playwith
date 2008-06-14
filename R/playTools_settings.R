## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

### SETTINGS

toolConstructors$settings <- function(playState)
{
    if (playState$accepts.arguments == FALSE) return(NA)

    quickTool(playState,
              label = "Plot settings",  ## or "Edit plot"?
              icon = "gtk-preferences",
              tooltip = "Change the plot type and settings",
              f = settings_handler)
}
## note: settings_handler can be found in plotSettingsGui.R
