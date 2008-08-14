## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

constructUI <- function(playState)
{
    menuEntries <-
        list(
             list("FileMenu", NULL, "_File"),
             list("ViewMenu", NULL, "_View"),
             list("LabelsMenu", NULL, "_Labels"),
             list("ToolsMenu", NULL, "_Tools"),
             list("DataMenu", NULL, "_Data"),
             list("ThemeMenu", NULL, "The_me"),
             list("HelpMenu", NULL, "_Help")
             )
    menuGroup <- gtkActionGroupNew("Menus")
    menuGroup$addActions(menuEntries)

    manager <- gtkUIManagerNew()
    window <- playState$win
    window$setData("ui-manager", manager)
    manager$insertActionGroup(plotActionGroup(playState), 0)
    manager$insertActionGroup(plot3DActionGroup(playState), 0)
    manager$insertActionGroup(identifyActionGroup(playState), 0)
    manager$insertActionGroup(annotationActionGroup(playState), 0)
    manager$insertActionGroup(globalActionGroup(playState), 0)
    ## user-defined actions:
    uact <- eval(playwith.getOption("custom.actions"))
    if (is.character(uact)) uact <- get(uact)
    if (is.function(uact)) uact <- uact(playState)
    if (is.list(uact)) {
        tmp <- gtkActionGroupNew("CustomActions")
        tmp$addActions(uact)
        uact <- tmp
    }
    if (!is.null(uact))
        manager$insertActionGroup(uact, 1)
    ## the menus themselves
    manager$insertActionGroup(menuGroup, 0)
    window$addAccelGroup(manager$getAccelGroup())
    ## read in structure of menus and toolbars specified in XML
    for (opt in c("ui.menus.xml", "ui.toolbars.xml", "ui.custom.xml")) {
        uifile <- playwith.getOption(opt)
        if (is.character(uifile) && (nchar(uifile) > 0))
            manager$addUiFromFile(uifile)
    }
    manager
}

initActions <- function(playState)
{
    ## TODO: wrap in try() and maybe catch errors
    initClickActions(playState)
    initIdentifyActions(playState)
    ## make dynamic parameter tools
    nm <- paste("/", playwith.getOption("parameters.toolbar"), sep="")
    paramTbar <- playState$uiManager$getWidget(nm)
    horiz <- (paramTbar["orientation"] == GtkOrientation["horizontal"])
    params <- playState$parameters
    for (i in seq_along(params)) {
        parname <- names(params)[i]
        parval <- params[[i]]
        newTool <- try(parameterControlTool(playState, name=parname,
                                            value=parval, horizontal=horiz))
        if (inherits(newTool, "try-error")) next
        paramToolbar$insert(newTool, -1)
    }
}

updateActions <- function(playState)
{
    ## TODO: wrap in try() and maybe catch errors
    updateGlobalActions(playState)
    updateClickActions(playState)
    updatePlotActions(playState)
    updateIdentifyActions(playState)
}
