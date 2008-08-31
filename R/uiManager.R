## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

constructUIManager <- function(playState)
{
    menuEntries <-
        list(
             list("FileMenu", NULL, "_File"),
             list("ViewMenu", NULL, "_View"),
             list("StyleMenu", NULL, "_Style"),
             list("LabelsMenu", NULL, "_Labels"),
             list("ToolsMenu", NULL, "_Tools"),
             list("DataMenu", NULL, "_Data"),
             list("OptionsMenu", NULL, "_Options"),
             list("HelpMenu", NULL, "_Help")
             )
    menuGroup <- gtkActionGroupNew("Menus")
    menuGroup$addActions(menuEntries)

    manager <- gtkUIManagerNew()
    window <- playState$win
    window$setData("ui-manager", manager)
    manager$insertActionGroup(plotActionGroup(playState), 0)
    manager$insertActionGroup(identifyActionGroup(playState), 0)
    manager$insertActionGroup(annotationActionGroup(playState), 0)
    manager$insertActionGroup(grobActionGroup(playState), 0)
    manager$insertActionGroup(optionsActionGroup(playState), 0)
    manager$insertActionGroup(globalActionGroup(playState), 0)
    ## user-defined actions:
    uact <- eval(playwith.getOption("custom.tools"))
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
    ## add style items
    ## TODO: separate this somehow
    # manager$addUi(manager$newMergeId(), path, name, action = NULL, type, top)
    # type = GtkUIManagerItemType["auto"]
    # gtkActionGroup("foo")
    # manager$insertActionGroup(...)
    styleMenu <- manager$getWidget("/MenuBar/StyleMenu")$getSubmenu()
    styleMenu$append(gtkSeparatorMenuItem())
    set.style_handler <- function(widget, theme) {
        trellis.par.set(eval(theme))
        playReplot(playState)
    }
    ## themes
    themes <- playwith.getOption("themes")
    foo <- gtkMenuItem("Themes:")
    foo["sensitive"] <- FALSE
    styleMenu$append(foo)
    for (nm in names(themes)) {
        item <- gtkMenuItem(nm)
        styleMenu$append(item)
        gSignalConnect(item, "activate", set.style_handler,
                       data = themes[[nm]])
    }
    styleMenu$append(gtkSeparatorMenuItem())
    ## style options
    styleOptions <- playwith.getOption("styleOptions")
    foo <- gtkMenuItem("Style options:")
    foo["sensitive"] <- FALSE
    styleMenu$append(foo)
    for (nm in names(styleOptions)) {
        item <- gtkMenuItem(nm)
        styleMenu$append(item)
        gSignalConnect(item, "activate", set.style_handler,
                       data = styleOptions[[nm]])
    }
    manager
}

initActions <- function(playState)
{
    ## TODO: wrap in try() and maybe catch errors
    playDevSet(playState)
    initClickActions(playState)
    initIdentifyActions(playState)
    initOptionsActions(playState)
    ## custom init actions
    customAct <- c(playwith.getOption("init.actions"),
                   playState$init.actions)
    for (x in customAct) {
        playDevSet(playState)
        if (is.character(x)) x <- get(x)
        if (is.function(x)) x(playState)
        if (is.language(x)) eval(x, playState$env)
    }
}

updateActions <- function(playState)
{
    ## TODO: wrap in try() and maybe catch errors
    playDevSet(playState)
    updateGlobalActions(playState)
    updateClickActions(playState)
    updatePlotActions(playState)
    updateIdentifyActions(playState)
    updateAnnotationActions(playState)
    updateGrobActions(playState)
    updateOptionsActions(playState)
    ## custom update actions
    customAct <- c(playwith.getOption("update.actions"),
                   playState$update.actions)
    for (x in customAct) {
        playDevSet(playState)
        if (is.character(x)) x <- get(x)
        if (is.function(x)) x(playState)
        if (is.language(x)) eval(x, playState$env)
    }
}
