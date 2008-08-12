## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

annotationActionGroup <- function(playState)
{
    entries <-
        list( ## : name, stock icon, label, accelerator, tooltip, callback
             list("Annotation", "gtk-italic", "Annotate", NULL, "Add custom labels to the plot", annotate_handler),
             list("Arrow", "gtk-connect", "Arrow", NULL, "Add an arrow to the plot", arrow_handler),
             list("UndoAnnotation", "gtk-undo", "Undo ann.", "<Ctrl>Z", "Remove last annotation", undo.annotation_handler),
             list("EditAnnotations", "gtk-edit", "Edit ann.", "<Ctrl><Shift>E", "Edit annotations code", edit.annotations_handler),
             list("Clear", "gtk-clear", "Clear", NULL, "Remove labels and annotations", clear_handler)
             )

    ## construct action group with playState passed to callbacks
    aGroup <- gtkActionGroupNew("AnnotationActions")
    aGroup$addActions(entries, playState)
    aGroup
}
