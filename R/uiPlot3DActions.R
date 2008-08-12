## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

plot3DActionGroup <- function(playState)
{
    entries <-
        list( ## : name, stock icon, label, accelerator, tooltip, callback
             list("ZoomIn3D", "gtk-zoom-in", "Zoom in", NULL, NULL, zoomin3d_handler),
             list("ZoomOut3D", "gtk-zoom-out", "Zoom out", NULL, NULL, zoomout3d_handler),
             list("FlyLeft3D", "gtk-media-rewind-ltr", "Fly left", NULL, NULL, flyleft3d_handler),
             list("FlyRight3D", "gtk-media-forward-ltr", "Fly right", NULL, NULL, flyright3d_handler)
             )

    ## construct action group with playState passed to callbacks
    aGroup <- gtkActionGroupNew("Plot3DActions")
    aGroup$addActions(entries, playState)
    aGroup
}


