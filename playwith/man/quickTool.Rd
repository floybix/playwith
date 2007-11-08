\name{quickTool}
\alias{quickTool}
\title{Create new playwith tools}
\description{
  Make new tools for a \code{\link{playwith}} plot window.
}
\usage{
## Note: this is not to be called directly.
quickTool(
	playState,
	label = "", 
	icon.name = NULL, 
	tooltip = NULL, 
	f, 
	data = NULL, 
	post.plot.action = NULL,
	isToggle = F, 
	show = TRUE)
}
\arguments{
  \item{playState}{  }
  \item{label}{ the button label. }
  \item{icon.name}{ name of the GTK icon to use, starting with "gtk-". 
  See \url{http://developer.gnome.org/doc/API/2.0/gtk/gtk-Stock-Items.html\#GTK-STOCK-ABOUT:CAPS} for a list. }
  \item{tooltip}{ the button tooltip. }
  \item{f}{ function to be called when the button is clicked ("callback function"). }
  \item{data}{ extra data to be passed to the callback function. }
  \item{post.plot.action}{  }
  \item{isToggle}{ whether the button should be a \code{gtkToggleToolButton}. }
  \item{show}{  }
}
\details{
  \code{plotAndPlayButtons} and \code{plotAndPlayBasicButtons} are lists of calls to generate \code{\link[RGtk2]{gtkToolItem}}s.
  These are used as buttons in a \code{\link{plotAndPlay}} plot window.
  
  New buttons can be defined: see the \code{Examples} section of this page.
  
  \code{quickTool} is a convenience function to create a \code{\link[RGtk2]{gtkToolButton}} with the given label, icon, tooltip and click handler.
  For example, one element of \code{plotAndPlayButtons} is
  \code{zoomout=quickTool("Zoom out", "gtk-zoom-out", f=.plotAndPlay_zoomout_event)}.
  
}
\value{

}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playwith}} }
\examples{

}
\keyword{ iplot }
\keyword{ programming }
\keyword{ utilities }

