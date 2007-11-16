\name{quickTool}
\alias{quickTool}
\title{Create new playwith tools}
\description{
  Make new tools for a \code{\link{playwith}} plot window.
}
\usage{
## Note: this is only to be called inside a tool constructor function.
quickTool(
	playState,
	label = "", 
	icon.name = NULL, 
	tooltip = NULL, 
	f = NULL, 
	data = NULL, 
	post.plot.action = NULL,
	isToggle = FALSE, 
	show = TRUE)
}
\arguments{
  \item{playState}{  }
  \item{label}{ the button label. }
  \item{icon.name}{ name of the GTK icon to use, starting with "gtk-". 
  See \url{http://developer.gnome.org/doc/API/2.0/gtk/gtk-Stock-Items.html\#GTK-STOCK-ABOUT:CAPS} for a list. }
  \item{tooltip}{ the button tooltip. }
  \item{f}{ function to be called when the button is clicked (button click handler). 
  This should be a \code{function(widget, playState)} if \code{data} is NULL;
  otherwise it should be a \code{function(widget, user.data} and access \code{user.data$playState}.}
  \item{data}{ extra data to be passed to the callback function. 
  If given, this must be a list, and an element \code{"playState"} will be added to it. }
  \item{post.plot.action}{ a function to run after the plot has been drawn.
  This should be a \code{function(widget, playState)}. }
  \item{isToggle}{ whether the button should stay on until it is clicked again (a \code{gtkToggleToolButton}). }
  \item{show}{ sets the initial visibility. }
}
\details{
  
}
\value{
	\code{quickTool} returns a \code{\link[RGtk2]{gtkToolButton}}.
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playwith}} }
\examples{

}
\keyword{ iplot }
\keyword{ programming }
\keyword{ utilities }

