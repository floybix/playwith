\name{quickTool}
\alias{quickTool}
\title{Create new playwith tools}
\description{
  Part of the \code{\link{playwith}} Application Programming Interface.
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
  \item{playState}{ a \code{playState} object, as passed in to the constructor function. }
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
  This is just a convenient way to make a \code{\link[RGtk2]{gtkToolButton}}. 
  The \code{post.plot.action} is attached to the returned object as attribute
  "post.plot.action".
}
\value{
	\code{quickTool} returns a \code{\link[RGtk2]{gtkToolButton}}.
	Any \code{post.plot.action} is set as an attribute "post.plot.action".
}
\author{ Felix Andrews \email{felix@nfrac.org}  }
\seealso{ \code{\link{playwith.API}}, \code{\link{playwith}} }
\examples{
##
## see the examples in help(playwith.API).
##
}
\keyword{ iplot }
\keyword{ programming }
\keyword{ utilities }

