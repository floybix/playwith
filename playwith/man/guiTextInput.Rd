\name{guiTextInput}
\alias{guiTextInput}
\title{Allow user to enter and edit text}
\description{
A text field in a dialog box.
}
\usage{
guiTextInput(text = "", title = "Text Input", prompt = "", oneLiner = F, 
             accepts.tab = T, wrap.mode = c("none", "char", "word", "word_char"), 
             size = c(600, 320), width.chars = -1, focus.on.ok = !oneLiner)
}
\arguments{
  \item{text}{ initial text to put in the text box. }
  \item{title}{ the window title. For \code{editAsText} the default is based on \code{x}. }
  \item{prompt}{ a prompt to display (as a \code{gtkLabel} above the text box. }
  \item{oneLiner}{ whether to use a single-line \code{gtkEntry} rather than a \code{gtkTextView}. }
  \item{accepts.tab}{ whether a tab can be typed in the text box; otherwise it changes the focus. }
  \item{wrap.mode}{ type of text wrapping, from \code{GtkWrapMode}. }
  \item{size}{ default window size as \code{c(x, y)} pixels, used when \code{oneLiner=F}. }
  \item{width.chars}{ width of the entry in characters, used when \code{oneLiner=T}. }
  \item{focus.on.ok}{ whether the initial keyboard focus is on the OK button.
  	Otherwise it will be in the text box. }
}
\details{

}
\value{
	\code{guiTextInput} returns the text from the text box.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{edit}} }
\examples{
\dontrun{

guiTextInput()

guiTextInput(oneLiner=T)

}
}

\keyword{ utilities }
\keyword{ manip }
