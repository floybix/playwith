\name{choose.file.save}
\alias{choose.file.save}
\alias{get.extension}
\alias{get.stem}
\title{GTK file save dialog}
\description{
	Like \code{utils::choose.files} but to save a file rather than open one. 
}
\usage{
choose.file.save(default = "", caption = "Save File", 
                 filters = Filters[c("All"), ], index = 0)

get.extension(path)
get.stem(path)
}
\arguments{
  \item{default}{ which filename to show initially. }
  \item{caption}{ the caption on the file selection dialog. }
  \item{filters}{ a matrix of filename filters (see \code{utils::choose.files}). }
  \item{index}{ which row of filters to use by default. }
  \item{path}{ file path (character). }
}
\details{
	\code{choose.file.save} is a wrapper around \code{\link[RGtk2]{gtkFileChooserDialog}}.
	
	\code{get.extension} returns the extension part of a filename;
	\code{get.stem} returns the rest of the \code{\link{basename}}.
	These were written by Graham Williams.
}
\value{
	\code{choose.file.save} returns the chosen file path, or \code{NA} if 'Cancel' was selected.
	
	\code{get.extension} and \code{get.stem} return character.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{  }
\examples{

}

\keyword{ file }
