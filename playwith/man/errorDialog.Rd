\name{errorDialog}
\alias{errorDialog}
\alias{infoDialog}
\alias{questionDialog}
\title{Quick GTK message dialogs}
\description{
	Simple wrappers around \code{\link[RGtk2]{gtkMessageDialog}}.
}
\usage{
errorDialog(..., isMarkup=F)
infoDialog(..., isMarkup=F)
questionDialog(..., isMarkup=F)
}
\arguments{
  \item{\dots}{ character object(s), pasted together with no separator. }
  \item{isMarkup}{ whether to format embedded pango markup. }
}
\details{
  
}
\value{
	\code{questionDialog} returns either \code{"yes"} or \code{NULL}.
	The others return \code{NULL}.
}
\author{ Graham Williams }
\seealso{  }
\examples{

}

\keyword{ utilities }
