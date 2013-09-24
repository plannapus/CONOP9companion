\name{CONOPinput-class}
\Rdversion{1.1}
\docType{class}
\alias{CONOPinput-class}
\alias{write,CONOPinput-method}

\title{Class \code{"CONOPinput"}}
\description{
A class that contains all inputs for a CONOP run.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CONOPinput", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Config}:}{Object of class \code{"list"}. It corresponds to the configuration file of the run.}
    \item{\code{Sectfile}:}{Object of class \code{"data.frame"}. It corresponds exactly to the SECTFILE file.}
    \item{\code{Eventfile}:}{Object of class \code{"data.frame"}. It corresponds exactly to the EVENTTFILE file.}
    \item{\code{Levelfile}:}{Object of class \code{"data.frame"}. It is a correspondance table giving the number of each sample in each section, according to its depth (height). It is meant to be the output of function \code{\link{prep.levelfile}}. }
    \item{\code{Loadfile}:}{Object of class \code{"data.frame"}. It corresponds exactly to the LOADFILE file. }
  }
}
\section{Methods}{
  \describe{
    \item{write}{\code{signature(x = "CONOPinput")}: write the intput files to a folder, giving them names according to the configuration file. To run CONOP9, just add a copy of the software in the folder and launch it. }
	 }
}
\author{
Johan Renaudie.
}

\seealso{
\code{\link{prep.CONOP}}
}
\examples{
showClass("CONOPinput")
}
\keyword{classes}
