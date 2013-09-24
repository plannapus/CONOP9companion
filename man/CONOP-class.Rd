\name{CONOP-class}
\Rdversion{1.1}
\docType{class}
\alias{CONOP-class}
\alias{print,CONOP-method}

\title{Class \code{"CONOP"}}
\description{
A class that contains all output from a CONOP run.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CONOP", ...)}.

}
\section{Slots}{
  \describe{
    \item{\code{Summary}:}{Object of class \code{"vector"}. It contains information on the CONOP run such as the name of the run, the misfit of the best solution and some metrics to estimate the quality of the run. }
    \item{\code{Config}:}{Object of class \code{"list"}. It contains the configuration file of the run. }
    \item{\code{Sectfile}:}{Object of class \code{"data.frame"}. It corresponds exactly to the SECTFILE file.}
    \item{\code{Eventfile}:}{Object of class \code{"data.frame"}. It corresponds exactly to the EVENTFILE file.}
    \item{\code{Obsdfile}:}{Object of class \code{"matrix"}. It corresponds exactly to the OBSDFILE file.}
    \item{\code{Plcdfile}:}{Object of class \code{"matrix"}. It corresponds exactly to the PLCDFILE file.}
    \item{\code{Composfile}:}{Object of class \code{"matrix"}. It corresponds exactly to the COMPOSFILE file.}
    \item{\code{Extnfile}:}{Object of class \code{"matrix"}. It corresponds exactly to the EXTNFILE file.}
    \item{\code{Events}:}{Object of class \code{"matrix"}. It contains, for each event, its misfit, weighted misfit and the number of sections in which it is observed. }
    \item{\code{Sections}:}{Object of class \code{"list"}.It is a list of events with their depth (observed and placed) and their misfit for each separate section studied. }
    \item{\code{Curvfile}:}{Object of class \code{"matrix"}. It corresponds exactly to the CURVFILE file.}
  }
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "CONOP")}: print a summary of what contains the CONOP object.}
	 }
}

\author{
Johan Renaudie.
}
\note{
Objects of class CONOP are meant to be created by function \code{\link{read.CONOP}}.
}

\seealso{
\code{\link{read.CONOP}}
}
\examples{
showClass("CONOP")
}
\keyword{classes}
