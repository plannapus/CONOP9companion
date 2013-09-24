\name{CONOP9companion-package}
\alias{CONOP9companion-package}
\alias{CONOP9companion}
\docType{package}
\title{
Tools for integrating biostratigraphic software CONOP9 in a statistical workflow
}
\description{
Help preparing input files of CONOP9, read output files of CONOP9 and a few functions to plot and analyze the data.
}
\details{
\tabular{ll}{
Package: \tab CONOP9companion\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-09-24\cr
License: \tab GPL\cr
Depends: \tab methods\cr
}
Two object classes are implemented (\code{\link{CONOPinput-class}} for input files and \code{\link{CONOP-class}} for output files of CONOP9).  Functions \code{\link{lofo}} and \code{\link{pacman}} are used to prepare the data before the CONOP analysis. Function \code{\link{prep.conop}} is used to prepare the input files for CONOP9 while function \code{\link{read.CONOP}} is used to import the result of a CONOP analysis. Several classic plots, already presents in the CONOP9 GUI, are also implemented here, namely \code{\link{draw.ranges}}, \code{\link{sect.ranges}}, \code{\link{fence}}, \code{\link{printLOC}} and \code{\link{collection.quality}}, \code{\link{culling}} and \code{\link{Section.vs.composite}}. Some additional functions are provided to help analysing the composite sequence of events (\code{\link{compact.ordinal}}, \code{\link{composite.age}}, \code{\link{site2composite}}).
}
\author{
Johan Renaudie.

Johan Renaudie <johan.renaudie@mfn-berlin.de> <lecoryphee@googlemail.com>

}
\references{
Sadler, P. M. 2007. CONOP9 version 7.43
}

\keyword{ package }