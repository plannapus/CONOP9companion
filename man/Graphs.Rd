\name{Graphs}
\alias{draw.ranges}
\alias{culling}
\alias{fence}
\alias{collection.quality}
\alias{Section.vs.Composite}
\alias{sect.range}
\title{
Classical, useful plots from CONOP output.
}
\description{
\code{draw.ranges} plots a range-chart.  \code{culling} shows the full range of positions where an event may be placed in the optimal sequence.  \code{fence} plots a fence diagram.  \code{collection.quality} plots the events and ranges observed in each given samples vs the events or ranges that should be observed in each given samples according to the solution of the optimization.  \code{Section_vs_Composite} create a plot of the composite section with the range occupied by the queried section highlighted.  \code{sect_range} create a plot of taxa ranges on a section, comparing observed range and predicted range.
}
\usage{
draw.ranges(CONOP, order = "FAD", method = "depth", ...)
culling(CONOP, cpcht, FO.col = "blue", LO.col = "red")
fence(CONOP, events = "observed", tag = 1)
collection.quality(CONOP, section)
Section.vs.Composite(CONOP, section, col, cex=1, axes=TRUE)
sect.range(CONOP, section, cex=0.8)
}

\arguments{
  \item{CONOP}{
An object of class \code{CONOP}, typically the output of function \code{\link{read.CONOP}}.
}
  \item{order}{
The rangechart can be ordered by \code{"FAD"} or by \code{"LAD"}.
}
  \item{method}{
If \code{"depth"}, then the rangechart is plotted according to the composite sequence 'depth'. If \code{"age"} it is plotted according to the age of the events.
}
  \item{cpcht}{
The output of function \code{read.cpchrt}.
}
  \item{FO.col}{
Color for FO.
}
  \item{LO.col}{
Color for LO.
}
  \item{col}{
Color of the highlighted area.
}
  \item{events}{
A character string. Can be \code{observed}, in which case the events are plotted at the depth (or height) they have been observed, or \code{placed}, in which case they are plotted at their optimal depth (or height).
}
  \item{tag}{
Two possible values: if \code{1} then each sites will be named after its abridged name, if \code{2} after its complete name (respectively columns 2 and 4 of the input file SECTFILE).
}
  \item{section}{
Section name. Should correspond to the complete name of the section as entered in the SECTFILE.
}
  \item{cex}{
Size factor for the annotations. To be decreased when plotting several sections on the same plot.
}
  \item{axes}{
Logical. TRUE if axes desired, FALSE otherwise. Two x-axes are to be plotted: one corresponding to the composite section, the other labels the extremes of the queried section. If only the composite section depth is desired, use \code{axes=FALSE} and then \code{axis(1,...)} or \code{axis(3,...)} after the call to \code{Section.vs.Composite}.
}
  \item{\dots}{
Any argument that could be passed to \code{segments}.
}
}

\author{
	Johan Renaudie, based on the CONOP9 functionalities created by Peter Sadler.
}

\examples{
	data(riley)
	par(mfcol=c(2,1))
	par(mar=c(0,5,1,1))
	fence(riley,"placed")
	par(mar=c(1,5,0,1))
	fence(riley, "observed")
	
	collection.quality(riley, "Wht")
	
	par(mfrow=c(nrow(riley@Sectfile),1))
	for(i in riley@Sectfile[,4]){
		par(mar=c(1,3,1,1))
		Section.vs.Composite(riley,i,col="lightblue",cex=0.8,axes=FALSE)
		}
	
	sect.range(riley,"Morgan")
}

\keyword{misc}
