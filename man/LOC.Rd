\name{LOC}
\alias{printLOC}
\alias{composite.age}
\alias{LOC.tiepoints}

\title{
Line of Correlation
}
\description{
Functions to derive an age model from the composite sequence.
}
\usage{
printLOC(CONOP, events = "observed")
composite.age(CONOP, agemodels, tie.points, age.span, pch, bg)
LOC.tiepoints(CONOP, saveLOC = FALSE)
}

\arguments{
  \item{CONOP}{
An object of class \code{CONOP}, typically the output of function \code{\link{read.CONOP}}.
}
  \item{events}{
A character string. Can be \code{observed}, in which case the events are plotted at the depth (or height) they have been observed, or \code{placed}, in which case they are plotted at their optimal depth (or height).
}
  \item{agemodels}{
A list of known age models with one element for each site (in the order defined in \code{sectfile}). Each element is a Two-column matrix: first column is the age value (in Ma) and the second column is the depth (in mbsf). The user can provide existing age models for any number of sites that he want (max is the number of site included in \code{sectfile}). Sites that are included in \code{sectfile} but for which no age models is given needs to be \code{NA}. E. g. two sites are present in \code{sectfile}, the first site is not provided with an age models but the second site have two tie points (10mbsf -> 1Ma and 50mbsf ->3Ma), \code{agemodels} should therefore be as follow: [[1]]NA	[[2]][,1]10 50 [,2]1 3
}
  \item{tie.points}{
Output from function \code{LOC.tiepoints}
}
  \item{age.span}{
Vector of two age values (oldest, youngest) that limit the plots.
}
  \item{pch}{
See \code{\link{par}}.
}
  \item{bg}{
See \code{\link{par}}.
}
  \item{saveLOC}{
If \code{TRUE}, plots and LOC are saved in the working directory.
}
}
\details{
	\itemize{
\item{\code{LOC.tiepoints}: Prompt the user to interactively draw a line of correlation on depth vs composite depth diagram and return the set of tie points thus selected. Press ESC when the line is drawn to move on to the next section.}
\item{\code{printLOC}: Plot and print depth vs composite depth (or age) diagram.}
\item{\code{composite.age}: From a given set of tie points, translate COMPOSFILE from composite depth to numerical age values.}
}
}
\value{
The output of \code{LOC.tiepoints} is a list where each element corresponds to a site (the index is the same as in \code{sectfile}). In each element, an age-depth matrix is stored. The output of code{composite.age} is an object of class \code{CONOP} similar to the input with numerical age value instead of composite depth.
}

\author{
Johan Renaudie.
}

\examples{
	data(riley)
	printLOC(riley, events="placed")
}

\keyword{misc}
