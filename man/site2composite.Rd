\name{site2composite}
\alias{site2composite}
\title{From site depth to composite depth.}
\description{Translate the depth of a sample in its 'depth' on the composite sequence.}
\usage{
site2composite(CONOP, mbsf, section)
}

\arguments{
  \item{CONOP}{
An object of class \code{CONOP}, typically the output of function \code{\link{read.CONOP}}.
}
  \item{mbsf}{
Vector of depths to translate.
}
  \item{section}{
Abridged name of the section (should correspond to one of the elements of column 2 of SECTFILE).
}
}
\value{Vector of composite depths.}
\author{Johan Renaudie.}


\keyword{misc}
